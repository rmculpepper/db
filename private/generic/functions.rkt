;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/class
         (rename-in "interfaces.rkt"
                    [statement-generator make-statement-generator]))

;; == Administrative procedures

(define (connection? x)
  (is-a? x connection<%>))

(define (connected? x)
  (send x connected?))

(define (disconnect x)
  (send x disconnect))

(define (connection-dbsystem x)
  (send x get-dbsystem))

(define (dbsystem? x)
  (is-a? x dbsystem<%>))

(define (dbsystem-name x)
  (send x get-short-name))


;; == Misc procedures

(define (statement? x)
  (or (string? x)
      (prepared-statement? x)
      (statement-generator? x)
      (statement-binding? x)))

#|
(define (complete-statement? x)
  (or (string? x)
      (statement-binding? x)))
|#
(define complete-statement?
  (or/c string? statement-binding?))

(define (bind-prepared-statement pst params)
  (send pst bind 'bind-prepared-statement params))

(define (prepared-statement? x)
  (is-a? x prepared-statement<%>))

(define (prepared-statement-parameter-types pst)
  (send pst get-param-types))
(define (prepared-statement-result-types pst)
  (send pst get-result-types))

(define (statement-generator gen)
  (make-statement-generator (make-weak-hasheq)
                            (if (string? gen) (lambda (_) gen) gen)))

;; == Query procedures

;; Query auxiliaries
;; Relies on query* method from primitive-query<%>

;; query1 : connection symbol Statement Collector -> QueryResult
(define (query1 c fsym stmt collector)
  (car (send c query* fsym (list stmt) collector)))

;; query/recordset : connection symbol Statement collector -> void
(define (query/recordset c fsym sql collector)
  (let [(result (query1 c fsym sql collector))]
    (cond [(recordset? result) result]
          [else
           (raise-mismatch-error
            fsym
            "query did not return recordset: " sql)])))

;; -fold : connection symbol Statement ('a fieldv -> 'a) 'a -> 'a
(define (-fold c fsym sql f base)
  (recordset-rows
   (query/recordset c
                    fsym
                    sql
                    (mk-folding-collector base f))))


;; Collectors (see type def in interfaces)

;; vectorlist-collector : collector
(define vectorlist-collector
  (lambda (fields ordered?)
    (values null
            (lambda (b fieldv) (cons fieldv b))
            (if ordered? reverse values)
            #f)))

;; vectorlist-collector : collector
(define vectorlist-collector/fieldinfo
  (lambda (fields ordered?)
    (values null
            (lambda (b fieldv) (cons fieldv b))
            (if ordered? reverse values)
            #t)))

;; void-collector : collector
(define void-collector
  (lambda (fields ordered?)
    (values #f void void #f)))

(define (mk-folding-collector base f)
  (lambda (fields ordered?)
    (if ordered?
        (values base f values #f)
        (values null
                (lambda (b fieldv) (cons fieldv b))
                (lambda (rows)
                  (for/fold ([acc base])
                      ([row (in-list rows)])
                    (f acc row)))
                #f))))

(define (mk-single-column-collector fsym sql)
  ;; The field count check is usually redundant, only needed for string queries
  ;; that don't go through prepare path.
  (lambda (fields ordered?)
    (case fields
      ((0) (error fsym "query returned zero columns: ~e " sql))
      ((1) 'ok)
      (else (error 'fsym "query returned multiple columns: ~e" sql)))
    (values null
            (lambda (b av) (cons (vector-ref av 0) b))
            (if ordered? reverse values)
            #f)))

(define (recordset->one-row fsym rs sql)
  (define rows (recordset-rows rs))
  (cond [(null? rows)
         (raise-mismatch-error fsym "query returned zero rows: " sql)]
        [(null? (cdr rows))
         (car rows)]
        [else
         (raise-mismatch-error fsym "query returned multiple rows: " sql)]))

(define (recordset->maybe-row fsym rs sql)
  (define rows (recordset-rows rs))
  (cond [(null? rows)
         #f]
        [(and (pair? rows) (null? (cdr rows)))
         (car rows)]
        [else
         (raise-mismatch-error fsym "query returned multiple rows: " sql)]))

(define (compose-statement fsym c sql args checktype)
  (cond [(or (pair? args)
             (prepared-statement? sql)
             (statement-generator? sql))
         (let ([pst
                (cond [(string? sql) (prepare c sql)]
                      [(statement-generator? sql)
                       (prepare1 fsym sql)]
                      [(prepared-statement? sql)
                       ;; Ownership check done later, by query* method.
                       sql]
                      [(statement-binding? sql)
                       (error fsym
                              (string-append "expected string or prepared statement "
                                             "for SQL statement with arguments, got ~e")
                              sql)])])
           (case checktype
             ((recordset)
              (check-results fsym pst sql))
             ((column)
              (check-results/one-column fsym pst sql))
             (else (void)))
           (send pst bind fsym args))]
        [else ;; no args, and sql is either string or statement-binding
         sql]))


;; Query API procedures

;; query-rows : connection Statement arg ... -> (listof (vectorof 'a))
(define (query-rows c sql . args)
  (let ([sql (compose-statement 'query-rows c sql args 'recordset)])
    (recordset-rows
     (query/recordset c 'query-rows sql
                      vectorlist-collector))))

;; query-list : connection Statement arg ... -> (listof 'a)
;; Expects to get back a recordset with one field per row.
(define (query-list c sql . args)
  (let ([sql (compose-statement 'query-list c sql args 'column)])
    (recordset-rows
     (query/recordset c 'query-list sql
                      (mk-single-column-collector 'query-list sql)))))

;; query-maybe-row : connection Statement arg ... -> (vector-of 'a) or #f
;; Expects to get back a recordset of zero or one rows.
(define (query-maybe-row c sql . args)
  (let ([sql (compose-statement 'query-maybe-row c sql args 'recordset)])
    (recordset->maybe-row 
     'query-maybe-row
     (query/recordset c 'query-maybe-row sql vectorlist-collector)
     sql)))

;; query-row : connection Statement arg ... -> (vector-of 'a)
;; Expects to get back a recordset of zero or one rows.
(define (query-row c sql . args)
  (let ([sql (compose-statement 'query-row c sql args 'recordset)])
    (recordset->one-row 
     'query-row
     (query/recordset c 'query-row sql vectorlist-collector)
     sql)))

;; query-value : connection string arg ... -> value | raises error
;; Expects to get back a recordset of exactly one row, exactly one column.
(define (query-value c sql . args)
  (let ([sql (compose-statement 'query-value c sql args 'column)])
    (recordset->one-row
     'query-value
     (query/recordset c 'query-value sql
                      (mk-single-column-collector 'query-value sql))
     sql)))

;; query-maybe-value : connection Statement arg ... -> value/#f
;; Expects to get back a recordset of zero or one rows, exactly one column.
(define (query-maybe-value c sql . args)
  (let ([sql (compose-statement 'query-maybe-value c sql args 'column)])
    (recordset->maybe-row
     'query-maybe-value
     (query/recordset c 'query-maybe-value sql
                      (mk-single-column-collector 'query-maybe-value sql))
     sql)))

;; query-exec : connection Statement arg ... -> void
(define (query-exec c sql . args)
  (let ([sql (compose-statement 'query-exec c sql args #f)])
    (send c query* 'query-exec (list sql) void-collector)
    (void)))

;; query : connection Statement arg ... -> QueryResult
;; Uses the default 'vectorlist' collector
(define (query c sql . args)
  (let ([sql (compose-statement 'query c sql args #f)])
    (query1 c 'query sql vectorlist-collector/fieldinfo)))

;; -- Functions that don't accept stmt params --

;; query-multiple : connection (list-of Statement) -> (list-of QueryResult)
(define (query-multiple c stmts)
  (send c query* 'query-multiple stmts vectorlist-collector/fieldinfo))

;; query-exec* : connection Statement ... -> void
(define (query-exec* c . stmts)
  (send c query* 'query-exec* stmts void-collector)
  (void))

;; query-fold : connection Statement ('a field ... -> 'a) 'a -> 'a
(define (query-fold c stmt f base)
  (-fold c 'query-fold stmt
         (lambda (b av)
           (apply f b (vector->list av)))
         base))

;; query-map : connection Statement (field ... -> 'a) -> (listof 'a)
(define (query-map c sql f)
  (reverse
   (-fold c 'query-map sql
          (lambda (b fieldv) (cons (apply f (vector->list fieldv)) b))
          null)))

;; query-mapfilter : connection Statement (field... -> 'a) (field... -> boolean)
;;                -> (listof 'a)
(define (query-mapfilter c sql f keep?)
  (reverse (-fold c 'query-mapfilter sql
                  (lambda (b fieldv)
                    (let ([fields (vector->list fieldv)])
                      (if (apply keep? fields)
                          (cons (apply f fields) b)
                          b)))
                  null)))

;; query-for-each : connection Statement (field ... -> unspecified) -> unspecified
(define (query-for-each c sql f)
  (-fold c 'query-for-each sql
         (lambda (_ fieldv) (apply f (vector->list fieldv)))
         #f)
  (void))


;; == Prepared query procedures

;; Prepare auxiliaries
;; Relies on prepare* method

(define-syntax defprepare
  (syntax-rules ()
    [(defprepare name method)
     (defprepare name method [#:check] [#:arg])]
    [(defprepare name method [#:check check ...])
     (defprepare name method [#:check check ...] [#:arg])]
    [(defprepare name method [#:check check ...] [#:arg arg ...])
     (define (name c sql arg ...)
       (let ([pst (prepare1 'name c sql)])
         (check 'name pst sql) ...
         (lambda args (method c (send pst bind 'name args) arg ...))))]))

(define (check-results name pst stmt)
  (unless (send pst get-result-count)
    (raise-user-error name "query does not return records")))

(define (check-results/one-column name pst stmt)
  (check-results name pst stmt)
  (let ([results (send pst get-result-count)])
    (unless (equal? results 1)
      (raise-user-error name
                        "query does not return a single column (returns ~a columns)"
                        (or results "no")))))

(define (prepare1 fsym c stmt)
  (cond [(string? stmt)
         (car (send c prepare* fsym (list stmt)))]
        [(statement-binding? stmt)
         (let ([table (statement-generator-table stmt)]
               [gen (statement-generator-gen stmt)])
           (let ([table-pst (hash-ref table c #f)])
             (or table-pst
                 (let* ([sql-string (gen (send c get-dbsystem))]
                        [pst (prepare1 fsym c sql-string)])
                   (hash-set! table c pst)
                   pst))))]))

;; Prepared query API procedures

;; FIXME: Currently, to support (prepare c stmt-gen), handle stmts one by one.
;; Should do something smarter.

(define (prepare c stmt)
  (prepare1 'prepare c stmt))

(define (prepare-multiple c stmts)
  (for/list ([stmt (in-list stmts)])
    (prepare1 'prepare-multiple c stmt)))

(defprepare prepare-query-rows query-rows)

(defprepare prepare-query-list query-list
  [#:check check-results/one-column])

(defprepare prepare-query-row query-row
  [#:check check-results])

(defprepare prepare-query-maybe-row query-maybe-row
  [#:check check-results])

(defprepare prepare-query-value query-value
  [#:check check-results/one-column])

(defprepare prepare-query-maybe-value query-maybe-value
  [#:check check-results/one-column])

(defprepare prepare-query-exec query-exec)

(defprepare prepare-query query)

(defprepare prepare-query-map query-map
  [#:check check-results]
  [#:arg proc])

(defprepare prepare-query-for-each query-for-each
  [#:check check-results]
  [#:arg proc])

(defprepare prepare-query-mapfilter query-mapfilter
  [#:check check-results]
  [#:arg map-proc filter-proc])

(defprepare prepare-query-fold query-fold
  [#:check check-results]
  [#:arg combine base])


;; == Contracts

(define preparable/c (or/c string? statement-generator?))

(provide/contract
 [connection?
  (-> any/c any)]
 [disconnect
  (-> connection? any)]
 [connected?
  (-> connection? any)]
 [connection-dbsystem
  (-> connection? dbsystem?)]
 [dbsystem?
  (-> any/c any)]
 [dbsystem-name
  (-> dbsystem? symbol?)]

 [statement?
  (-> any/c any)]
 #|
 [complete-statement?
  (-> any/c any)]
 |#
 [prepared-statement?
  (-> any/c any)]
 [prepared-statement-parameter-types
  (-> prepared-statement? (or/c list? #f))]
 [prepared-statement-result-types
  (-> prepared-statement? (or/c list? #f))]

 [query-exec
  (->* (connection? statement?) () #:rest list? any)]
 [query-rows
  (->* (connection? statement?) () #:rest list? (listof vector?))]
 [query-list
  (->* (connection? statement?) () #:rest list? list?)]
 [query-row
  (->* (connection? statement?) () #:rest list? vector?)]
 [query-maybe-row
  (->* (connection? statement?) () #:rest list? (or/c #f vector?))]
 [query-value
  (->* (connection? statement?) () #:rest list? any)]
 [query-maybe-value
  (->* (connection? statement?) () #:rest list? any)]
 [query
  (->* (connection? statement?) () #:rest list? any)]

 [query-multiple
  (-> connection? (listof complete-statement?) any)]
 [query-exec*
  (->* (connection?) () #:rest (listof complete-statement?) any)]
 [query-fold
  (-> connection? complete-statement? procedure? any/c any)]

 [prepare
  (-> connection? preparable/c any)]
 [prepare-multiple
  (-> connection? (listof preparable/c) any)]
 [bind-prepared-statement
  (-> prepared-statement? list? any)]

 [statement-generator
  (-> (or/c string? (-> dbsystem? string?))
      statement-generator?)]

#| 
 [prepare-query
  (-> connection? preparable/c any)]
 [prepare-query-exec
  (-> connection? preparable/c any)]
 [prepare-query-rows
  (-> connection? preparable/c any)]
 [prepare-query-list
  (-> connection? preparable/c any)]
 [prepare-query-row
  (-> connection? preparable/c any)]
 [prepare-query-maybe-row
  (-> connection? preparable/c any)]
 [prepare-query-value
  (-> connection? preparable/c any)]
 [prepare-query-maybe-value
  (-> connection? preparable/c any)]
 [prepare-query-fold
  (-> connection? preparable/c procedure? any/c any)]
|#)
