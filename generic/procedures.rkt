;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/contract
         racket/class
         "interfaces.rkt")

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
  (or (string? x) (StatementBinding? x)))

(define (prepared-statement? x)
  (is-a? x prepared-statement<%>))

(define (bind-prepared-statement pst params)
  (send pst bind params))


;; == Query procedures

;; Query auxiliaries
;; Relies on query* method from primitive-query<%>

;; query1 : connection symbol Statement Collector -> QueryResult
(define (query1 c fsym stmt collector)
  (car (send c query* fsym (list stmt) collector)))

;; query/recordset : connection symbol Statement collector -> void
(define (query/recordset c fsym sql collector)
  (let [(result (query1 c fsym sql collector))]
    (cond [(Recordset? result) result]
          [else
           (raise-mismatch-error
            fsym
            "query did not return recordset: " sql)])))

;; -fold : connection symbol Statement ('a fieldv -> 'a) 'a -> 'a
(define (-fold c function sql f base)
  (Recordset-data
   (query/recordset c
                    function
                    sql
                    (mk-folding-collector base f))))

;; standard-info : (listof ???) -> (listof FieldInfo)
(define (standard-info field-records)
  (map (lambda (fr) (make-FieldInfo (get-field-name fr)))
       field-records))

(define (get-field-name alist)
  (cond [(assq 'name alist)
         => cdr]
        [else #f]))

;; collector = ((listof ??) bool)
;;          -> (values _a
;;                     (_a (vectorof field) -> _a)
;;                     (_a -> _a)
;;                     _info

;; vectorlist-collector : collector
(define vectorlist-collector
  (lambda (fields binary?)
    (values null
            (lambda (b fieldv) (cons fieldv b))
            reverse
            #f)))

;; vectorlist-collector : collector
(define vectorlist-collector/fieldinfo
  (lambda (fields binary?)
    (values null
            (lambda (b fieldv) (cons fieldv b))
            reverse
            (standard-info fields))))

;; void-collector : collector
(define void-collector
  (lambda (fields binary?)
    (values #f void void #f)))

(define (mk-folding-collector base f)
  (lambda (fields binary?)
    (values base f values #f)))

(define (mk-single-column-collector function sql)
  (lambda (fields binary?)
    (unless (= 1 (length fields))
      (raise-mismatch-error function 
                            "query did not return exactly one column: "
                            sql))
    (values null
            (lambda (b av) (cons (vector-ref av 0) b))
            reverse
            #f)))

(define (recordset->one-row function rs sql)
  (define rows (Recordset-data rs))
  (cond [(and (pair? rows) (null? (cdr rows)))
         (car rows)]
        [else (raise-mismatch-error 
               function
               "query did not return exactly one row: "
               sql)]))

(define (recordset->maybe-row function rs sql)
  (define rows (Recordset-data rs))
  (cond [(null? rows) #f]
        [(and (pair? rows) (null? (cdr rows)))
         (car rows)]
        [else (raise-mismatch-error 
               function
               "query did not return zero or one rows: "
               sql)]))

(define (compose-statement who c sql args)
  (cond [(null? args)
         sql]
        [else ;; must prepare statement and bind
         (unless (string? sql)
           (error who "expected string for SQL statement with arguments, got ~e" sql))
         (let ([pst (prepare c sql)])
           (case checktype
             ((recordset)
              (check-results who pst sql))
             ((column)
              (check-results/one-column who pst sql))
             (else (void)))
           (send pst bind args))]))

;; Query API procedures

;; query : connection Statement arg ... -> QueryResult
;; Uses the default 'vectorlist' collector
(define (query c sql . args)
  (let ([sql (compose-statement 'query c sql args)])
    (query1 c 'query sql vectorlist-collector/fieldinfo)))

;; query-rows : connection Statement arg ... -> (listof (vectorof 'a))
(define (query-rows c sql . args)
  (let ([sql (compose-statement 'query-rows c sql args)])
    (Recordset-data
     (query/recordset c 'query-rows sql
                      vectorlist-collector))))

;; query-list : connection Statement arg ... -> (listof 'a)
;; Expects to get back a recordset with one field per row.
(define (query-list c sql . args)
  (let ([sql (compose-statement 'query-list c sql args)])
    (Recordset-data
     (query/recordset c 'query-list sql
                      (mk-single-column-collector 'query-list sql)))))

;; query-maybe-row : connection Statement arg ... -> (vector-of 'a) or #f
;; Expects to get back a recordset of zero or one rows.
(define (query-maybe-row c sql . args)
  (let ([sql (compose-statement 'query-maybe-row c sql args)])
    (recordset->maybe-row 
     'query-maybe-row
     (query/recordset c 'query-maybe-row sql vectorlist-collector)
     sql)))

;; query-row : connection Statement arg ... -> (vector-of 'a)
;; Expects to get back a recordset of zero or one rows.
(define (query-row c sql . args)
  (let ([sql (compose-statement 'query-row c sql args)])
    (recordset->one-row 
     'query-row
     (query/recordset c 'query-row sql vectorlist-collector)
     sql)))

;; query-value : connection string arg ... -> value | raises error
;; Expects to get back a recordset of exactly one row, exactly one column.
(define (query-value c sql . args)
  (let ([sql (compose-statement 'query-value c sql args)])
    (recordset->one-row
     'query-value
     (query/recordset c 'query-value sql
                      (mk-single-column-collector 'query-value sql))
     sql)))

;; query-maybe-value : connection Statement arg ... -> value/#f
;; Expects to get back a recordset of zero or one rows, exactly one column.
(define (query-maybe-value c sql . args)
  (let ([sql (compose-statement 'query-maybe-value c sql args)])
    (recordset->maybe-row
     'query-maybe-value
     (query/recordset c 'query-maybe-value sql
                      (mk-single-column-collector 'query-maybe-value sql))
     sql)))

;; query-exec : connection Statement arg ... -> void
(define (query-exec c sql . args)
  (let ([sql (compose-statement 'query-exec c sql args)])
    (send c query* 'query-exec (list sql) void-collector)
    (void)))

;; -- Functions without auto-prep --

;; query-multiple : connection (list-of Statement) -> (list-of QueryResult)
(define (query-multiple c stmts)
  (send c query* 'query-multiple stmts vectorlist-collector/fieldinfo))

;; query-exec* : connection Statement ... -> void
(define (query-exec* c . sqls)
  (send c query* 'query-exec* sqls void-collector)
  (void))

;; query-fold : connection Statement ('a field ... -> 'a) 'a -> 'a
(define (query-fold c sql f base)
  (-fold c 'query-fold sql
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
;; Relies on prepare-multiple method

(define-syntax defprepare
  (syntax-rules ()
    [(defprepare name method)
     (defprepare name method [#:check] [#:arg])]
    [(defprepare name method [#:check check ...])
     (defprepare name method [#:check check ...] [#:arg])]
    [(defprepare name method [#:check check ...] [#:arg arg ...])
     (define (name c sql arg ...)
       (let ([pst (prepare c sql)])
         (check 'name pst sql) ...
         (lambda args (method c (send pst bind args) arg ...))))]))

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

;; Prepared query API procedures

(define (prepare c stmt)
  (car (prepare-multiple c (list stmt))))

(define (prepare-multiple c stmts)
  (send c prepare-multiple stmts))

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
 [prepared-statement?
  (-> any/c any)]
 [bind-prepared-statement
  (-> prepared-statement? list? any)]

 [query
  (->* (connection? statement?) () #:rest list? QueryResult?)]
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

 [query-exec*
  (->* (connection?) () #:rest (listof statement?) any)]
 [query-fold
  (-> connection? statement? procedure? any/c any)]

 #|
 [query-multiple
  (-> connection? (listof statement?) (listof QueryResult?))]
 [query-map
  (-> connection? statement? procedure? list?)]
 [query-for-each
  (-> connection? statement? procedure? any)]
 [query-mapfilter
  (-> connection? statement? procedure? procedure? list?)]
 |#
 
 [prepare
  (-> connection? string? any)]
 #|
 [prepare-multiple
  (-> connection? (listof string?) any)]
 |#
 [prepare-query-exec
  (-> connection? string? any)]
 [prepare-query-rows
  (-> connection? string? any)]
 [prepare-query-list
  (-> connection? string? any)]
 [prepare-query-row
  (-> connection? string? any)]
 [prepare-query-maybe-row
  (-> connection? string? any)]
 [prepare-query-value
  (-> connection? string? any)]
 [prepare-query-maybe-value
  (-> connection? string? any)]
 #|
 [prepare-query-map
  (-> connection? string? procedure? any)]
 [prepare-query-for-each
  (-> connection? string? procedure? any)]
 [prepare-query-mapfilter
  (-> connection? string? procedure? procedure? any)]
 |#
 [prepare-query-fold
  (-> connection? string? procedure? any/c any)])
