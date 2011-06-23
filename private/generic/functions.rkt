;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base)
         racket/contract
         racket/class
         (rename-in "interfaces.rkt"
                    [virtual-statement make-virtual-statement]))

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

(define (dbsystem-supported-types x)
  (send x get-known-types))

;; == Misc procedures

(define (statement? x)
  (or (string? x)
      (prepared-statement? x)
      (virtual-statement? x)
      (statement-binding? x)))

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

(define (virtual-statement gen)
  (make-virtual-statement (make-weak-hasheq)
                          (if (string? gen) (lambda (_) gen) gen)))

;; == Query procedures

;; query1 : connection symbol Statement Collector -> QueryResult
(define (query1 c fsym stmt collector)
  (send c query fsym stmt collector))

;; query/recordset : connection symbol Statement collector -> void
(define (query/recordset c fsym sql collector)
  (let [(result (query1 c fsym sql collector))]
    (cond [(recordset? result) result]
          [else
           (error fsym "query did not return recordset: ~e" sql)])))

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

(define (mk-N-column-collector fsym N sql)
  (lambda (fields ordered?)
    (when (and N (not (= fields N)))
      (error fsym "query returned ~a ~a (expected ~a): ~e"
             fields (if (= fields 1) "column" "columns") N sql))
    (values null
            (lambda (b av) (cons av b))
            (if ordered? reverse values)
            #f)))

(define (mk-single-column-collector fsym sql)
  ;; The field count check is usually redundant, only needed for string queries
  ;; that don't go through prepare path.
  (lambda (fields ordered?)
    (case fields
      ((0) (error fsym "query returned 0 columns (expected 1): ~e " sql))
      ((1) 'ok)
      (else (error fsym "query returned ~a columns (expected 1): ~e" fields sql)))
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

(define (compose-statement fsym c stmt args checktype)
  (cond [(or (pair? args)
             (prepared-statement? stmt)
             (virtual-statement? stmt))
         (let ([pst
                (cond [(string? stmt)
                       (prepare1 fsym c stmt #t)]
                      [(virtual-statement? stmt)
                       (prepare1 fsym c stmt #f)]
                      [(prepared-statement? stmt)
                       ;; Ownership check done later, by query method.
                       stmt]
                      [(statement-binding? stmt)
                       (error fsym
                              (string-append
                               "cannot execute statement-binding with "
                               "additional inline arguments: ~e")
                              stmt)])])
           (send pst check-results fsym checktype stmt)
           (send pst bind fsym args))]
        [else ;; no args, and sql is either string or statement-binding
         stmt]))


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
  (let ([sql (compose-statement 'query-list c sql args 1)])
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
  (let ([sql (compose-statement 'query-value c sql args 1)])
    (recordset->one-row
     'query-value
     (query/recordset c 'query-value sql
                      (mk-single-column-collector 'query-value sql))
     sql)))

;; query-maybe-value : connection Statement arg ... -> value/#f
;; Expects to get back a recordset of zero or one rows, exactly one column.
(define (query-maybe-value c sql . args)
  (let ([sql (compose-statement 'query-maybe-value c sql args 1)])
    (recordset->maybe-row
     'query-maybe-value
     (query/recordset c 'query-maybe-value sql
                      (mk-single-column-collector 'query-maybe-value sql))
     sql)))

;; query-exec : connection Statement arg ... -> void
(define (query-exec c sql . args)
  (let ([sql (compose-statement 'query-exec c sql args #f)])
    (query1 c 'query-exec sql void-collector)
    (void)))

;; query : connection Statement arg ... -> QueryResult
;; Uses the default 'vectorlist' collector
(define (query c sql . args)
  (let ([sql (compose-statement 'query c sql args #f)])
    (query1 c 'query sql vectorlist-collector/fieldinfo)))

;; -- Functions that don't accept stmt params --

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

;; ========================================

(define (in-query c stmt . args)
  (let ([rows (in-query-helper #f c stmt args)])
    (make-do-sequence
     (lambda ()
       (values (lambda (p) (vector->values (car p)))
               cdr
               rows
               pair?
               (lambda _ #t)
               (lambda _ #t))))))

(define-sequence-syntax in-query*
  (lambda () #'in-query)
  (lambda (stx)
    (syntax-case stx ()
      [[(var ...) (in-query c stmt arg ...)]
       #'[(var ...)
          (:do-in ([(rows) (in-query-helper (length '(var ...)) c stmt (list arg ...))])
                  (void) ;; outer check
                  ([rows rows]) ;; loop inits
                  (pair? rows) ;; pos guard
                  ([(var ...) (vector->values (car rows))]) ;; inner bindings
                  #t ;; pre guard
                  #t ;; post guard
                  ((cdr rows)))]] ;; loop args
      [_ #f])))

(define (in-query-helper vars c stmt args)
  ;; Not protected by contract
  (unless (connection? c)
    (apply raise-type-error 'in-query "connection" 0 c stmt args))
  (unless (statement? stmt)
    (apply raise-type-error 'in-query "statement" 1 c stmt args))
  (let* ([check (or vars 'recordset)]
         [stmt (compose-statement 'in-query c stmt args check)])
    (recordset-rows
     (query/recordset c 'in-query stmt
                      (mk-N-column-collector 'in-query vars stmt)))))

;; ========================================

(define (prepare c stmt)
  (prepare1 'prepare c stmt #f))

;; ----

(define (prepare1 fsym c stmt close-on-exec?)
  (cond [(string? stmt)
         (send c prepare fsym stmt close-on-exec?)]
        [(virtual-statement? stmt)
         (let ([table (virtual-statement-table stmt)]
               [gen (virtual-statement-gen stmt)]
               [cache? (not (is-a? c no-cache-prepare<%>))])
           (let ([table-pst (hash-ref table c #f)])
             (or table-pst
                 (let* ([sql-string (gen (send c get-dbsystem))]
                        [pst (prepare1 fsym c sql-string (not cache?))])
                   (when cache? (hash-set! table c pst))
                   pst))))]))

;; ========================================

(define (start-transaction c #:isolation [isolation #f])
  (send c start-transaction 'start-transaction isolation))

(define (commit-transaction c)
  (send c end-transaction 'commit-transaction 'commit))

(define (rollback-transaction c)
  (send c end-transaction 'rollback-transaction 'rollback))

(define (in-transaction? c)
  (and (send c transaction-status 'in-transaction?) #t))

(define (needs-rollback? c)
  (eq? (send c transaction-status 'needs-rollback?) 'invalid))

(define (call-with-transaction c proc #:isolation [isolation #f])
  (send c start-transaction 'call-with-transaction isolation)
  (begin0 (with-handlers ([(lambda (e) #t)
                           (lambda (e)
                             (send c end-transaction 'call-with-transaction 'rollback)
                             (raise e))])
            (proc))
    (send c end-transaction 'call-with-transaction 'commit)))

;; ========================================

(define (get-schemas c)
  (recordset-rows
   (send c query 'get-schemas
         "select catalog_name, schema_name from information_schema.schemata"
         vectorlist-collector)))

(define (get-tables c)
  (recordset-rows
   (send c query 'get-tables
         "select table_catalog, table_schema, table_name from information_schema.tables"
         vectorlist-collector)))

;; ========================================

(define preparable/c (or/c string? virtual-statement?))

(provide (rename-out [in-query* in-query]))

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
 [dbsystem-supported-types
  (-> dbsystem? (listof symbol?))]

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

 [query-fold
  (-> connection? complete-statement? procedure? any/c any)]

 #|
 [in-query
  (->* (connection? statement?) () #:rest list? sequence?)]
 |#

 [prepare
  (-> connection? preparable/c any)]
 [bind-prepared-statement
  (-> prepared-statement? list? any)]

 [virtual-statement
  (-> (or/c string? (-> dbsystem? string?))
      virtual-statement?)]

 [start-transaction
  (->* (connection?)
       (#:isolation (or/c 'serializable 'repeatable-read 'read-committed 'read-uncommitted #f))
       void?)]
 [commit-transaction
  (-> connection? void?)]
 [rollback-transaction
  (-> connection? void?)]
 [in-transaction?
  (-> connection? boolean?)]
 [needs-rollback?
  (-> connection? boolean?)]
 [call-with-transaction
  (->* (connection? (-> any))
       (#:isolation (or/c 'serializable 'repeatable-read 'read-committed 'read-uncommitted #f))
       void?)]

#|
 [get-schemas
  (-> connection? (listof vector?))]
 [get-tables
  (-> connection? (listof vector?))]
|#)
