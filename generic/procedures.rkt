;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/contract
         racket/class
         "interfaces.rkt")

;; == Administrative procedures

(define (connection? x)
  (and (is-a? x connection:admin<%>)
       (is-a? x connection:query<%>)
       (is-a? x connection:query/prepare<%>)))

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
  (car (query* c fsym (list stmt) collector)))

;; query/recordset : connection symbol Statement collector -> void
(define (query/recordset c fsym sql collector)
  (let [(result (query1 c fsym sql collector))]
    (cond [(Recordset? result) result]
          [else
           (raise-mismatch-error
            fsym
            "query did not return recordset: " sql)])))

;; -fold : connection symbol Statement ('a field ... -> 'a) 'a -> 'a
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

;; vectorlist-collector : collector
(define vectorlist-collector
  (lambda (fields binary?)
    (values null
            (lambda (b . fields) (cons (apply vector fields) b))
            reverse
            #f)))

;; vectorlist-collector : collector
(define vectorlist-collector/fieldinfo
  (lambda (fields binary?)
    (values null
            (lambda (b . fields) (cons (apply vector fields) b))
            reverse
            (standard-info fields))))

;; void-collector : collector
(define void-collector
  (lambda (fields binary?)
    (values #f void void #f)))

(define (mk-folding-collector base f)
  (lambda (fields binary?) (values base f values #f)))

(define (mk-single-column-collector function sql)
  (lambda (fields binary?)
    (unless (= 1 (length fields))
      (raise-mismatch-error function 
                            "query did not return exactly one column: "
                            sql))
    (values null
            (lambda (b a) (cons a b))
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

;; Query API procedures

;; query : connection Statement -> QueryResult
;; Uses the default 'vectorlist' collector
(define (query c sql)
  (query1 c 'query sql vectorlist-collector/fieldinfo))

;; query-multiple : connection (list-of Statement) -> (list-of QueryResult)
(define (query-multiple c stmts)
  (query* c 'query-multiple stmts vectorlist-collector/fieldinfo))

;; query-rows : connection Statement -> (listof (vectorof 'a))
(define (query-rows c sql)
  (Recordset-data
   (query/recordset c 'query-rows sql
                    vectorlist-collector)))

;; query-list : connection Statement -> (listof 'a)
;; Expects to get back a recordset with one field per row.
(define (query-list c sql)
  (Recordset-data
   (query/recordset c 'query-list sql
                    (mk-single-column-collector 'query-list sql))))

;; query-maybe-row : connection Statement -> (vector-of 'a) or #f
;; Expects to get back a recordset of zero or one rows.
(define (query-maybe-row c sql)
  (recordset->maybe-row 
   'query-maybe-row
   (query/recordset c 'query-maybe-row sql vectorlist-collector)
   sql))

;; query-row : connection Statement -> (vector-of 'a)
;; Expects to get back a recordset of zero or one rows.
(define (query-row c sql)
  (recordset->one-row 
   'query-row
   (query/recordset c 'query-row sql vectorlist-collector)
   sql))

;; query-value : connection string -> value | raises error
;; Expects to get back a recordset of exactly one row, exactly one column.
(define (query-value c sql)
  (recordset->one-row
   'query-value
   (query/recordset c 'query-value sql
                    (mk-single-column-collector 'query-value sql))
   sql))

;; query-maybe-value : connection Statement -> value/#f
;; Expects to get back a recordset of zero or one rows, exactly one column.
(define (query-maybe-value c sql)
  (recordset->maybe-row
   'query-maybe-value
   (query/recordset c 'query-maybe-value sql
                    (mk-single-column-collector 'query-maybe-value sql))
   sql))

;; query-exec : connection Statement ... -> void
(define (query-exec . sqls)
  (query* c 'query-exec sqls void-collector)
  (void))

;; query-fold : connection Statement ('a field ... -> 'a) 'a -> 'a
(define (query-fold c sql f base)
  (-fold c 'query-fold sql f base))

;; query-map : connection Statement (field ... -> 'a) -> (listof 'a)
(define (query-map c sql f)
  (reverse
   (-fold c 'query-map sql
          (lambda (b . fields) (cons (apply f fields) b))
          null)))

;; query-mapfilter : connection Statement (field... -> 'a) (field... -> boolean)
;;                -> (listof 'a)
(define (query-mapfilter c sql f keep?)
  (reverse (-fold c 'query-mapfilter sql
                  (lambda (b . fields)
                    (if (apply keep? fields)
                        (cons (apply f fields) b)
                        b))
                  null)))

;; query-for-each : connection Statement (field ... -> unspecified) -> unspecified
(define (query-for-each c sql f)
  (-fold c 'query-for-each sql
         (lambda (_ . fields) (apply f fields))
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
         (lambda args (method (send pst bind args) arg ...))))]))

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

(defprepare prepare-query-exec query-exec)

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
  (-> connection? statement? any)]
 [query-multiple
  (-> connection? (listof statement?) any)]
 [query-exec
  (->* (connection?) () #:rest (listof statement?) any)]
 [query-rows
  (-> connection? statement? any)]
 [query-list
  (-> connection? statement? any)]
 [query-row
  (-> connection? statement? any)]
 [query-maybe-row
  (-> connection? statement? any)]
 [query-value
  (-> connection? statement? any)]
 [query-maybe-value
  (-> connection? statement? any)]
 [query-map
  (-> connection? statement? procedure? any)]
 [query-for-each
  (-> connection? statement? procedure? any)]
 [query-mapfilter
  (-> connection? statement? procedure? procedure? any)]
 [query-fold
  (-> connection? statement? procedure? any/c any)]

 [prepare
  (-> connection? string? any)]
 [prepare-multiple
  (-> connection? (listof string?) any)]
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
 [prepare-query-map
  (-> connection? string? procedure? any)]
 [prepare-query-for-each
  (-> connection? string? procedure? any)]
 [prepare-query-mapfilter
  (-> connection? string? procedure? procedure? any)]
 [prepare-query-fold
  (-> connection? string? procedure? any/c any)])
