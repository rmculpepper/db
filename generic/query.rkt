;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         "interfaces.rkt"
         "sql-data.rkt")

(provide standard-info
         get-field-name
         get-type

         vectorlist-collector
         vectorlist-collector/fieldinfo
         void-collector

         primitive-query-base-mixin
         query-mixin
         prepare-query-mixin)

;; Standard recordset collectors

(define (standard-info field-records)
  (map (lambda (fr) (make-FieldInfo (get-field-name fr)))
       field-records))

(define (get-field-name alist)
  (cond [(assq 'name alist)
         => cdr]
        [else #f]))

(define (get-type alist)
  (cond [(assq '*type* alist)
         => cdr]
        [else #f]))

(define vectorlist-collector
  (lambda (fields binary?)
    (values null
            (lambda (b . fields) (cons (apply vector fields) b))
            reverse
            #f)))

(define vectorlist-collector/fieldinfo
  (lambda (fields binary?)
    (values null
            (lambda (b . fields) (cons (apply vector fields) b))
            reverse
            (standard-info fields))))

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

;; query-mixin
;; Provides high-level query methods in terms of low-level ones
(define query-mixin
  (mixin (primitive-query<%>) (connection:query<%>)
    (inherit query*)
    (super-new)

    ;; query1 : symbol Statement Collector -> QueryResult
    (define/private (query1 fsym stmt collector)
      (car (query* fsym (list stmt) collector)))

    ;; query : Statement -> QueryResult
    ;; Uses the default 'vectorlist' collector
    (define/public-final (query sql)
      (car (query* 'query (list sql) vectorlist-collector/fieldinfo)))

    ;; query-multiple : (list-of Statement) -> (list-of QueryResult)
    (define/public-final (query-multiple stmts)
      (query* 'query-multiple stmts vectorlist-collector/fieldinfo))

    ;; query-fold : Statement ('a field ... -> 'a) 'a -> 'a
    (define/public-final (query-fold sql f base)
      (-fold 'fold sql f base))

    ;; -fold : symbol Statement ('a field ... -> 'a) 'a -> 'a
    (define/private (-fold function sql f base)
      (Recordset-data
       (query/recordset function
                        sql
                        (mk-folding-collector base f))))

    ;; query-rows : Statement -> (listof (vectorof 'a))
    (define/public-final (query-rows sql)
      (Recordset-data
       (query/recordset 'query-rows
                        sql
                        vectorlist-collector)))

    ;; query-list : Statement -> (listof 'a)
    ;; Expects to get back a recordset with one field per row.
    (define/public-final (query-list sql)
      (Recordset-data
       (query/recordset 'query-list
                        sql (mk-single-column-collector 'query-list sql))))

    ;; query-maybe-row : Statement -> (vector-of 'a) or #f
    ;; Expects to get back a recordset of zero or one rows.
    (define/public-final (query-maybe-row sql)
      (recordset->maybe-row 
       'query-maybe-row
       (query/recordset 'query-maybe-row sql vectorlist-collector)
       sql))

    ;; query-row : Statement -> (vector-of 'a)
    ;; Expects to get back a recordset of zero or one rows.
    (define/public-final (query-row sql)
      (recordset->one-row 
       'query-row
       (query/recordset 'query-row sql vectorlist-collector)
       sql))

    ;; query-value : string -> value | raises error
    ;; Expects to get back a recordset of exactly one row, exactly one column.
    (define/public-final (query-value sql)
      (recordset->one-row
       'query-value
       (query/recordset 'query-value
                        sql (mk-single-column-collector 'query-value sql))
       sql))

    ;; query-maybe-value : Statement -> value/#f
    ;; Expects to get back a recordset of zero or one rows, exactly one column.
    (define/public-final (query-maybe-value sql)
      (recordset->maybe-row
       'query-maybe-value
       (query/recordset 
        'query-maybe-value sql
        (mk-single-column-collector 'query-maybe-value sql))
       sql))

    ;; query-exec : Statement ... -> void
    (define/public-final (query-exec . sqls)
      (query* 'query-exec sqls void-collector)
      (void))

    ;; query-mapfilter : Statement (field... -> 'a) (field... -> boolean)
    ;;                -> (listof 'a)
    (define/public-final (query-mapfilter sql f keep?)
      (unless (procedure? keep?)
        (raise-type-error 'query-mapfilter "procedure" keep?))
      (unless (procedure? f)
        (raise-type-error 'query-mapfilter "procedure" f))
      (reverse (-fold 'query-mapfilter
                      sql
                      (lambda (b . fields)
                        (if (apply keep? fields)
                            (cons (apply f fields) b)
                            b))
                      null)))

    ;; query-map : Statement (field ... -> 'a) -> (listof 'a)
    (define/public-final (query-map sql f)
      (unless (procedure? f)
        (raise-type-error 'query-map "procedure" f))
      (reverse
       (-fold 'query-map
              sql (lambda (b . fields) (cons (apply f fields) b)) null)))

    ;; query-for-each : Statement (field ... -> unspecified) -> unspecified
    (define/public-final (query-for-each sql f)
      (unless (procedure? f)
        (raise-type-error 'query-for-each "procedure" f))
      (-fold 'query-for-each sql (lambda (_ . fields) (apply f fields)) #f))

    ;; query/recordset : symbol Statement collector -> void
    (define/private (query/recordset fsym sql collector)
      (let [(result (query1 fsym sql collector))]
        (cond [(Recordset? result) result]
              [else
               (raise-mismatch-error
                fsym
                "query did not return recordset: " sql)])))
    ))

;; prepare-query-mixin
;; Provides closure-producing versions of high-level query methods
(define prepare-query-mixin
  (mixin (primitive-query/prepare<%> connection:query<%>) 
         (connection:query/prepare<%>)
    (inherit query-exec
             query-rows
             query-list
             query-row
             query-maybe-row
             query-value
             query-maybe-value
             query-map
             query-for-each
             query-mapfilter
             query-fold)
    (inherit prepare-multiple
             bind-prepared-statement)
    (super-new)

    ;; prepare : string -> PreparedStatement
    (define/public-final (prepare stmt)
      (car (prepare-multiple (list stmt))))

    (define-syntax prepare-query-method
      (syntax-rules ()
        [(prepare-query-method name method)
         (prepare-query-method name method [#:check])]
        [(prepare-query-method name method [#:check check ...])
         (prepare-query-method name method [#:check check ...] [#:arg])]
        [(prepare-query-method name method [#:check check ...] [#:arg arg ...])
         (define/public (name sql arg ...)
           (let ([pst (prepare sql)])
             (check 'name pst sql) ...
             (lambda args (method (bind-prepared-statement pst args) arg ...))))]))

    (prepare-query-method prepare-query-exec query-exec)
    (prepare-query-method prepare-query-rows query-rows)
    (prepare-query-method prepare-query-list query-list
                          [#:check check-results/one-column])
    (prepare-query-method prepare-query-row query-row
                          [#:check check-results])
    (prepare-query-method prepare-query-maybe-row query-maybe-row
                          [#:check check-results])
    (prepare-query-method prepare-query-value query-value
                          [#:check check-results/one-column])
    (prepare-query-method prepare-query-maybe-value query-maybe-value
                          [#:check check-results/one-column])
    
    (prepare-query-method prepare-query-map query-map
                          [#:check check-results]
                          [#:arg proc])
    (prepare-query-method prepare-query-for-each query-for-each
                          [#:check check-results]
                          [#:arg proc])
    (prepare-query-method prepare-query-mapfilter query-mapfilter
                          [#:check check-results]
                          [#:arg map-proc filter-proc])
    (prepare-query-method prepare-query-fold query-fold
                          [#:check check-results]
                          [#:arg combine base])
    ))

(define (check-results name pst stmt)
  (unless (PreparedStatement-results pst)
    (raise-user-error name "query does not return records")))
(define (check-results/one-column name pst stmt)
  (check-results name pst stmt)
  (unless (equal? (PreparedStatement-results pst) 1)
    (raise-user-error name
                      "query does not return a single column (returns ~a columns)"
                      (PreparedStatement-results pst))))

;; primitive-query-base-mixin
;; Abstract method 'query*/no-conversion'
(define primitive-query-base-mixin
  (mixin (connection:admin<%>) (primitive-query<%>)
    (inherit get-system)
    (super-new)

    ;; query*/no-conversion : symbol (list-of Statement) Collector
    ;;                     -> (list-of QueryResult)
    (define/public (query*/no-conversion fsym stmts collector)
      (error 'query*/no-conversion "unimplemented"))

    ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
    ;; Overridden to automatically use type conversion
    (define/public-final (query* fsym stmts collector)
      (query*/no-conversion fsym stmts
                            (compose-with-converters (get-system) collector)))

    ;; datum->external-representation : TypeID datum -> string
    (define/public (datum->external-representation typeid datum)
      (let* ([sys (get-system)]
             [type (send sys typeid->type typeid)]
             [convert (send sys get-type-writer type)])
        (cond [convert (convert datum)]
              [(string? datum) datum]
              [else
               (raise-user-error
                'datum->external-representation
                "cannot convert datum: ~s" datum)])))))

;; compose-with-converters
;;     : (FieldInfo -> 'a ('a field ... -> 'a) ('a -> 'b))
;;    -> (list-of FieldInfo)
;;    -> 'a ('a field ... -> 'a) ('a -> 'b))
(define (compose-with-converters sys f)
  (lambda (field-infos binary?)
    (let* ([type-functions
            (map (lambda (field-info)
                   (send sys get-type-reader
                         (send sys typeid->type
                               (get-type field-info))))
                 field-infos)]
           [convert
            (lambda (args)
              (map (lambda (convert arg)
                     (if (sql-null? arg) sql-null (convert arg)))
                   type-functions
                   args))])
      (let-values ([(base combine finish info) (f field-infos binary?)])
        (values base 
                (if binary?
                    combine
                    (lambda (b . args) (apply combine b (convert args))))
                finish
                info)))))
