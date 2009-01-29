;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/class
         "interfaces.ss"
         "sql-data.ss")

(provide standard-info
         get-field-name
         get-type
         
         vectorlist-collector
         vectorlist-collector/fieldinfo
         void-collector
         
         query-mixin
         prepare-query-mixin
         conversion-mixin)

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
    
    ;; fold : Statement ('a field ... -> 'a) 'a -> 'a
    (define/public-final (fold sql f base)
      (-fold 'fold sql f base))
    
    ;; -fold : symbol Statement ('a field ... -> 'a) 'a -> 'a
    (define/private (-fold function sql f base)
      (Recordset-data
       (query/recordset function
                        sql
                        (mk-folding-collector base f))))
    
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
    
    ;; exec : Statement ... -> void
    (define/public-final (exec . sqls)
      (query* 'exec sqls void-collector)
      (void))
    
    ;; mapfilter : Statement (field... -> 'a) (field... -> boolean) -> (listof 'a)
    (define/public-final (mapfilter sql f keep?)
      (unless (procedure? keep?)
        (raise-type-error 'mapfilter "procedure" keep?))
      (unless (procedure? f)
        (raise-type-error 'mapfilter "procedure" f))
      (reverse (-fold 'mapfilter
                      sql
                      (lambda (b . fields)
                        (if (apply keep? fields)
                            (cons (apply f fields) b)
                            b))
                      null)))
    
    ;; -map : Statement (field ... -> 'a) -> (listof 'a)
    (public (-map map))
    (define (-map sql f)
      (unless (procedure? f)
        (raise-type-error 'map "procedure" f))
      (reverse
       (-fold 'map
              sql (lambda (b . fields) (cons (apply f fields) b)) null)))
    
    ;; -for-each : Statement (field ... -> unspecified) -> unspecified
    (public (-for-each for-each))
    (define (-for-each sql f)
      (unless (procedure? f)
        (raise-type-error 'for-each "procedure" f))
      (-fold 'for-each sql (lambda (_ . fields) (apply f fields)) #f))
    
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
    (inherit exec
             query-list
             query-row
             query-maybe-row
             query-value
             query-maybe-value
             map
             for-each
             mapfilter
             fold)
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
    
    (prepare-query-method prepare-exec exec)
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
    
    (prepare-query-method prepare-map map
                          [#:check check-results]
                          [#:arg proc])
    (prepare-query-method prepare-for-each for-each
                          [#:check check-results]
                          [#:arg proc])
    (prepare-query-method prepare-mapfilter mapfilter
                          [#:check check-results]
                          [#:arg map-proc filter-proc])
    (prepare-query-method prepare-fold fold
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

;; conversion-mixin
;; Adds automatic conversions from SQL external representations to Scheme data
(define conversion-mixin
  (mixin (primitive-query<%>) (primitive-query/conversion<%>)
    (super-new)
    
    ;; query*/no-conversion : symbol (list-of Statement) Collector
    ;;                     -> (list-of QueryResult)
    (define/public-final (query*/no-conversion fsym stmts collector)
      (super query* fsym stmts collector))
    
    ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
    ;; Overridden to automatically use type conversion
    (define/override (query* fsym stmts collector)
      (super query* fsym stmts (compose-with-converters collector)))
    
    ;; compose-with-converters : (FieldInfo -> 'a ('a field ... -> 'a) ('a -> 'b))
    ;;                        -> (list-of FieldInfo)
    ;;                        -> 'a ('a field ... -> 'a) ('a -> 'b))
    (define/private (compose-with-converters f)
      (lambda (field-infos binary?)
        (let* ([type-functions
                (map (lambda (field-info)
                       (get-type-reader
                        (typeid->type
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
    
    ;; typeid->type : TypeID -> symbol
    (define/public (typeid->type typeid)
      #f)
    
    ;; get-type-reader : symbol -> (string -> datum)
    (define/public (get-type-reader key)
      values)
    
    ;; get-type-writer : symbol -> (datum -> string) or #f
    (define/public (get-type-writer key)
      #f)
    
    ;; datum->external-representation : TypeID datum -> string
    (define/override (datum->external-representation typeid datum)
      (let ([convert (get-type-writer (typeid->type typeid))])
        (cond [convert
               (convert datum)]
              [(string? datum)
               datum]
              [else
               (raise-user-error
                'datum->external-representation
                "cannot convert datum: ~s" datum)])))))
