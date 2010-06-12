;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         "interfaces.rkt"
         "sql-data.rkt")

(provide get-type
         primitive-query-mixin)

(define (get-type alist)
  (cond [(assq '*type* alist)
         => cdr]
        [else #f]))

;; primitive-query-mixin : connection:admin<%> -> primitive-query<%>
;; Abstract method 'query*/no-conversion'
(define primitive-query-mixin
  (mixin (connection:admin<%>) (primitive-query<%>)
    (inherit get-dbsystem)
    (super-new)

    ;; query*/no-conversion : symbol (list-of Statement) Collector
    ;;                     -> (list-of QueryResult)
    (define/public (query*/no-conversion fsym stmts collector)
      (error 'query*/no-conversion "unimplemented"))

    ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
    ;; Overridden to automatically use type conversion
    (define/public-final (query* fsym stmts collector)
      (query*/no-conversion fsym stmts
                            (compose-with-converters (get-dbsystem) collector)))

    (define/public (get-type-writers typeids)
      (let* ([sys (get-dbsystem)])
        (for/list ([typeid (in-list typeids)])
          (let* ([type (send sys typeid->type typeid)]
                 [convert (send sys get-type-writer type)])
            (or convert (mk-default-convert type))))))))

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

;; mk-default-convert : Type -> datum -> string
(define ((mk-default-convert type) datum)
  (cond [(string? datum) datum]
        [else
         (raise-user-error 'convert-datum->net-representation
                           "cannot convert to type ~a datum: ~s"
                           type
                           datum)]))
