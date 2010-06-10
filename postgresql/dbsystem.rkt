;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         "../generic/interfaces.ss"
         "types.ss")
(provide (all-defined-out))

(define options
  '#hasheq((real-infinities . #t)
           (numeric-infinities . #t)))

(define postgresql-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'postgresql)
    (define/public (get-description) "PostgreSQL")

    (define/public (typeid->type typeid)
      (std:typeoid->type typeid))

    (define/public (typealias->type alias)
      (type-alias->type alias))

    (define/public (get-known-types #:can-read? [can-read? #t]
                                    #:can-write? [can-write? #t])
      (for/list ([type known-types+aliases]
                 #:when (or (not can-read?)
                            (get-type-reader (type-alias->type type)))
                 #:when (or (not can-write?)
                            (get-type-writer (type-alias->type type))))
        type))

    (define/public (get-type-reader type #:options [options null])
      (or (type->type-reader type) values))

    (define/public (get-type-writer type #:options [options null])
      (or (type->type-writer type)
          (lambda (s)
            (unless (string? s)
              (raise-type-error 'default-type-writer "string" s))
            s)))

    (define/public (sql:escape-name name
                                    #:preserve-case? [preserve-case? #f])
      (escape-name preserve-case? name))

    (define/public (sql:literal-expression type value
                                           #:options [options null])
      (literal-expression (if (string? type)
                              type
                              (symbol->string (typealias->type type)))
                          value))

    (define/public (has-support? option)
      (let ([v (hash-ref options option 'notfound)])
        (when (eq? v 'notfound)
          (error 'has-support? "unknown option: ~e" option))
        v))

    (super-new)))

(define dbsystem
  (new postgresql-dbsystem%))