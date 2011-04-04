;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "unstable-syntax.rkt"))
(provide define-struct-property)

;; syntax define-struct-property
;; Example:
;;   (define-struct-property prop:fooable)
(define-syntax (define-struct-property stx)
  (syntax-parse stx
    [(_ name (~and (~optional _) (~seq stuff ...)))
     (and (identifier? #'name))
     (with-syntax ([predicate
                    (format-id #'name "~a?" #'name)]
                   [accessor
                    (format-id #'name "~a-value" #'name)])
       #'(define-values (name predicate accessor)
           (make-struct-type-property 'name stuff ...)))]))
