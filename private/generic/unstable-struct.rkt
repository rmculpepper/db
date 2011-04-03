;; Copyright 2007-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

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
