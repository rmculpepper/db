;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require scribble/manual
         scribble/eval
         racket/sandbox
         (for-label racket/base
                    racket/contract
                    (planet ryanc/db:1:0)))
(provide (all-defined-out)
         (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)
                    (all-from-out (planet ryanc/db:1:0))))

(define-syntax-rule (my-defmodule)
  (defmodule (planet ryanc/db:1:0)))
(define-syntax-rule (my-declare-exporting)
  (declare-exporting (planet ryanc/db:1:0)))

(define (my-package-version) "1.0")
(define (my-require-form) (racket (require #,(racketmodname (planet ryanc/db:1:0)))))

;; ----

(define the-eval (make-base-eval))
(void
 (interaction-eval #:eval the-eval
                   (require racket/class
                            "main.rkt")))
(void
 (interaction-eval #:eval the-eval
                   (define connection% (class object% (super-new)))))

(define-syntax-rule (examples/results [example result] ...)
  (examples #:eval the-eval (eval:alts example result) ...))
(define-syntax-rule (my-interaction [example result] ...)
  (interaction #:eval the-eval (eval:alts example result) ...))
