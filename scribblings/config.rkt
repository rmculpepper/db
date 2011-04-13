;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base "../private/generic/unstable-syntax.rkt")
         scribble/manual
         scribble/eval
         racket/sandbox
         (for-label racket/base
                    racket/contract
                    (planet ryanc/db:1:0)
                    (planet ryanc/db:1:0/util/connect)))
(provide (all-defined-out)
         (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)
                    (all-from-out (planet ryanc/db:1:0))
                    (all-from-out (planet ryanc/db:1:0/util/connect))))

(define (my-package-version) "1.0")
(define (my-require-form) (racket (require #,(racketmodname (planet ryanc/db:1:0)))))

(define-syntax-rule (defmy name underlying)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(name)
       #'(underlying (planet ryanc/db:1:0))]
      [(name id)
       (identifier? #'id)
       (with-syntax ([mod (format-id #'id "ryanc/db:1:0/~a" #'id)])
         #'(underlying (planet mod)))])))

(defmy my-defmodule defmodule)
(defmy my-defmodule/nd defmodule/nd)
(defmy my-declare-exporting declare-exporting)
(defmy my-racketmodname racketmodname)

(define-syntax-rule (defmodule/nd mod)
  (defmodule*/no-declare (mod)))

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
