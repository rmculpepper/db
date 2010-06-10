;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require scribble/manual
         scribble/eval
         racket/sandbox
         (for-syntax racket/base)
         planet/util)
(require (planet cce/scheme:7/scribble))

(provide (all-defined-out)
         (all-from-out (planet cce/scheme:7/scribble)))

(define (package-version)
  (format "~a.~a" (this-package-version-maj) (this-package-version-min)))

;; ----

(define the-eval (make-base-eval))
(void
 (interaction-eval #:eval the-eval
                   (require racket/class
                            "main.ss")))
(void
 (interaction-eval #:eval the-eval
                   (define connection% (class object% (super-new)))))

(define-syntax-rule (examples/results [example result] ...)
  (examples #:eval the-eval (eval:alts example result) ...))
(define-syntax-rule (my-interaction [example result] ...)
  (interaction #:eval the-eval (eval:alts example result) ...))
