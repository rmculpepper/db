#lang scheme/base
(require scribble/manual
         scribble/eval
         scheme/sandbox
         (for-syntax scheme/base)
         (for-syntax planet/util))
(require (planet cce/scheme:3:0))

(provide (all-defined-out)
         (all-from-out (planet cce/scheme:3:0)))
;; defmodule/this-package
;; declare-exporting/this-package

(begin-for-syntax
 (define package-owner (this-package-version-owner))
 (define package-name (this-package-version-name))
 (define package-maj (this-package-version-maj))
 (define package-min (this-package-version-min))
 (define package
   `(,package-owner ,package-name ,package-maj ,package-min)))

;; ----

(define the-eval (make-base-eval))
(interaction-eval #:eval the-eval
                  (require scheme/class
                           "main.ss"))
(interaction-eval #:eval the-eval
                  (define connection% (class object% (super-new))))
(define-syntax-rule (examples/results [example result] ...)
  (examples #:eval the-eval (eval:alts example result) ...))
(define-syntax-rule (my-interaction [example result] ...)
  (interaction #:eval the-eval (eval:alts example result) ...))
