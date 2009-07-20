#lang scheme/base
(require scribble/manual
         scribble/eval
         scheme/sandbox
         (for-syntax scheme/base)
         planet/util)
(require (planet cce/scheme:3:0))

(provide (all-defined-out)
         (all-from-out (planet cce/scheme:3:0)))
;; defmodule/this-package
;; declare-exporting/this-package

(define (package-version)
  (format "~a.~a" (this-package-version-maj) (this-package-version-min)))

;; ----

(define the-eval (make-base-eval))
(void
 (interaction-eval #:eval the-eval
                   (require scheme/class
                            "main.ss")))
(void
 (interaction-eval #:eval the-eval
                   (define connection% (class object% (super-new)))))

(define-syntax-rule (examples/results [example result] ...)
  (examples #:eval the-eval (eval:alts example result) ...))
(define-syntax-rule (my-interaction [example result] ...)
  (interaction #:eval the-eval (eval:alts example result) ...))
