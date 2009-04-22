#lang scheme/base
(require scribble/manual
         (for-syntax scheme/base)
         (for-syntax planet/util))
(require (planet cce/scheme:3:0))

(provide (all-defined-out))

(provide (all-from-out (planet cce/scheme:3:0)))
;; defmodule/this-package
;; declare-exporting/this-package

(begin-for-syntax
 (define package-owner (this-package-version-owner))
 (define package-name (this-package-version-name))
 (define package-maj (this-package-version-maj))
 (define package-min (this-package-version-min))
 (define package
   `(,package-owner ,package-name ,package-maj ,package-min)))

(define-syntax (thispackagemodname stx)
  (syntax-case stx ()
    [(_ mod)
     #`(schememodname (planet mod #,package))]))

#;
(define-syntax (defmodule/this-package stx)
  (syntax-case stx ()
    [(_ mod)
     #`(defmodule (planet mod #,package))]))

#;
(define-syntax (declare-exporting/this-package stx)
  (syntax-case stx ()
    [(_ mod)
     #`(declare-exporting (planet mod #,package))]))

