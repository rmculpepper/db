;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/unit
         "../generic/signatures.rkt"
         "main.rkt")
(provide mysql@)

(define-unit-from-context mysql@ database^)
