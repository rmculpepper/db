;; Copyright 2008-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/unit
         "../generic/signatures.rkt"
         "main.rkt")
(provide mysql@)

(define-unit-from-context mysql@ database^)