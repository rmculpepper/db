;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/unit
         "../generic/signatures.rkt"
         "main.rkt")
(provide sqlite3@)

(define-unit-from-context sqlite3@ database^)
