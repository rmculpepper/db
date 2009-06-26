;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/unit
         "../generic/signatures.ss"
         "../postgresql.ss")
(provide postgresql@)

(define-unit-from-context postgresql@ database^)
