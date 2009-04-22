;; Copyright 2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/unit
         "../generic/signatures.ss"
         "main.ss")
(provide mysql@)

(define-unit-from-context mysql@ database^)
