;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/unit)
(provide database^)

(define-signature database^
  (connect
   dbsystem))
