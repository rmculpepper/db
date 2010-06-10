;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/unit)
(provide database^)

(define-signature database^
  (connect
   dbsystem))
