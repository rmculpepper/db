;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/vector
         "interfaces.rkt"
         "sql-data.rkt")
(provide get-fi-name
         get-fi-typeid)

(define (get-fi-name alist)
  (cond [(assq 'name alist)
         => cdr]
        [else #f]))

(define (get-fi-typeid alist)
  (cond [(assq 'typeid alist)
         => cdr]
        [else #f]))
