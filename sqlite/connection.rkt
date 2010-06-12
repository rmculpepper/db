;; Copyright 2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         "../generic/query.rkt"
         "sqlite-ffi.rkt"
         "constants.rkt")

(define base%
  (class* object% (connection:admin<%>)
    (init db)
    (define -db db)

    (define/public (get-dbsystem) dbsystem)
    (define/public (connected?) (and -db #t))
    (define/public (disconnect)
      ...
      (set! -db #f))

    (super-new)))

(define prepared-mixin
  (mixin (connection:admin<%> primitive-query<%>) ()

    (define/public (prepare-multiple stmts)
      ...)

    (super-new)))

(define connection%
  (class* (prepared-mixin (primitive-query-mixin base%)) (connection<%>)
    (super-new)))
