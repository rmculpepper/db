;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/atomic
         "../generic/query.rkt"
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "ffi.rkt")
(provide dbsystem)

(define odbc-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'odbc) ;; FIXME: need also underlying driver info
    (define/public (typeids->types x) null)
    (define/public (typeids->type-readers x) null)
    (define/public (typeids->type-writers typeids)
      (map (lambda (x) values) typeids)) ;; FIXME

    (define/public (get-known-types) '())
    (define/public (has-support? x) #f)
    (super-new)))

(define dbsystem
  (new odbc-dbsystem%))
