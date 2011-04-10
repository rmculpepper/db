;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

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
    (define/public (typeids->types typeids) (map typeid->type typeids))
    (define/public (get-known-types) known-types+aliases)
    (define/public (has-support? x) #f)

    (define/public (get-parameter-handlers param-typeids)
      (map (lambda (param-typeid)
             ;; FIXME: do parameter checks! (for drivers that give param types)
             check-param)
           param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map (lambda (dvec) (vector-ref dvec 1)) dvecs))

    (super-new)))

(define dbsystem
  (new odbc-dbsystem%))

;; ----

(define (check-param fsym index param)
  (unless (or (string? param)
              (bytes? param)
              (rational? param)
              (sql-date? param)
              (sql-time? param)
              (sql-timestamp? param))
    ;; FIXME: need fsym propagation
    (error fsym "cannot convert to ODBC unknown type: ~e" param))
  param)

;; ----

(define-type-table (known-type-aliases
                    known-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    type->type-reader
                    type->type-writer)
  (0  unknown        ()           #f #f)
  (1  character      (char)       #f #f)
  (2  numeric        ()           #f #f)
  (3  decimal        ()           #f #f)
  (4  integer        (int)        #f #f)
  (5  smallint       ()           #f #f)
  (6  float          ()           #f #f)
  (7  real           ()           #f #f)
  (8  double         ()           #f #f)
  (9  datetime       ()           #f #f)
  (12 varchar        ()           #f #f)
  (91 date           ()           #f #f)
  (92 time           ()           #f #f)
  (93 timestamp      ()           #f #f)
  (-1 longvarchar    ()           #f #f)
  (-2 binary         ()           #f #f)
  (-3 varbinary      ()           #f #f)
  (-4 longvarbinary  ()           #f #f)
  (-5 bigint         ()           #f #f)
  (-6 tinyint        ()           #f #f)
  (-7 bit1           ()           #f #f)) ;; not bit(n), always single bit

(define known-types+aliases
  (append known-types known-type-aliases))
