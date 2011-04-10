;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/atomic
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "ffi.rkt")
(provide dbsystem)

(define odbc-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'odbc) ;; FIXME: need also underlying driver info
    (define/public (typeids->types typeids) (map typeid->type typeids))
    (define/public (get-known-types) supported-types)
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

(define-type-table (supported-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    type->type-reader
                    type->type-writer)

  (0  unknown        ()           #t #f #f)
  (1  character      (char)       #t #f #f)
  (2  numeric        ()           #t #f #f)
  (3  decimal        ()           #t #f #f)
  (4  integer        (int)        #t #f #f)
  (5  smallint       ()           #t #f #f)
  (6  float          ()           #t #f #f)
  (7  real           ()           #t #f #f)
  (8  double         ()           #t #f #f)
  (9  datetime       ()           #t #f #f)
  (12 varchar        ()           #t #f #f)
  (91 date           ()           #t #f #f)
  (92 time           ()           #t #f #f)
  (93 timestamp      ()           #t #f #f)
  (-1 longvarchar    ()           #t #f #f)
  (-2 binary         ()           #t #f #f)
  (-3 varbinary      ()           #t #f #f)
  (-4 longvarbinary  ()           #t #f #f)
  (-5 bigint         ()           #t #f #f)
  (-6 tinyint        ()           #t #f #f)
  (-7 bit1           ()           #t #f #f) ;; not bit(n), always single bit

  ;; Unsupported types

  (101 interval-year          () #f #f #f)
  (102 interval-month         () #f #f #f)
  (103 interval-day           () #f #f #f)
  (104 interval-hour          () #f #f #f)
  (105 interval-minute        () #f #f #f)
  (106 interval-second        () #f #f #f)
  (107 interval-year-month    () #f #f #f)
  (108 interval-day-hour      () #f #f #f)
  (109 interval-day-minute    () #f #f #f)
  (110 interval-day-second    () #f #f #f)
  (111 interval-hour-minute   () #f #f #f)
  (112 interval-hour-second   () #f #f #f)
  (113 interval-minute-second () #f #f #f))
