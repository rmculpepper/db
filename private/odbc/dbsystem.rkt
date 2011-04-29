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
    (define/public (get-known-types) supported-types)
    (define/public (has-support? x) #f)

    (define/public (get-parameter-handlers param-typeids)
      (map (lambda (param-typeid)
             ;; FIXME: do parameter checks! (for drivers that give param types)
             check-param)
           param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map (lambda (dvec) (vector-ref dvec 1)) dvecs))

    (define/public (describe-typeids typeids)
      (map describe-typeid typeids))

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
                    describe-typeid)

  (0  unknown        ()           #t)
  (1  character      (char)       #t)
  (2  numeric        ()           #t)
  (3  decimal        ()           #t)
  (4  integer        (int)        #t)
  (5  smallint       ()           #t)
  (6  float          ()           #t)
  (7  real           ()           #t)
  (8  double         ()           #t)
  (9  datetime       ()           #t)
  (12 varchar        ()           #t)
  (91 date           ()           #t)
  (92 time           ()           #t)
  (93 timestamp      ()           #t)
  (-1 longvarchar    ()           #t)
  (-2 binary         ()           #t)
  (-3 varbinary      ()           #t)
  (-4 longvarbinary  ()           #t)
  (-5 bigint         ()           #t)
  (-6 tinyint        ()           #t)
  (-7 bit1           ()           #t) ;; not bit(n), always single bit

  ;; Unsupported types

  (101 interval-year          () #f)
  (102 interval-month         () #f)
  (103 interval-day           () #f)
  (104 interval-hour          () #f)
  (105 interval-minute        () #f)
  (106 interval-second        () #f)
  (107 interval-year-month    () #f)
  (108 interval-day-hour      () #f)
  (109 interval-day-minute    () #f)
  (110 interval-day-second    () #f)
  (111 interval-hour-minute   () #f)
  (112 interval-hour-second   () #f)
  (113 interval-minute-second () #f))
