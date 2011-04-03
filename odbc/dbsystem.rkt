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
    (define/public (typeids->types typeids) (map typeid->type typeids))
    (define/public (typeids->type-readers x) null)
    (define/public (typeids->type-writers typeids)
      (map (lambda (x) values) typeids)) ;; FIXME

    (define/public (get-known-types) (map cadr type-table))
    (define/public (has-support? x) #f)
    (super-new)))

(define dbsystem
  (new odbc-dbsystem%))

;; ----

(define type-table
  '((0 unknown)
    (1 char)
    (2 numeric)
    (3 decimal)
    (4 integer)
    (5 smallint)
    (6 float)
    (7 real)
    (8 double)
    (9 datetime)
    (12 varchar)
    (91 date)
    (92 time)
    (93 timestamp)
    (-1 longvarchar)
    (-2 binary)
    (-3 varbinary)
    (-4 longvarbinary)
    (-5 bigint)
    (-6 tinyint)
    (-7 bit)))

(define (typeid->type typeid)
  (cond [(assoc typeid type-table)
         => cadr]
        [else 'unsupported]))
