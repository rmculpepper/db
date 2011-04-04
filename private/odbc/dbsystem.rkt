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
    (define/public (get-known-types) (map cadr type-table))
    (define/public (has-support? x) #f)

    (define/public (get-parameter-handlers param-infos)
      (map (lambda (param-info)
             ;; FIXME: do parameter checks! (for drivers that give param types)
             check-param)
           param-infos))

    (define/public (get-result-handlers result-infos)
      (error 'get-result-handlers "unsupported"))

    (super-new)))

(define dbsystem
  (new odbc-dbsystem%))

;; ----

(define (check-param fsym index param-info param)
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
