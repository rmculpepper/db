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
         "ffi.rkt"
         "types.rkt")
(provide dbsystem
         (all-from-out "types.rkt"))

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

;; ========================================

(define type-writer-table
  `((,SQL_CHAR . ,send-char)
    (,SQL_VARCHAR . ,send-varchar)
    ;; SQL_DECIMAL
    ;; SQL_NUMERIC
    (,SQL_SMALLINT . ,send-smallint)
    (,SQL_INTEGER . ,send-integer)
    (,SQL_REAL . ,send-real)
    ;; SQL_FLOAT
    (,SQL_DOUBLE . ,send-double)
    ;; SQL_BIT
    (,SQL_TINYINT . ,send-tinyint)
    (,SQL_BIGINT . ,send-bigint)
    ;; SQL_BINARY, SQL_VARBINARY
    (,SQL_TYPE_DATE . ,send-date)
    (,SQL_TYPE_TIME . ,send-time)
    (,SQL_TYPE_TIMESTAMP . ,send-timestamp)
    ;; SQL_UTCDATETIME
    ;; SQL_UTCTIME
    ;; SQL_INTERVAL_*
    ;; SQL_GUID
    ))

(define (typeid->type-writer typeid)
  (cond [(assoc typeid type-writer-table)
         => cdr]
        [else (error 'typeid->type-writer "unsupported type id: ~e" typeid)]))
