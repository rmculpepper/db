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
(provide (all-defined-out))

;; FIXME: what endianness to use? system? little?

(struct param (ctype sqltype buffer size digits) #:prefab)

(define DC 0) ;; don't care
(define size 0) ;; FIXME

;; typeid = nat
;; type-writer = (datum -> param)

(define (send-char s)
  (let ([buf (string->bytes/utf-8 s)])
    (param SQL_C_CHAR SQL_CHAR buf size DC)))

(define (send-varchar s)
  (let ([buf (string->bytes/utf-8 s)])
    (param SQL_C_CHAR SQL_VARCHAR buf size DC)))

(define (send-tinyint n)
  (let ([buf (bytes n)])
    (param SQL_C_SHORT SQL_SMALLINT buf size DC)))

(define (send-smallint n)
  (let ([buf (integer->integer-bytes n 2)])
    (param SQL_C_SHORT SQL_SMALLINT buf size DC)))

(define (send-integer n)
  (let ([buf (integer->integer-bytes n 4)])
    (param SQL_C_LONG SQL_INTEGER buf size DC)))

(define (send-bigint n)
  (let ([buf (integer->integer-bytes n 8)])
    (param SQL_C_SBIGINT SQL_BIGINT buf size DC)))

(define (send-real n)
  (let ([buf (real->floating-point-bytes n 4)])
    (param SQL_C_FLOAT SQL_REAL buf size DC)))

(define (send-double n)
  (let ([buf (real->floating-point-bytes n 8)])
    (param SQL_C_DOUBLE SQL_DOUBLE buf size DC)))

(define (send-date x)
  (let* ([y (sql-date-year x)]
         [m (sql-date-month x)]
         [d (sql-date-day x)]
         [buf (bytes-append (integer->integer-bytes y 2)
                            (integer->integer-bytes m 2)
                            (integer->integer-bytes d 2))])
    (param SQL_C_DATE SQL_DATE buf size DC)))

(define (send-time x)
  (let* ([h (sql-time-hour x)]
         [m (sql-time-minute x)]
         [s (sql-time-second x)]
         [buf (bytes-append (integer->integer-bytes h 2)
                            (integer->integer-bytes m 2)
                            (integer->integer-bytes s 2))])
    (param SQL_C_TIME SQL_TIME buf size DC)))

(define (send-timestamp x)
  (let* ([buf (bytes-append
               (integer->integer-bytes (sql-timestamp-year x) 2)
               (integer->integer-bytes (sql-timestamp-month x) 2)
               (integer->integer-bytes (sql-timestamp-day x) 2)
               (integer->integer-bytes (sql-timestamp-hour x) 2)
               (integer->integer-bytes (sql-timestamp-minute x) 2)
               (integer->integer-bytes (sql-timestamp-second x) 2)
               (integer->integer-bytes (sql-timestamp-nanosecond x) 2))])
    (param SQL_C_TIMESTAMP SQL_TIMESTAMP buf size DC)))
