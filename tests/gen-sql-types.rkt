;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require rackunit
         racket/class
         (prefix-in srfi: srfi/19)
         "../private/generic/main.rkt"
         "../private/generic/sql-convert.rkt"
         "config.rkt")
(provide gen-sql-types:test)

(define gen-sql-types:test
  (test-suite "Parsing SQL types"
    (test-case "date"
      (check-equal? (parse-date "1980-08-17")
                    (make-sql-date 1980 08 17)))
    (test-case "time"
      (check-equal? (parse-time "12:34:56")
                    (make-sql-time 12 34 56 0 #f))
      (check-equal? (parse-time "12:34:56.789")
                    (make-sql-time 12 34 56 789000000 #f))
      (check-equal? (parse-time "12:34:56.000789")
                    (make-sql-time 12 34 56 000789000 #f)))
    (test-case "timetz"
      (check-equal? (parse-time-tz "12:34:56+0123")
                    (make-sql-time 12 34 56 0 4980))
      (check-equal? (parse-time-tz "12:34:56.789+0123")
                    (make-sql-time 12 34 56 789000000 4980))
      (check-equal? (parse-time-tz "12:34:56.000789-0123")
                    (make-sql-time 12 34 56 000789000 -4980)))
    (test-case "timestamp"
      (check-equal?
       (parse-timestamp "1980-08-17 12:34:56")
       (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
      (check-equal?
       (parse-timestamp "1980-08-17 12:34:56.123")
       (make-sql-timestamp 1980 08 17 12 34 56 123000000 #f))
      (check-equal?
       (parse-timestamp "1980-08-17 12:34:56.000123")
       (make-sql-timestamp 1980 08 17 12 34 56 000123000 #f)))
    (test-case "timestamp-with-time-zone"
      (check-equal?
       (parse-timestamp-tz "1980-08-17 12:34:56+0123")
       (make-sql-timestamp 1980 08 17 12 34 56 0 4980))
      (check-equal?
       (parse-timestamp-tz "1980-08-17 12:34:56.123+0123")
       (make-sql-timestamp 1980 08 17 12 34 56 123000000 4980))
      (check-equal?
       (parse-timestamp-tz "1980-08-17 12:34:56.000123-0123")
       (make-sql-timestamp 1980 08 17 12 34 56 000123000 -4980)))
    (test-case "numeric"
      (check-equal? (parse-decimal "12345678901234567890")
                    12345678901234567890)
      (check-equal? (parse-decimal "-12345678901234567890")
                    -12345678901234567890))))
