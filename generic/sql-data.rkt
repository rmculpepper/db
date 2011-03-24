;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/match
         (prefix-in srfi: srfi/19))

(provide sql-null
         sql-null?

         (struct-out sql-date)
         (struct-out sql-time)
         (struct-out sql-timestamp)

         sql-datetime->srfi-date
         srfi-date->sql-date
         srfi-date->sql-time
         srfi-date->sql-time-tz
         srfi-date->sql-timestamp
         srfi-date->sql-timestamp-tz)

;; SQL Data
;; Datatypes for things that have no appropriate corresponding Scheme datatype

(define sql-null
  (let ()
    (define-struct sql-null ())
    (make-sql-null)))

(define (sql-null? x)
  (eq? x sql-null))

(define-struct sql-date (year month day) #:transparent)
(define-struct sql-time (hour minute second nanosecond tz) #:transparent)
(define-struct sql-timestamp
  (year month day hour minute second nanosecond tz) 
  #:transparent)

(define (sql-datetime->srfi-date datetime)
  (match datetime
    [(struct sql-date (year month day))
     (srfi:make-date 0 0 0 0 day month year 0)]
    [(struct sql-time (hour minute second nanosecond tz))
     (srfi:make-date nanosecond second minute hour 0 0 0 (or tz 0))]
    [(struct sql-timestamp (year month day hour minute second nanosecond tz))
     (srfi:make-date nanosecond second minute hour day month year (or tz 0))]
    [else
     (raise-type-error 'sql-datetime->srfi-date
                       "sql-date, sql-time, or sql-timestamp"
                       datetime)]))

(define (srfi-date->sql-date date)
  (make-sql-date (srfi:date-year date)
                 (srfi:date-month date)
                 (srfi:date-day date)))

(define (srfi-date->sql-time* date tz? ns)
  (make-sql-time (srfi:date-hour date)
                 (srfi:date-minute date)
                 (srfi:date-second date)
                 (or ns (srfi:date-nanosecond date))
                 (and tz? (srfi:date-zone-offset date))))

(define (srfi-date->sql-time date [ns #f])
  (srfi-date->sql-time* date #f ns))

(define (srfi-date->sql-time-tz date [ns #f])
  (srfi-date->sql-time* date #t ns))

(define (srfi-date->sql-timestamp* date tz? ns)
  (make-sql-timestamp (srfi:date-year date)
                      (srfi:date-month date)
                      (srfi:date-day date)
                      (srfi:date-hour date)
                      (srfi:date-minute date)
                      (srfi:date-second date)
                      (or ns (srfi:date-nanosecond date))
                      (and tz? (srfi:date-zone-offset date))))

(define (srfi-date->sql-timestamp date [ns #f])
  (srfi-date->sql-timestamp* date #f ns))

(define (srfi-date->sql-timestamp-tz date [ns #f])
  (srfi-date->sql-timestamp* date #t ns))
