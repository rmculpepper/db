;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/match
         (prefix-in srfi: srfi/19))

;; SQL Data
;; Datatypes for things that have no appropriate corresponding Scheme datatype

(define sql-null
  (let ()
    (define-struct sql-null ())
    (make-sql-null)))

(define (sql-null? x)
  (eq? x sql-null))

;; ----

(define-struct sql-date (year month day) #:transparent)
(define-struct sql-time (hour minute second nanosecond tz) #:transparent)
(define-struct sql-timestamp
  (year month day hour minute second nanosecond tz) 
  #:transparent)

;; Intervals must be "pre-multiplied" rather than carry extra sign field.
;; Rationale: postgresql, at least, allows mixture of signs, eg "1 month - 30 days"
;; No "normalization" requirement; may have 2 months + 37 days + 60 hours...
;; but, should probably normalize within HMS.N
(define-struct sql-interval
  (years months days hours minutes seconds nanoseconds)
  #:transparent)

;; ----

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

;; ----

;; A interval is "simple" if it involves only hours, minutes, seconds, nsecs
;; In that case, it represents a definite amount of time.
;; For convenience elsewhere, require all HMS.N have same sign (or are zero).
(define (sql-simple-interval? x)
  (define (same-signs? w x y z)
    (define some-pos? (or (positive? w) (positive? x) (positive? y) (positive? z)))
    (define some-neg? (or (negative? w) (negative? x) (negative? y) (negative? z)))
    (not (and some-pos? some-neg?)))
  (and (sql-interval? x)
       (zero? (sql-interval-years x))
       (zero? (sql-interval-months x))
       (zero? (sql-interval-days x))
       (same-signs? (sql-interval-hours x)
                    (sql-interval-minutes x)
                    (sql-interval-seconds x)
                    (sql-interval-nanoseconds x))))

(define (sql-simple-interval->seconds x)
  (+ (* (sql-interval-hours x) 60 60)
     (* (sql-interval-minutes x) 60)
     (sql-interval-seconds x)
     (/ (sql-interval-nanoseconds x) #e1e9)))

(define no-arg (gensym))

(define (sql-simple-interval->sql-time x [default no-arg])
  (let ([h (sql-interval-hours x)]
        [m (sql-interval-minutes x)]
        [s (sql-interval-seconds x)]
        [ns (sql-interval-nanoseconds x)])
    (cond [(and (<= 0 h 23)
                (<= 0 m 59)
                (<= 0 s 59)
                (<= 0 ns (- #e1e9 1)))
           (sql-time h m s ns #f)]
          [else
           (cond [(eq? default no-arg)
                  (error 'sql-interval->sql-time
                         "cannot convert interval to time: ~e" x)]
                 [(procedure? default) (default)]
                 [else default])])))

;; ----

(provide sql-null
         sql-null?)
(provide/contract
 [struct sql-date ([year exact-integer?]
                   [month exact-nonnegative-integer?]
                   [day exact-nonnegative-integer?])]
 [struct sql-time ([hour exact-nonnegative-integer?]
                   [minute exact-nonnegative-integer?]
                   [second exact-nonnegative-integer?]
                   [nanosecond exact-nonnegative-integer?]
                   [tz (or/c #f exact-integer?)])]
 [struct sql-timestamp ([year exact-integer?]
                        [month exact-nonnegative-integer?]
                        [day exact-nonnegative-integer?]
                        [hour exact-nonnegative-integer?]
                        [minute exact-nonnegative-integer?]
                        [second exact-nonnegative-integer?]
                        [nanosecond exact-nonnegative-integer?]
                        [tz (or/c #f exact-integer?)])]
 [struct sql-interval ([years exact-integer?]
                       [months exact-integer?]
                       [days exact-integer?]
                       [hours exact-integer?]
                       [minutes exact-integer?]
                       [seconds exact-integer?]
                       [nanoseconds exact-integer?])]

 [sql-datetime->srfi-date
  (-> (or/c sql-date? sql-time? sql-timestamp?)
      srfi:date?)]
 [srfi-date->sql-date
  (-> srfi:date? sql-date?)]
 [srfi-date->sql-time
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-time?)]
 [srfi-date->sql-time-tz
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-time?)]
 [srfi-date->sql-timestamp
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-timestamp?)]
 [srfi-date->sql-timestamp-tz
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-timestamp?)]

 [sql-simple-interval?
  (-> any/c boolean?)]
 [sql-simple-interval->seconds
  (-> sql-simple-interval? rational?)]
 [sql-simple-interval->sql-time
  (->* (sql-simple-interval?) (any/c)
       any)])
