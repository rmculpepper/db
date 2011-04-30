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

(define (sql-null->false x)
  (if (eq? x sql-null)
      #f
      x))

(define (false->sql-null x)
  (if (eq? x #f)
      sql-null
      x))

;; ----

(define-struct sql-date (year month day) #:transparent)
(define-struct sql-time (hour minute second nanosecond tz) #:transparent)
(define-struct sql-timestamp
  (year month day hour minute second nanosecond tz) 
  #:transparent)

;; Intervals must be "pre-multiplied" rather than carry extra sign field.
;; Rationale: postgresql, at least, allows mixture of signs, eg "1 month - 30 days"
(define-struct sql-interval
  (years months days hours minutes seconds nanoseconds)
  #:transparent
  #:guard (lambda (years months days hours minutes seconds nanoseconds _name)
            ;; Normalize years/months, days/hours/minutes/seconds/nanoseconds
            ;; Recall: quotient, remainder results have sign of first arg
            ;;   (if second arg is positive)
            (let ([total-months (+ months (* years 12))]
                  ;; FIXME: avoid overflow to bignums
                  [total-nsecs (+ nanoseconds
                                  (* (+ seconds
                                        (* minutes 60)
                                        (* hours 60 60)
                                        (* days 60 60 24))
                                     #e1e9))])
              (let*-values ([(years months) (quotient/remainder total-months 12)]
                            [(left-secs nsecs) (quotient/remainder total-nsecs #e1e9)]
                            [(left-mins secs) (quotient/remainder left-secs 60)]
                            [(left-hours mins) (quotient/remainder left-mins 60)]
                            [(days hours) (quotient/remainder left-hours 24)])
                (values years months days hours mins secs nsecs)))))

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

(define (sql-day-time-interval? x)
  (and (sql-interval? x)
       (zero? (sql-interval-years x))
       (zero? (sql-interval-months x))))

(define (sql-year-month-interval? x)
  (and (sql-interval? x)
       (zero? (sql-interval-days x))
       (zero? (sql-interval-hours x))
       (zero? (sql-interval-minutes x))
       (zero? (sql-interval-seconds x))
       (zero? (sql-interval-nanoseconds x))))

(define (sql-day-time-interval->seconds x)
  (+ (* (sql-interval-hours x) 60 60)
     (* (sql-interval-minutes x) 60)
     (sql-interval-seconds x)
     (/ (sql-interval-nanoseconds x) #e1e9)))

(define (same-signs? w x y z)
  (define some-pos? (or (positive? w) (positive? x) (positive? y) (positive? z)))
  (define some-neg? (or (negative? w) (negative? x) (negative? y) (negative? z)))
  (not (and some-pos? some-neg?)))

(define no-arg (gensym))

(define (sql-interval->sql-time x [default no-arg])
  (let ([d (sql-interval-days x)]
        [h (sql-interval-hours x)]
        [m (sql-interval-minutes x)]
        [s (sql-interval-seconds x)]
        [ns (sql-interval-nanoseconds x)])
    (cond [(and (sql-day-time-interval? x)
                (zero? d)
                (<= 0 h 23)
                (<= 0 m 59)
                (<= 0 s 59)
                (<= 0 ns (sub1 #e1e9)))
           (sql-time h m s ns #f)]
          [else
           (cond [(eq? default no-arg)
                  (error 'sql-day-time-interval->sql-time
                         "cannot convert interval to time: ~e" x)]
                 [(procedure? default) (default)]
                 [else default])])))

(define (sql-time->sql-interval x)
  (sql-interval 0 0 0
                (sql-time-hour x)
                (sql-time-minute x)
                (sql-time-second x)
                (sql-time-nanosecond x)))

;; ----

;; Note: MySQL allows 0 month, 0 day, etc.

(provide sql-null
         sql-null?
         sql-null->false
         false->sql-null)
(provide/contract
 [struct sql-date ([year exact-integer?]
                   [month (integer-in 0 12)]
                   [day (integer-in 0 31)])]
 [struct sql-time ([hour (integer-in 0 23)]
                   [minute (integer-in 0 59)]
                   [second (integer-in 0 61)] ;; leap seconds
                   [nanosecond (integer-in 0 (sub1 #e1e9))]
                   [tz (or/c #f exact-integer?)])]
 [struct sql-timestamp ([year exact-integer?]
                        [month (integer-in 0 12)]
                        [day (integer-in 0 31)]
                        [hour (integer-in 0 23)]
                        [minute (integer-in 0 59)]
                        [second (integer-in 0 61)]
                        [nanosecond (integer-in 0 (sub1 #e1e9))]
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

 [sql-day-time-interval?
  (-> any/c boolean?)]
 [sql-year-month-interval?
  (-> any/c boolean?)]
 [sql-day-time-interval->seconds
  (-> sql-day-time-interval? rational?)]
 [sql-interval->sql-time
  (->* (sql-interval?) (any/c)
       any)]
 [sql-time->sql-interval
  (-> sql-time? sql-day-time-interval?)])

;; ----

(struct sql-bits (length bv) #:transparent)

(define (make-sql-bits len)
  (sql-bits len (make-bytes (/ceiling len 8) 0)))

(define (make-sql-bits/bytes len bv)
  (sql-bits len bv))

(define (check-index fsym b index diff)
  (let ([len (sql-bits-length b)])
    (unless (< index (+ len diff))
      (if (zero? len)
          (error fsym "index out of range (empty sql-bits): ~e" index)
          (error fsym "index out of range: [0, ~a]: ~e" (+ len -1 diff) index)))))

(define (sql-bits-ref b i)
  (check-index 'sql-bits-ref b i 0)
  (let-values ([(bytei biti) (quotient/remainder i 8)])
    (not (zero? (bitwise-and (bytes-ref (sql-bits-bv b) bytei)
                             (arithmetic-shift 1 (- 7 biti)))))))

(define (sql-bits-set! b i v)
  (check-index 'sql-bits-set! b i 0)
  (let-values ([(bytei biti) (quotient/remainder i 8)])
    (let* ([bv (sql-bits-bv b)]
           [oldbyte (bytes-ref bv bytei)]
           [newbyte
            (bitwise-ior (bitwise-and oldbyte (bitwise-xor 255 (arithmetic-shift 1 (- 7 biti))))
                         (if v (arithmetic-shift 1 (- 7 biti)) 0))])
      (unless (= oldbyte newbyte)
        (bytes-set! bv bytei newbyte)))))

#|
(define (sql-bits-ref/n b i)
  (check-index 'sql-bits-ref/n b i 0)
  (if (sql-bits-ref/b b i) 1 0))
(define (sql-bits-set!/n b i v)
  (check-index 'sql-bits-set!/n b i 0)
  (sql-bits-set!/b b i (not (zero? v))))
|#

(define (sql-bits->list b)
  (for/list ([i (in-range (sql-bits-length b))])
    (sql-bits-ref b i)))

(define (list->sql-bits lst)
  (let ([b (make-sql-bits (length lst))])
    (for ([v (in-list lst)]
          [i (in-naturals)])
      (sql-bits-set! b i v))
    b))

(define (/ceiling x y)
  (let-values ([(q r) (quotient/remainder x y)])
    (+ q (if (zero? r) 0 1))))

(provide make-sql-bits/bytes
         sql-bits-bv)

(provide/contract
 [make-sql-bits
  (-> exact-nonnegative-integer? sql-bits?)]
 [sql-bits?
  (-> any/c boolean?)]
 [sql-bits-length
  (-> sql-bits? exact-nonnegative-integer?)]
 [sql-bits-ref
  (-> sql-bits? exact-nonnegative-integer? boolean?)]
 [sql-bits-set!
  (-> sql-bits? exact-nonnegative-integer? boolean? void?)]
 [sql-bits->list
  (-> sql-bits? (listof boolean?))]
 [list->sql-bits
  (-> (listof boolean?) sql-bits?)])
