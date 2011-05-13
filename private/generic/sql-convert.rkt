;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         (prefix-in srfi: srfi/19)
         "sql-data.rkt")

#|
parse-<type> : string -> racket-datum

Takes the textual wire representation of <type> as a string, and
produces the corresponding racket datum.

No conversion may be passed sql-null.
|#

(provide parse-char1
         parse-decimal
         parse-date
         parse-time
         parse-time-tz
         parse-timestamp
         parse-timestamp-tz
         parse-interval)

(define (parse-char1 s)
  (string-ref s 0))

(define (parse-decimal s)
  (cond [(equal? s "NaN") +nan.0]
        [(regexp-match #rx"^-?([0-9]*)$" s)
         ;; big integer
         => (lambda (m)
              (string->number s))]
        [(regexp-match #rx"^-?([0-9]*)\\.([0-9]*)$" s)
         => (lambda (m)
              (+ (string->number (cadr m))
                 (parse-exact-fraction (caddr m))))]
        [else (parse-error "numeric" s)]))

(define (parse-exact-fraction s)
  ;; eg: (parse-exact-fraction "12") = 12/100
  (/ (string->number s)
     (expt 10 (string-length s))))

(define (parse-date d)
  (srfi-date->sql-date
   (srfi:string->date d "~Y-~m-~d")))

(define time/ns-rx #rx"^[0-9]*:[0-9]*:[0-9]*\\.([0-9]*)")
(define timestamp/ns-rx #rx"^.* [0-9]*:[0-9]*:[0-9]*\\.([0-9]*)")

(define (ns-of t rx)
  (let ([m (regexp-match rx t)])
    (if m
        (* #e1e9 (parse-exact-fraction (cadr m)))
        0)))

(define (parse-time t)
  (srfi-date->sql-time
   (srfi:string->date t "~k:~M:~S")
   (ns-of t time/ns-rx)))

(define (parse-time-tz t)
  (srfi-date->sql-time-tz
   (srfi:string->date t "~k:~M:~S~z")
   (ns-of t time/ns-rx)))

(define (parse-timestamp t)
  (srfi-date->sql-timestamp
   (srfi:string->date t "~Y-~m-~d ~k:~M:~S")
   (ns-of t timestamp/ns-rx)))

(define (parse-timestamp-tz t)
  (srfi-date->sql-timestamp-tz
   (srfi:string->date t "~Y-~m-~d ~k:~M:~S~z")
   (ns-of t timestamp/ns-rx)))

(define interval-rx
  (regexp
   (string-append "^"
                  "(?:(-?[0-9]*) years? *)?"
                  "(?:(-?[0-9]*) mons? *)?"
                  "(?:(-?[0-9]*) days? *)?"
                  "(?:(-?)([0-9]*):([0-9]*):([0-9]*)(?:\\.([0-9]*))?)?"
                  "$")))

(define (parse-interval s)
  (define (to-num m)
    (if m (string->number m) 0))
  (define match-result (regexp-match interval-rx s))
  (match match-result
    [(list _whole years months days tsign hours mins secs fsec)
     (let* ([years (to-num years)]
            [months (to-num months)]
            [days (to-num days)]
            [sg (if (equal? tsign "-") - +)]
            [hours (sg (to-num hours))]
            [mins (sg (to-num mins))]
            [secs (sg (to-num secs))]
            [nsecs (if fsec
                       (let ([flen (string-length fsec)])
                         (* (string->number (substring fsec 0 (min flen 9)))
                            (expt 10 (min 0 (- 9 flen)))))
                       0)])
       (sql-interval years months days hours mins secs nsecs))]))


;; ----------------------------------------

;; parse-error : string string -> (raises error)
(define (parse-error type rep)
  (error 'query* "internal error: cannot parse as SQL type ~s: ~e"
         type rep))

;; ========================================

#|
marshal-<type> : fsym index datum -> string

Takes a racket datum and converts it into <type>'s text wire format.
No conversion may be passed sql-null.
|#

(provide marshal-decimal
         marshal-date
         marshal-time
         marshal-time-tz
         marshal-timestamp
         marshal-timestamp-tz
         marshal-interval)

(define (marshal-decimal f i n)
  (cond [(not (real? n))
         (marshal-error f i "numeric" n)]
        [(eqv? n +nan.0)
         "NaN"]
        [(or (eqv? n +inf.0) (eqv? n -inf.0))
         (marshal-error f i "numeric" n)]
        [(or (integer? n) (inexact? n))
         (number->string n)]
        [(exact? n)
         ;; Bleah.
         (or (number->exact-decimal n)
             (number->string (exact->inexact n)))]))

(define (number->exact-decimal n)
  (define (factor-out n factor fpower)
    (let-values ([(q r) (quotient/remainder n factor)])
      (if (zero? r)
          (factor-out q factor (add1 fpower))
          (values n fpower))))
  (let* ([whole-part (truncate n)]
         [fractional-part (- (abs n) (abs whole-part))]
         [num (numerator fractional-part)]
         [den (denominator fractional-part)])
    (let*-values ([(den* fives) (factor-out den 5 0)]
                  [(den** twos) (factor-out den* 2 0)])
      (and (= 1 den**)
           (let* ([tens (max fives twos 1)]
                  [new-den (expt 10 tens)]
                  [new-num (* num (quotient new-den den))]
                  [num-str (number->string new-num)])
             (string-append (number->string whole-part)
                            "."
                            (make-string (- tens (string-length num-str))
                                         #\0)
                            num-str))))))

(define (marshal-date f i d)
  (unless (sql-date? d)
    (marshal-error f i "date" d))
  (srfi:date->string (sql-datetime->srfi-date d) "~Y-~m-~d"))

(define (marshal-time f i t)
  (unless (sql-time? t)
    (marshal-error f i "time" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N"))

(define (marshal-time-tz f i t)
  (unless (sql-time? t)
    (marshal-error f i "time" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N~z"))

(define (marshal-timestamp f i t)
  (unless (sql-timestamp? t)
    (marshal-error f i "timestamp" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N"))

(define (marshal-timestamp-tz f i t)
  (unless (sql-timestamp? t)
    (marshal-error f i "timestamp" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N~z"))

(define (marshal-interval f i t)
  (define (tag num unit)
    (if (zero? num) "" (format "~a ~a " num unit)))
  (match t
    [(sql-interval years months days hours minutes seconds nanoseconds)
     ;; Note: we take advantage of PostgreSQL's liberal interval parser
     ;; and we acknowledge its micro-second precision.
     (string-append (tag years "years")
                    (tag months "months")
                    (tag days "days")
                    (tag minutes "minutes")
                    (tag seconds "seconds")
                    (tag (quotient nanoseconds 1000) "microseconds"))]
    [else
     (marshal-error f i "interval" t)]))

;; ----------------------------------------

;; marshal-error : string datum -> (raises error)
(define (marshal-error f i type datum)
  (error f "cannot marshal as SQL type ~s: ~e"
         type datum))
