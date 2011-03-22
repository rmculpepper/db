;; Copyright 2000-2010 Ryan Culpepper
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
         srfi-date->sql-timestamp-tz

         parse-string
         parse-bytea
         parse-integer
         parse-real
         parse-decimal
         parse-boolean
         parse-date
         parse-time
         parse-time-tz
         parse-timestamp
         parse-timestamp-tz

         marshal-string
         marshal-ascii-string
         marshal-bytea
         marshal-integer
         marshal-int1
         marshal-int2
         marshal-int3
         marshal-int4
         marshal-int8
         marshal-real
         marshal-decimal
         marshal-bool
         marshal-date
         marshal-time
         marshal-time-tz
         marshal-timestamp
         marshal-timestamp-tz)

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

;; external representation => Scheme datum
;; All input-conversions take strings

;; raise-parse-error : string string -> (raises error)
(define (raise-parse-error type rep)
  (raise-user-error
   'external-representation->datum
   (format "cannot interpret as a SQL ~a: ~s" type rep)))

;; string & bytea need full decoder

(define (parse-string s) s)

(define (parse-bytea s)
  (define (decode in out)
    (define (loop)
      (let ([next (read-char in)])
        (cond [(eof-object? next)
               (void)]
              [(eq? next #\\)
               (escaped-loop)]
              [else
               (let ([next-as-byte (char->integer next)])
                 (unless (< next-as-byte 256)
                   (raise-parse-error "bytea" s))
                 (write-byte next-as-byte out)
                 (loop))])))
    (define (escaped-loop)
      (let ([next (peek-char in)])
        (cond [(eq? next #\\)
               (read-char in)
               (write-char next out)]
              [else
               (let* ([s (read-string 3 in)]
                      [n (string->number s 8)])
                 (unless (< n 256)
                   (raise-parse-error "bytea" s))
                 (write-byte n out))])
        (loop)))
    (loop))
  (if (regexp-match? #rx"\\\\" s)
      (let ([out (open-output-bytes)]
            [in (open-input-string s)])
        (decode in out)
        (get-output-bytes out))
      (with-handlers ([exn:fail?
                       (lambda (e) (raise-parse-error "bytea" s))])
        (string->bytes/latin-1 s))))

;; other types do not contain non-ASCII bytes (FIXME: VERIFY)

(define (parse-integer s)
  (or (string->number s)
      (raise-parse-error "integer" s)))

(define (parse-real s)
  (cond [(string->number s) => exact->inexact]
        [(equal? s "NaN") +nan.0]
        [(equal? s "Infinity") +inf.0]
        [(equal? s "-Infinity") -inf.0]
        [else (raise-parse-error "real" s)]))

(define (parse-decimal s)
  (cond [(equal? s "NaN") +nan.0]
        [(regexp-match #rx"^-?([0-9]*)$" s)
         ;; big integer
         => (lambda (m)
              (string->number s))]
        [(regexp-match #rx"^-?([0-9]*)\\.([0-9]*)$" s)
         => (lambda (m)
              (+ (string->number (cadr m))
                 (let ([fp (caddr m)])
                   (parse-exact-fraction fp))))]
        [else (raise-parse-error "numeric" s)]))

;; parse-exact-fraction : string[in #rx"[0-9]*"] -> exact number
;; Given the fractional part of a number (including leading zeros),
;; produces an exact number representing the fraction.
;; eg: (parse-exact-fraction "12") = 12/100
(define (parse-exact-fraction s)
  (/ (string->number s)
     (expt 10 (string-length s))))

(define (parse-boolean s)
  (cond [(equal? s "t") #t]
        [(equal? s "f") #f]
        [else (raise-parse-error "boolean" s)]))

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

;; Scheme datum => external representation
;; All conversions take the appropriate Scheme datatype
;; and produce bytes.
;; No conversion may be passed sql-null values.

;; raise-marshal-error : string datum -> (raises error)
(define (raise-marshal-error type datum)
  (raise-user-error
   'datum->external-representation
   "cannot create ~s representation for value: ~s" type datum))

;; encode : input-port output-port boolean -> void
(define (encode in out bytes-mode?)
  (define (loop)
    (let ([next-byte (read-byte in)])
      (cond [(eof-object? next-byte)
             (void)]
            [(= next-byte (char->integer #\\))
             (write-char #\\ out)
             (write-char #\\ out)
             (loop)]
            [(= next-byte 0)
             (unless bytes-mode?
               (raise-user-error
                'datum->external-representation
                "NUL character not allowed"))
             (write-char #\\ out)
             (write-string "000" out)
             (loop)]
            [(and bytes-mode? (> next-byte 127))
             (write-char #\\ out)
             (let ([ns (number->string next-byte 8)])
               (write-string "000" out (string-length ns) 3)
               (write-string ns out))
             (loop)]
            [else
             (write-byte next-byte out)
             (loop)])))
  (loop))

(define (marshal-string s) s)

(define (marshal-ascii-string s)
  (for ([i (in-range (string-length s))])
    (unless (<= 0 (char->integer (string-ref s i)) 127)
      (raise-type-error 'marshal-ascii-string
                        "string containing only ascii characters"
                        s)))
  s)

(define (marshal-bytea s)
  (unless (bytes? s)
    (raise-marshal-error "bytea" s))
  (let ([in (open-input-bytes s)]
        [out (open-output-string)])
    (encode in out #t)
    (get-output-string out)))

(define (marshal-integer n)
  (unless (and (integer? n) (exact? n))
    (raise-marshal-error "integer" n))
  (number->string n))

(define (marshal-int1 n)
  (unless (and (integer? n) (exact? n) (<= #x-80 n #x7F))
    (raise-marshal-error "int1" n))
  (number->string n))

(define (marshal-int2 n)
  (unless (and (integer? n) (exact? n) (<= #x-8000 n #x7FFF))
    (raise-marshal-error "int2" n))
  (number->string n))

(define (marshal-int3 n)
  (unless (and (integer? n) (exact? n) (<= #x-800000 n #x7FFFFF))
    (raise-marshal-error "int3" n))
  (number->string n))

(define (marshal-int4 n)
  (unless (and (integer? n) (exact? n) (<= #x-80000000 n #x7FFFFFFF))
    (raise-marshal-error "int4" n))
  (number->string n))

(define (marshal-int8 n)
  (unless (and (integer? n) (exact? n) (<= #x-8000000000000000 n #x7FFFFFFFFFFFFFFF))
    (raise-marshal-error "int8" n))
  (number->string n))

(define (marshal-real n)
  (unless (real? n)
    (raise-marshal-error "real" n))
  (cond [(eqv? n +inf.0) "Infinity"]
        [(eqv? n -inf.0) "-Infinity"]
        [(eqv? n +nan.0) "NaN"]
        [else
         (number->string
          (exact->inexact n))]))

(define (marshal-decimal n)
  (define (dlog10 n)
    (inexact->exact (ceiling (/ (log n) (log 2)))))
  (cond [(not (real? n))
         (raise-marshal-error "numeric" n)]
        [(eqv? n +nan.0)
         "NaN"]
        [(or (eqv? n +inf.0) (eqv? n -inf.0))
         (raise-marshal-error "numeric" n)]
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

(define (marshal-bool v)
  (if v "t" "f"))

(define (marshal-date d)
  (srfi:date->string (sql-datetime->srfi-date d) "~Y-~m-~d"))

(define (marshal-time t)
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N"))

(define (marshal-time-tz t)
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N~z"))

(define (marshal-timestamp t)
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N"))

(define (marshal-timestamp-tz t)
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N~z"))
