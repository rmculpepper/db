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

(provide parse-string
         parse-char1
         parse-bytea
         parse-integer
         parse-real
         parse-decimal
         parse-boolean
         parse-date
         parse-time
         parse-time-tz
         parse-timestamp
         parse-timestamp-tz)

(define (parse-string s) s)

(define (parse-char1 s)
  (string-ref s 0))

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
                   (parse-error "bytea" s))
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
                   (parse-error "bytea" s))
                 (write-byte n out))])
        (loop)))
    (loop))
  (if (regexp-match? #rx"\\\\" s)
      (let ([out (open-output-bytes)]
            [in (open-input-string s)])
        (decode in out)
        (get-output-bytes out))
      (with-handlers ([exn:fail?
                       (lambda (e) (parse-error "bytea" s))])
        (string->bytes/latin-1 s))))

(define (parse-integer s)
  (or (string->number s)
      (parse-error "integer" s)))

(define (parse-real s)
  (cond [(string->number s) => exact->inexact]
        [(equal? s "NaN") +nan.0]
        [(equal? s "Infinity") +inf.0]
        [(equal? s "-Infinity") -inf.0]
        [else (parse-error "real" s)]))

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
        [else (parse-error "boolean" s)]))

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

;; ----------------------------------------

;; parse-error : string string -> (raises error)
(define (parse-error type rep)
  (error 'query* "internal error: cannot parse as SQL type ~s: ~e"
         type rep))

;; ========================================

#|
marshal-<type> : fsym index param-info datum -> string

Takes a racket datum and converts it into <type>'s text wire format.
No conversion may be passed sql-null.
|#

(provide marshal-string
         marshal-ascii-string
         marshal-char1
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
         marshal-timestamp-tz

         marshal-error
         make-default-marshal)

(define (marshal-string f i pi s)
  (unless (string? s)
    (marshal-error f i pi "string" s))
  s)

(define (marshal-ascii-string f i pi s)
  (unless (string? s)
    (marshal-error f i pi "ascii-string" s))
  (for ([i (in-range (string-length s))])
    (unless (<= 0 (char->integer (string-ref s i)) 127)
      (marshal-error f i pi "ascii-string" s)))
  s)

(define (marshal-char1 f i pi c)
  (unless (and (char? c) (< (char->integer c) 128))
    (marshal-error f i pi "char1"))
  (string c))

(define (marshal-bytea f i pi s)
  (unless (bytes? s)
    (marshal-error f i pi "bytea" s))
  (let ([in (open-input-bytes s)]
        [out (open-output-string)])
    (encode in out)
    (get-output-string out)))

;; encode : input-port output-port boolean -> void
(define (encode in out)
  (define (loop)
    (let ([next-byte (read-byte in)])
      (cond [(eof-object? next-byte)
             (void)]
            [(= next-byte (char->integer #\\))
             (write-char #\\ out)
             (write-char #\\ out)
             (loop)]
            [(= next-byte 0)
             (write-char #\\ out)
             (write-string "000" out)
             (loop)]
            [(> next-byte 127)
             (write-char #\\ out)
             (let ([ns (number->string next-byte 8)])
               (write-string "000" out (string-length ns) 3)
               (write-string ns out))
             (loop)]
            [else
             (write-byte next-byte out)
             (loop)])))
  (loop))

(define (marshal-integer f i pi n)
  (unless (exact-integer? n)
    (marshal-error f i pi "integer" n))
  (number->string n))

(define (marshal-int* f i pi n type min max)
  (unless (and (exact-integer? n) (<= min n max))
    (marshal-error f i pi type n))
  (number->string n))

(define (marshal-int1 f i pi n)
  (marshal-int* f i pi n "int1" #x-80 #x7F))

(define (marshal-int2 f i pi n)
  (marshal-int* f i pi n "int2" #x-8000 #x7FFF))

(define (marshal-int3 f i pi n)
  (marshal-int* f i pi n "int3" #x-800000 #x7FFFFF))

(define (marshal-int4 f i pi n)
  (marshal-int* f i pi n "int4" #x-80000000 #x7FFFFFFF))

(define (marshal-int8 f i pi n)
  (marshal-int* f i pi n "int8" #x-8000000000000000 #x7FFFFFFFFFFFFFFF))

(define (marshal-real f i pi n)
  (unless (real? n)
    (marshal-error f i pi "real" n))
  (cond [(eqv? n +inf.0) "Infinity"]
        [(eqv? n -inf.0) "-Infinity"]
        [(eqv? n +nan.0) "NaN"]
        [else
         (number->string
          (exact->inexact n))]))

(define (marshal-decimal f i pi n)
  (define (dlog10 n)
    (inexact->exact (ceiling (/ (log n) (log 2)))))
  (cond [(not (real? n))
         (marshal-error f i pi "numeric" n)]
        [(eqv? n +nan.0)
         "NaN"]
        [(or (eqv? n +inf.0) (eqv? n -inf.0))
         (marshal-error f i pi "numeric" n)]
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

(define (marshal-bool f i pi v)
  (if v "t" "f"))

(define (marshal-date f i pi d)
  (srfi:date->string (sql-datetime->srfi-date d) "~Y-~m-~d"))

(define (marshal-time f i pi t)
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N"))

(define (marshal-time-tz f i pi t)
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N~z"))

(define (marshal-timestamp f i pi t)
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N"))

(define (marshal-timestamp-tz f i pi t)
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N~z"))

;; ----------------------------------------

;; marshal-error : string datum -> (raises error)
(define (marshal-error f i pi type datum)
  (error f "cannot marshal as SQL type ~s: ~e"
         type datum))

;; make-default-marshal : Type -> datum -> string
(define ((make-default-marshal type) f i pi datum)
  (cond [(string? datum) datum]
        [else (marshal-error f i pi type datum)]))
