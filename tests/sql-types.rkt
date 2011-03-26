;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/unit
(require rackunit
         racket/class
         (prefix-in srfi: srfi/19)
         "../generic/main.rkt"
         "../generic/sql-convert.rkt"
         "../generic/signatures.rkt"
         "config.rkt")
(import config^ database^)
(export test^)

(define parse/marshal-tests
  (test-suite "Parsing"
    (test-case "boolean"
      (check-eq? (parse-boolean "t") #t)
      (check-eq? (parse-boolean "f") #f)
      (check-exn exn? (lambda () (parse-boolean "g"))))
    (test-case "varchar"
      (check-equal? (parse-string "abc") "abc")
      (check-equal? (parse-string "") ""))
    (test-case "integer"
      (check-equal? (parse-integer "0") 0)
      (check-equal? (parse-integer "17") 17)
      (check-exn exn? (lambda () (parse-integer "")))
      (check-exn exn? (lambda () (parse-integer "alpha"))))
    (test-case "real"
      (check-equal? (parse-real "0.0") 0.0)
      (check-equal? (parse-real "17.123") 17.123)
      (check-exn exn? (lambda () (parse-real "")))
      (check-exn exn? (lambda () (parse-real "alpha"))))
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

;; ----------------------------------------

(define current-type (make-parameter #f))

(define-syntax-rule (type-test-case types . body)
  (type-test-case* types (lambda () . body)))

(define (type-test-case* types proc)
  (let* ([known-types (send dbsystem get-known-types)]
         [type (for/or ([type types])
                (and (member type known-types) type))])
    (if type
        (test-case (format "~s" type)
          (parameterize ((current-type type)) (proc)))
        (test-case (format "unsupported: ~s" types) (void)))))

(define (check-string-length c value len)
  (define psql
    (case (send dbsystem get-short-name)
      ((postgresql)
       "select length($1)")
      ((mysql)
       "select char_length(?)")))
  (when (string? psql)
    (check-equal? ((prepare-query-value c psql) value)
                  (string-length value))))

(define (check-timestamptz-equal? a b)
  (check srfi:time=?
         (srfi:date->time-utc (sql-datetime->srfi-date a))
         (srfi:date->time-utc (sql-datetime->srfi-date b))))

(define (supported? option)
  (send dbsystem has-support? option))

(define-syntax check-roundtrip
  (syntax-rules ()
    [(check-roundtrip c expr)
     (check-roundtrip c expr check-equal?)]
    [(check-roundtrip c expr check)
     (begin
       (let ([value expr])
         (case (send dbsystem get-short-name)
           ((postgresql)
            ;; only valid Postgreql syntax!
            (check (let ([q (format "select $1::~a" (current-type))])
                     ((prepare-query-value c q) value))
                   value))
           ((mysql)
            ;; FIXME: can do better once prepare supports types
            (when (eq? (current-type) 'varchar)
              (check ((prepare-query-value c "select ?") value) value))))))]))

(define string-tests
  (test-suite "String escaping"
    (test-case "tricky varchar"
      (parameterize ((current-type 'varchar))
        (call-with-connection
         (lambda (c)
           (check-roundtrip c (string #\\))
           (check-roundtrip c (string #\\ #\\))
           (check-roundtrip c (string #\\ #\'))))))
    (test-case "tricky varchar by length"
      (parameterize ((current-type 'varchar))
        (call-with-connection
         (lambda (c)
           ;; backslash = 92
           ;; apostrophe = 39
           (check-string-length c (string #\\) 1)
           (check-string-length c (string #\\ #\\) 2)
           (check-string-length c (string #\') 1)
           (check-string-length c (string #\λ) 1)))))))

(define roundtrip-tests
  (test-suite "Roundtrip"
    (type-test-case '(bool boolean)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c #t)
         (check-roundtrip c #f))))
    (type-test-case '(bytea)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c #"this is the time to remember")
         (check-roundtrip c #"that's the way it is")
         (check-roundtrip c (list->bytes (build-list 256 values))))))
    (type-test-case '(integer)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 5)
         (check-roundtrip c -1)
         (check-roundtrip c #x7FFFFF)
         (check-roundtrip c #x-800000))))
    (type-test-case '(real)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 1.0)
         (check-roundtrip c 1.1)
         (check-roundtrip c -5.8)
         (when (supported? 'real-infinities)
           (check-roundtrip c +inf.0)
           (check-roundtrip c -inf.0)
           (check-roundtrip c +nan.0)))))
    (type-test-case '(numeric)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 12345678901234567890)
         (check-roundtrip c #e1234567890.0987654321)
         (when (supported? 'numeric-infinities)
           (check-roundtrip c +nan.0)))))
    (type-test-case '(varchar)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c "this is the time to remember")
         (check-roundtrip c "that's the way it is")
         (check-roundtrip c (string #\\))
         (check-roundtrip c (string #\'))
         (check-roundtrip c (string #\\ #\'))
         (check-roundtrip c "λ the ultimate")
         (check-roundtrip c (make-string 800 #\a))
         (check-roundtrip c
          (string-append "αβψδεφγηιξκλμνοπρστθωςχυζ"
                         "अब्च्देघिज्क्ल्म्नोप्र्स्तुव्य्"
                         "شﻻؤيثبلاهتنمةىخحضقسفعرصءغئ"
                         "阿あでいおうわぁ"
                         "абцдефгхиклмнопљрстувњџзѕЋч"))
         ;; Following might not produce valid string (??)
         (when #t
           (check-roundtrip c
                            (list->string
                             (build-list 800
                                         (lambda (n)
                                           (integer->char (add1 n))))))))))
    (type-test-case '(date)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-date 1980 08 17)))))
    (type-test-case '(time)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-time 12 34 56 0 #f))
         (check-roundtrip c (make-sql-time 12 34 56 123456000 #f))
         (check-roundtrip c (make-sql-time 12 34 56 100000000 #f))
         (check-roundtrip c (make-sql-time 12 34 56 000001000 #f)))))
    (type-test-case '(timetz)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-time 12 34 56 0 3600))
         (check-roundtrip c (make-sql-time 12 34 56 123456000 3600))
         (check-roundtrip c (make-sql-time 12 34 56 100000000 3600))
         (check-roundtrip c (make-sql-time 12 34 56 000001000 3600)))))
    (type-test-case '(timestamp datetime)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 123456000 #f))
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 100000000 #f))
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 000001000 #f)))))
    ;; Bizarrely, PostgreSQL converts timestamptz to a standard timezone
    ;; when returning them, but it doesn't for timetz.
    (type-test-case '(timestamptz)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 0 3600)
                          check-timestamptz-equal?)
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 123456000 3600)
                          check-timestamptz-equal?)
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 100000000 3600)
                          check-timestamptz-equal?)
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 000001000 3600)
                          check-timestamptz-equal?))))))  

(define test
  (test-suite "SQL types"
    parse/marshal-tests
    string-tests
    roundtrip-tests))
