;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require rackunit
         racket/class
         racket/unit
         (prefix-in srfi: srfi/19)
         "../generic/main.rkt"
         "../generic/signatures.rkt"
         "config.rkt")
(provide sql-types-test@)

(define-unit sql-types-test@
  (import config^ database^)
  (export test^)

  (define-syntax type-test-case
    (syntax-rules ()
      [(type-test-case type-expr . contents)
       (let ([type type-expr])
         (if (memq type (send dbsystem get-known-types))
             (test-case (format "~s" type) . contents)
             (test-case (format "unsupported: ~s" type) (void))))]))

  (define-syntax check-roundtrip
    (syntax-rules ()
      [(check-roundtrip c type expr)
       (check-roundtrip c type expr check-equal?)]
      [(check-roundtrip c type expr check)
       (begin
         (let ([value expr])
           (case (send dbsystem get-short-name)
             ((postgresql)
              ;; only valid Postgreql syntax!
              (check (let ([q (format "select $1::~a"
                                      (send dbsystem typealias->type 'type))])
                       ((send c prepare-query-value q) value))
                     value))
             ((mysql)
              (when (eq? 'type 'varchar)
                (check ((send c prepare-query-value "select ?") value) value))))))]))

  (define (check-string-length c value len)
    (define psql
      (case (send dbsystem get-short-name)
        ((postgresql)
         "select length($1)")
        ((mysql)
         "select char_length(?)")))
    (when (string? psql)
      (check-equal? ((send c prepare-query-value psql) value)
                    (string-length value))))

  (define (check-timestamptz-equal? a b)
    (check srfi:time=?
           (srfi:date->time-utc (sql-datetime->srfi-date a))
           (srfi:date->time-utc (sql-datetime->srfi-date b))))

  (define (sql-parse typealias s)
    (define type (send dbsystem typealias->type typealias))
    (define reader (send dbsystem get-type-reader type))
    (unless reader
      (error 'sql-parse "no reader for type: ~s (~s)" typealias type))
    (reader s))

  (define (sql-marshal typealias s)
    (define type (send dbsystem typealias->type typealias))
    (define writer (send dbsystem get-type-writer type))
    (unless writer
      (error 'sql-marshal "no writer for type: ~s (~s)" typealias type))
    (writer s))

  (define (supported? option)
    (send dbsystem has-support? option))

  (define test
    (test-suite "SQL types"
      (test-suite "Parsing"
        (type-test-case 'boolean
          (check-eq? (sql-parse 'boolean "t") #t)
          (check-eq? (sql-parse 'boolean "f") #f)
          (check-exn exn? (lambda () (sql-parse 'bool "g"))))
        (type-test-case 'varchar
          (check-equal? (sql-parse 'varchar "abc") "abc")
          (check-equal? (sql-parse 'varchar "") ""))
        (type-test-case 'integer
          (check-equal? (sql-parse 'integer "0") 0)
          (check-equal? (sql-parse 'integer "17") 17)
          (check-exn exn? (lambda () (sql-parse 'integer "")))
          (check-exn exn? (lambda () (sql-parse 'integer "alpha"))))
        (type-test-case 'real
          (check-equal? (sql-parse 'real "0.0") 0.0)
          (check-equal? (sql-parse 'real "17.123") 17.123)
          (check-exn exn? (lambda () (sql-parse 'real "")))
          (check-exn exn? (lambda () (sql-parse 'real "alpha"))))
        (type-test-case 'date
          (check-equal? (sql-parse 'date "1980-08-17")
                        (make-sql-date 1980 08 17)))
        (type-test-case 'time
          (check-equal? (sql-parse 'time "12:34:56")
                        (make-sql-time 12 34 56 0 #f))
          (check-equal? (sql-parse 'time "12:34:56.789")
                        (make-sql-time 12 34 56 789000000 #f))
          (check-equal? (sql-parse 'time "12:34:56.000789")
                        (make-sql-time 12 34 56 000789000 #f)))
        (type-test-case 'timetz
          (check-equal? (sql-parse 'timetz "12:34:56+0123")
                        (make-sql-time 12 34 56 0 4980))
          (check-equal? (sql-parse 'timetz "12:34:56.789+0123")
                        (make-sql-time 12 34 56 789000000 4980))
          (check-equal? (sql-parse 'timetz "12:34:56.000789-0123")
                        (make-sql-time 12 34 56 000789000 -4980)))
        (type-test-case 'timestamp
          (check-equal?
           (sql-parse 'timestamp "1980-08-17 12:34:56")
           (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
          (check-equal?
           (sql-parse 'timestamp "1980-08-17 12:34:56.123")
           (make-sql-timestamp 1980 08 17 12 34 56 123000000 #f))
          (check-equal?
           (sql-parse 'timestamp "1980-08-17 12:34:56.000123")
           (make-sql-timestamp 1980 08 17 12 34 56 000123000 #f)))
        (type-test-case 'timestamp-with-time-zone
          (check-equal?
           (sql-parse 'timestamp-with-time-zone "1980-08-17 12:34:56+0123")
           (make-sql-timestamp 1980 08 17 12 34 56 0 4980))
          (check-equal?
           (sql-parse 'timestamp-with-time-zone "1980-08-17 12:34:56.123+0123")
           (make-sql-timestamp 1980 08 17 12 34 56 123000000 4980))
          (check-equal?
           (sql-parse 'timestamp-with-time-zone "1980-08-17 12:34:56.000123-0123")
           (make-sql-timestamp 1980 08 17 12 34 56 000123000 -4980)))
        (type-test-case 'numeric
          (check-equal? (sql-parse 'numeric "12345678901234567890")
                        12345678901234567890)
          (check-equal? (sql-parse 'numeric "-12345678901234567890")
                        -12345678901234567890))
        )

      (test-suite "String escaping"
        (test-case "tricky varchar"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c varchar (string #\\))
             (check-roundtrip c varchar (string #\\ #\\))
             (check-roundtrip c varchar (string #\\ #\')))))
        (test-case "tricky varchar by length"
          (call-with-connection
           (lambda (c)
             ;; backslash = 92
             ;; apostrophe = 39
             (check-string-length c (string #\\) 1)
             (check-string-length c (string #\\ #\\) 2)
             (check-string-length c (string #\') 1)
             (check-string-length c (string #\λ) 1)))))

      (test-suite "Roundtrip"
        (type-test-case 'boolean
          (call-with-connection
           (lambda (c)
             (check-roundtrip c bool #t)
             (check-roundtrip c bool #f))))
        (type-test-case 'bytea
          (call-with-connection
           (lambda (c)
             (check-roundtrip c bytea #"this is the time to remember")
             (check-roundtrip c bytea #"that's the way it is")
             (check-roundtrip c bytea (list->bytes (build-list 256 values))))))
        (type-test-case 'integer
          (call-with-connection
           (lambda (c)
             (check-roundtrip c int 5)
             (check-roundtrip c int -1)
             (check-roundtrip c int #x7FFFFF)
             (check-roundtrip c int #x-800000))))
        (type-test-case 'real
          (call-with-connection
           (lambda (c)
             (check-roundtrip c float 1.0)
             (check-roundtrip c float 1.1)
             (check-roundtrip c float -5.8)
             (when (supported? 'real-infinities)
               (check-roundtrip c float +inf.0)
               (check-roundtrip c float -inf.0)
               (check-roundtrip c float +nan.0)))))
        (type-test-case 'numeric
          (call-with-connection
           (lambda (c)
             (check-roundtrip c numeric 12345678901234567890)
             (check-roundtrip c numeric #e1234567890.0987654321)
             (when (supported? 'numeric-infinities)
               (check-roundtrip c numeric +nan.0)))))
        (type-test-case 'varchar
          (call-with-connection
           (lambda (c)
             (check-roundtrip c varchar "this is the time to remember")
             (check-roundtrip c varchar "that's the way it is")
             (check-roundtrip c varchar (string #\\))
             (check-roundtrip c varchar (string #\'))
             (check-roundtrip c varchar (string #\\ #\'))
             (check-roundtrip c varchar "λ the ultimate")
             (check-roundtrip c varchar (make-string 800 #\a))
             (check-roundtrip 
              c varchar
              (string-append "αβψδεφγηιξκλμνοπρστθωςχυζ"
                             "अब्च्देघिज्क्ल्म्नोप्र्स्तुव्य्"
                             "شﻻؤيثبلاهتنمةىخحضقسفعرصءغئ"
                             "阿あでいおうわぁ"
                             "абцдефгхиклмнопљрстувњџзѕЋч"))
             ;; Following might not produce valid string (??)
             (when #t
               (check-roundtrip c varchar
                                (list->string
                                 (build-list 800
                                             (lambda (n)
                                               (integer->char (add1 n))))))))))
        (type-test-case 'date
          (call-with-connection
           (lambda (c)
             (check-roundtrip c date (make-sql-date 1980 08 17)))))
        (type-test-case 'time
          (call-with-connection
           (lambda (c)
             (check-roundtrip c time (make-sql-time 12 34 56 0 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 123456000 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 100000000 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 000001000 #f)))))
        (type-test-case 'timetz
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timetz (make-sql-time 12 34 56 0 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 123456000 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 100000000 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 000001000 3600)))))
        (type-test-case 'timestamp
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timestamp
                              (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
             (check-roundtrip c timestamp
                              (make-sql-timestamp 1980 08 17 12 34 56 123456000 #f))
             (check-roundtrip c timestamp
                              (make-sql-timestamp 1980 08 17 12 34 56 100000000 #f))
             (check-roundtrip c timestamp
                              (make-sql-timestamp 1980 08 17 12 34 56 000001000 #f)))))

        ;; Bizarrely, PostgreSQL converts timestamptz to a standard timezone
        ;; when returning them, but it doesn't for timetz.
        (type-test-case 'timestamp-with-time-zone
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timestamp-with-time-zone
                              (make-sql-timestamp 1980 08 17 12 34 56 0 3600)
                              check-timestamptz-equal?)
             (check-roundtrip c timestamp-with-time-zone
                              (make-sql-timestamp 1980 08 17 12 34 56 123456000 3600)
                              check-timestamptz-equal?)
             (check-roundtrip c timestamp-with-time-zone
                              (make-sql-timestamp 1980 08 17 12 34 56 100000000 3600)
                              check-timestamptz-equal?)
             (check-roundtrip c timestamp-with-time-zone
                              (make-sql-timestamp 1980 08 17 12 34 56 000001000 3600)
                              check-timestamptz-equal?))))
        ))))
