;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 7))
         scheme/class
         scheme/unit
         (prefix-in srfi: srfi/19)
         "../generic/main.ss"
         "../generic/sql-format.ss"
         "../generic/signatures.ss"
         "config.ss")
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
             (test-case (format "unsupported: ~s" type))))]))

  (define-syntax check-roundtrip
    (syntax-rules ()
      [(check-roundtrip c type expr)
       (check-roundtrip c type expr check-equal?)]
      [(check-roundtrip c type expr check)
       (begin
         (let ([value expr])
           (when #t
             (check (let ([q (format-sql c "select ~a" [type value])])
                      (send c query-value q))
                    value))
           (when #f ;; only valid Postgreql syntax!
             (check (let ([q (format "select $1::~a"
                                     (send dbsystem typealias->type 'type))])
                      ((send c prepare-query-value q) value))
                    value))))]))

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
             (check-roundtrip c float +inf.0)
             (check-roundtrip c float -inf.0)
             (check-roundtrip c float +nan.0))))
        (type-test-case 'numeric
          (call-with-connection
           (lambda (c)
             (check-roundtrip c numeric 12345678901234567890)
             (check-roundtrip c numeric #e1234567890.0987654321)
             (check-roundtrip c numeric +nan.0))))
        (type-test-case 'varchar
          (call-with-connection
           (lambda (c)
             (check-roundtrip c varchar "this is the time to remember")
             (check-roundtrip c varchar "that's the way it is")
             (check-roundtrip c varchar (string #\\))
             (check-roundtrip c varchar (string #\'))
             (check-roundtrip c varchar (string #\\ #\'))
             (check-roundtrip c varchar "λ the ultimate")
             (check-roundtrip c varchar
                              (list->string
                               (build-list 800
                                           (lambda (n)
                                             (integer->char (add1 n)))))))))
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

;; OLD TEST CASES
#|
  ;; POSTGRESQL-specific tests

  (define postgresql-test
    (test-suite "SQL types"
      (test-suite "Parsing"
        (test-case "Parse boolean"
          (check-eq? (sql-parse 'bool "t") #t)
          (check-eq? (sql-parse 'bool "f") #f)
          (check-exn exn? (lambda () (sql-parse 'bool "g"))))
        (test-case "Parse integer"
          (check-equal? (sql-parse 'int4 "0") 0)
          (check-equal? (sql-parse 'int4 "17") 17)
          (check-exn exn? (lambda () (sql-parse 'int4 "")))
          (check-exn exn? (lambda () (sql-parse 'int4 "alpha"))))
        (test-case "Parse float"
          (check-equal? (sql-parse 'float4 "0.0") 0.0)
          (check-equal? (sql-parse 'float4 "17.123") 17.123)
          (check-exn exn? (lambda () (sql-parse 'float4 "")))
          (check-exn exn? (lambda () (sql-parse 'float4 "alpha"))))
        (test-case "Parse date"
          (check-equal? (sql-parse 'date "1980-08-17")
                        (make-sql-date 1980 08 17)))
        (test-case "Parse time"
          (check-equal? (sql-parse 'time "12:34:56")
                        (make-sql-time 12 34 56 0 #f))
          (check-equal? (sql-parse 'time "12:34:56.789")
                        (make-sql-time 12 34 56 789000000 #f))
          (check-equal? (sql-parse 'time "12:34:56.000789")
                        (make-sql-time 12 34 56 000789000 #f)))
        (test-case "Parse timetz"
          (check-equal? (sql-parse 'timetz "12:34:56+0123")
                        (make-sql-time 12 34 56 0 4980))
          (check-equal? (sql-parse 'timetz "12:34:56.789+0123")
                        (make-sql-time 12 34 56 789000000 4980))
          (check-equal? (sql-parse 'timetz "12:34:56.000789-0123")
                        (make-sql-time 12 34 56 000789000 -4980)))
        (test-case "Parse timestamp"
          (check-equal?
           (sql-parse 'timestamp "1980-08-17 12:34:56")
           (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
          (check-equal?
           (sql-parse 'timestamp "1980-08-17 12:34:56.123")
           (make-sql-timestamp 1980 08 17 12 34 56 123000000 #f))
          (check-equal?
           (sql-parse 'timestamp "1980-08-17 12:34:56.000123")
           (make-sql-timestamp 1980 08 17 12 34 56 000123000 #f)))
        (test-case "Parse timestamptz"
          (check-equal?
           (sql-parse 'timestamptz "1980-08-17 12:34:56+0123")
           (make-sql-timestamp 1980 08 17 12 34 56 0 4980))
          (check-equal?
           (sql-parse 'timestamptz "1980-08-17 12:34:56.123+0123")
           (make-sql-timestamp 1980 08 17 12 34 56 123000000 4980))
          (check-equal?
           (sql-parse 'timestamptz "1980-08-17 12:34:56.000123-0123")
           (make-sql-timestamp 1980 08 17 12 34 56 000123000 -4980))))

      (test-suite "Roundtrip"
        (test-case "boolean"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c bool #t)
             (check-roundtrip c bool #f))))
        (test-case "bytea"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c bytea #"this is the time to remember")
             (check-roundtrip c bytea #"that's the way it is")
             (check-roundtrip c bytea (list->bytes (build-list 256 values))))))
        (test-case "numbers"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c int 5)
             (check-roundtrip c int -1)
             (check-roundtrip c int #x7FFFFF)
             (check-roundtrip c int #x-800000)
             (check-roundtrip c float 1.0)
             (check-roundtrip c float 1.1)
             (check-roundtrip c float -5.8)
             (check-roundtrip c float +inf.0)
             (check-roundtrip c float -inf.0)
             (check-roundtrip c float +nan.0))))
        (test-case "numeric"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c numeric 12345678901234567890)
             (check-roundtrip c numeric #e1234567890.0987654321)
             (check-roundtrip c numeric +nan.0))))
        (test-case "strings"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c text "this is the time to remember")
             (check-roundtrip c text "that's the way it is")
             (check-roundtrip c text (string #\\))
             (check-roundtrip c text (string #\'))
             (check-roundtrip c text (string #\\ #\'))
             (check-roundtrip c text "λ the ultimate")
             (check-roundtrip c text (list->string
                                      (build-list 800
                                                  (lambda (n)
                                                    (integer->char (add1 n)))))))))
        (test-case "date"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c date (make-sql-date 1980 08 17)))))
        (test-case "time"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c time (make-sql-time 12 34 56 0 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 123456000 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 100000000 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 000001000 #f)))))
        (test-case "timetz"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timetz (make-sql-time 12 34 56 0 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 123456000 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 100000000 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 000001000 3600)))))
        (test-case "timestamp"
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
        (test-case "timestamptz"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timestamptz
                              (make-sql-timestamp 1980 08 17 12 34 56 0 3600)
                              check-timestamptz-equal?)
             (check-roundtrip c timestamptz
                              (make-sql-timestamp 1980 08 17 12 34 56 123456000 3600)
                              check-timestamptz-equal?)
             (check-roundtrip c timestamptz
                              (make-sql-timestamp 1980 08 17 12 34 56 100000000 3600)
                              check-timestamptz-equal?)
             (check-roundtrip c timestamptz
                              (make-sql-timestamp 1980 08 17 12 34 56 000001000 3600)
                              check-timestamptz-equal?))))
        )))
|#
