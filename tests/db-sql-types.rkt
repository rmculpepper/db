;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/unit
(require rackunit
         racket/class
         (prefix-in srfi: srfi/19)
         "../private/generic/main.rkt"
         "../private/generic/sql-convert.rkt"
         "config.rkt")
(import config^ database^)
(export test^)

(define dbsystem #f) ;; hack, set within test suite

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
    (case dbsys
      ((postgresql)
       "select length($1)")
      ((mysql)
       "select char_length(?)")
      ((sqlite3)
       "select length(?)")))
  (when (string? psql)
    (check-equal? (query-value c psql value)
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
            (check (let ([q (format "select $1::~a"
                                    (let ([t (current-type)])
                                      (case t
                                        ((double) "float8")
                                        (else t))))])
                     (query-value c q value))
                   value))
           ((mysql)
            ;; FIXME: can do better once prepare supports types
            (let ([stmt
                   (case (current-type)
                     ((varchar) "select cast(? as char)")
                     ;;((blob) "select cast(? as binary)")
                     ((integer) "select cast(? as signed integer)")
                     ((real) #f)
                     ((numeric) "select cast(? as decimal)")
                     ((date) "select cast(? as date)")
                     ((time) "select cast(? as time)")
                     ((datetime) "select cast(? as datetime)")
                     (else #f))])
              (when stmt
                (check (query-value c stmt value)
                       value)))))))]))

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
    (type-test-case '(bytea blob)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c #"this is the time to remember")
         (check-roundtrip c #"that's the way it is")
         (check-roundtrip c (list->bytes (build-list 256 values))))))

    (type-test-case '(smallint)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 5)
         (check-roundtrip c -1)
         (check-roundtrip c #x7FFF)
         (check-roundtrip c #x-8000))))
    (type-test-case '(integer)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 5)
         (check-roundtrip c -1)
         (check-roundtrip c #x7FFFFFFF)
         (check-roundtrip c #x-80000000))))
    (type-test-case '(bigint)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 5)
         (check-roundtrip c -1)
         (check-roundtrip c (sub1 (expt 2 63)))
         (check-roundtrip c (- (expt 2 63))))))

    (type-test-case '(real)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 1.0)
         (check-roundtrip c 1.5)
         (check-roundtrip c -5.5)
         (when (supported? 'real-infinities)
           (check-roundtrip c +inf.0)
           (check-roundtrip c -inf.0)
           (check-roundtrip c +nan.0)))))
    (type-test-case '(double)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 1.0)
         (check-roundtrip c 1.5)
         (check-roundtrip c -5.5)
         (check-roundtrip c 1.1)
         (check-roundtrip c -5.8)
         (when (supported? 'real-infinities)
           (check-roundtrip c +inf.0)
           (check-roundtrip c -inf.0)
           (check-roundtrip c +nan.0)))))

    (type-test-case '(numeric decimal)
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
    (call-with-connection (lambda (c) (set! dbsystem (connection-dbsystem c))))
    string-tests
    roundtrip-tests
    (set! dbsystem #f)))
