;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/unit
         "../base.rkt"
         (only-in "../private/generic/interfaces.rkt" connection<%>))
(provide database^
         test^
         config^
         config@)

(define-signature database^
  (dbtestname
   connect
   dbsys
   dbflags))

(define-signature test^ (test))
(define-signature config^
  (connect-for-test
   connect-and-setup
   call-with-connection
   (define-syntaxes (with-connection)
     (syntax-rules ()
       [(with-connection c . body)
        (call-with-connection (lambda (c) . body))]))
   test-data
   set-equal?
   sql
   NOISY?
   TESTFLAGS
   ANYFLAGS))

(define-unit config@
  (import database^)
  (export config^)

  (define NOISY? #f)

  (define (connect-for-test)
    (connect))

  (define test-data
    '((0 "nothing")
      (1 "unity")
      (2 "the loneliest number since the number one")
      (4 "four")
      (5 "five")
      (6 "half a dozen")))

  (define (connect-and-setup)
    (let [(cx (connect-for-test))]
      (query-exec cx
         "create temporary table the_numbers (N integer primary key, descr varchar(80))")
      (for-each (lambda (p)
                  (query-exec cx
                              (format "insert into the_numbers values (~a, '~a')"
                                      (car p) (cadr p))))
                test-data)
      cx))

  ;; set-equal? : ('a list) ('a list) -> boolean
  (define (set-equal? a b)
    (and (andmap (lambda (xa) (member xa b)) a)
         (andmap (lambda (xb) (member xb a)) b)
         #t))

  (define (call-with-connection f)
    (let [(c (connect-and-setup))]
      (dynamic-wind void
                    (lambda () (f c))
                    (lambda () (disconnect c)))))

  (define (sql str)
    (case dbsys
      ((postgresql) str)
      ((mysql sqlite3 odbc) (regexp-replace* #rx"\\$[0-9]" str "?"))
      (else (error 'sql "unsupported dbsystem: ~e" dbsys))))

  ;; Flags = dbflags U dbsys

  ;; Returns #t if all are set.
  (define (TESTFLAGS . xs)
    (for/and ([x xs])
      (or (eq? x dbsys)
          (and (member x dbflags) #t))))

  ;; Returns #t if any are set.
  (define (ANYFLAGS . xs)
    (for/or ([x xs])
      (or (eq? x dbsys)
          (and (member x dbflags) #t)))))
