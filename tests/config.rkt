;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/unit
         "../base.rkt")
(provide database^
         test^
         config^
         config@)

(define-signature database^
  (connect
   dbsystem))

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
   set-equal?))

(define-unit config@
  (import database^)
  (export config^)

  (define (connect-for-test)
    (case (dbsystem-name dbsystem)
      ((postgresql)
       (connect #:user (getenv "DBUSER")
                #:database (or (getenv "DBDB") (getenv "DBUSER"))
                #:password (getenv "DBPASSWORD")
                #:notice-handler void))
      ((mysql)
       (connect #:user (getenv "DBUSER")
                #:database (or (getenv "DBDB") (getenv "DBUSER"))
                #:password (getenv "DBPASSWORD")))
      ((sqlite3)
       (connect #:database (or 'memory)))
      ((odbc)
       (connect #:database (or (getenv "DBODBC"))))
      (else
       (error 'connect-for-test "unknown database system: ~e" dbsystem))))

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
                    (lambda () (disconnect c))))))
