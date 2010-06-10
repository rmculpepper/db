;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         racket/unit
         "../generic/signatures.rkt")
(provide test^
         config^
         config@)

(define-signature test^ (test))
(define-signature config^
  (connect-for-test
   connect-and-setup
   call-with-connection
   (define-syntaxes (with-connection)
     (syntax-rules ()
       [(with-connection c . body)
        (call-with-connection (lambda (c) . body))]))
   testing-connection-mixin
   test-data
   set-equal?))

(define-unit config@
  (import database^)
  (export config^)

  (define testing-connection-mixin (make-parameter values))

  (define (connect-for-test)
    (connect #:user (getenv "DBUSER")
             #:database (or (getenv "DBDB") (getenv "DBUSER"))
             #:password (getenv "DBPASSWORD")
             #:mixin (testing-connection-mixin)))

  (define test-data
    '((0 "nothing")
      (1 "unity")
      (2 "the loneliest number since the number one")
      (4 "four")
      (5 "five")
      (6 "half a dozen")))

  (define (connect-and-setup)
    (let [(cx (connect-for-test))]
      (send cx query-exec
            "create temporary table the_numbers (N integer primary key, description text)")
      (for-each (lambda (p)
                  (send cx query-exec
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
                    (lambda () (send c disconnect))))))
