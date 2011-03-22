;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require rackunit
         rackunit/gui
         racket/unit
         "../generic/signatures.rkt"
         "../postgresql/unit.rkt"
         "../mysql/unit.rkt"
         "../sqlite3/unit.rkt"
         "config.rkt"
         "connection.rkt"
         "sql-types.rkt"
         "concurrent.rkt")

(define-unit all-tests@
  (import (tag query (prefix query: test^))
          (tag sql-types (prefix sql-types: test^))
          (tag concurrent (prefix concurrent: test^)))
  (export test^)

  (define test
    (make-test-suite
     "All tests"
     (list query:test
           sql-types:test
           concurrent:test))))

(define (specialize-test db@)
  (compound-unit
    (import)
    (export ALL-TESTS)
    (link (((DB : database^)) db@)
          (((CONFIG : config^)) config@ DB)
          (((QUERY-TEST : test^)) query-test@ CONFIG)
          (((SQL-TYPES-TEST : test^)) sql-types-test@ CONFIG DB)
          (((CONCURRENT-TEST : test^)) concurrent-test@ CONFIG DB)
          (((ALL-TESTS : test^)) all-tests@
                                 (tag query QUERY-TEST)
                                 (tag sql-types SQL-TYPES-TEST)
                                 (tag concurrent CONCURRENT-TEST)))))

(define-values/invoke-unit (specialize-test postgresql@)
  (import)
  (export (prefix postgresql: test^)))

(define-values/invoke-unit (specialize-test mysql@)
  (import)
  (export (prefix mysql: test^)))

(define-values/invoke-unit (specialize-test sqlite3@)
  (import)
  (export (prefix sqlite3: test^)))

#|
;; Normal testing:
(putenv "DBUSER" "ryan")  ;; or whoever you are
(putenv "DBDB" "ryan")    ;; or any db that exists
(putenv "DBPASSWORD" ???)
(test/gui postgresql:test)
(test/gui mysql:test)
|#

(define-syntax-rule (setup-debug db@ c)
  (begin (define-values/invoke-unit db@ (import) (export database^))
         (define-values/invoke-unit config@ (import database^) (export config^))
         (define c (connect-and-setup))))
