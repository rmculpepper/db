;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require rackunit
         rackunit/gui
         racket/unit
         "../private/generic/functions.rkt"
         "../private/generic/signatures.rkt"
         "../private/postgresql/unit.rkt"
         "../private/mysql/unit.rkt"
         "../private/sqlite3/unit.rkt"
         "../private/odbc/unit.rkt"
         "config.rkt"
         "connection.rkt"
         "query.rkt"
         "sql-types.rkt"
         "concurrent.rkt")

(define-unit all-tests@
  (import database^
          (tag connect (prefix connect: test^))
          (tag query (prefix query: test^))
          (tag sql-types (prefix sql-types: test^))
          (tag concurrent (prefix concurrent: test^)))
  (export test^)

  (define test
    (make-test-suite
     (format "~a tests" (dbsystem-name dbsystem))
     (list connect:test
           query:test
           sql-types:test
           concurrent:test))))

(define (specialize-test db@)
  (compound-unit
    (import)
    (export ALL-TESTS)
    (link (((DB : database^)) db@)
          (((CONFIG : config^)) config@ DB)
          (((CONNECT-TEST : test^)) connection@ CONFIG)
          (((QUERY-TEST : test^)) query@ DB CONFIG)
          (((SQL-TYPES-TEST : test^)) sql-types@ CONFIG DB)
          (((CONCURRENT-TEST : test^)) concurrent@ CONFIG DB)
          (((ALL-TESTS : test^)) all-tests@
                                 DB
                                 (tag connect CONNECT-TEST)
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

(define-values/invoke-unit (specialize-test odbc@)
  (import)
  (export (prefix odbc: test^)))

#|
;; Normal testing:
(putenv "DBUSER" "ryan")  ;; or whoever you are
(putenv "DBDB" "ryan")    ;; or any db that exists
(putenv "DBPASSWORD" ???)
(test/gui postgresql:test)
(test/gui mysql:test)

;; sqlite3 test just uses 'memory
(test/gui sqlite3:test)

;; ODBC testing:
(putenv "DBDB" <data-source-name>")
(test/gui odbc:test)
|#

(define-syntax-rule (setup-debug db@ c)
  (begin (define-values/invoke-unit db@ (import) (export database^))
         (define-values/invoke-unit config@ (import database^) (export config^))
         (define c (connect-and-setup))))
