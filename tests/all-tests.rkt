;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require rackunit
         rackunit/gui
         racket/unit
         "../base.rkt"
         (only-in "../postgresql.rkt" postgresql-connect)
         (only-in "../private/postgresql/main.rkt" postgresql-dbsystem)
         (only-in "../mysql.rkt" mysql-connect)
         (only-in "../private/mysql/main.rkt" mysql-dbsystem)
         (only-in "../sqlite3.rkt" sqlite3-connect)
         (only-in "../private/sqlite3/main.rkt" sqlite3-dbsystem)
         (only-in "../odbc.rkt" odbc-connect)
         (only-in "../private/odbc/main.rkt" odbc-dbsystem)
         "config.rkt"
         "connection.rkt"
         "query.rkt"
         "sql-types.rkt"
         "concurrent.rkt")
(provide (all-defined-out))

(define (db-unit connect dbsystem)
  (unit-from-context database^))

(define postgresql@ (db-unit postgresql-connect postgresql-dbsystem))
(define mysql@ (db-unit mysql-connect mysql-dbsystem))
(define sqlite3@ (db-unit sqlite3-connect sqlite3-dbsystem))
(define odbc@ (db-unit odbc-connect odbc-dbsystem))

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
  (define-values/invoke-unit 
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
                                  (tag concurrent CONCURRENT-TEST))))
    (import)
    (export test^))
  test)

(define postgresql:test (specialize-test postgresql@))
(define mysql:test (specialize-test mysql@))
(define sqlite3:test (specialize-test sqlite3@))
(define odbc:test (specialize-test odbc@))

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
