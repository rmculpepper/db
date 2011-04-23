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
         "db-connection.rkt"
         "db-query.rkt"
         "db-sql-types.rkt"
         "db-concurrent.rkt"

         "gen-sql-types.rkt")
(provide (all-defined-out))

#|

RUNNING THE TESTS
-----------------

The following environment variables must be defined, and must be valid
for both PostgreSQL and MySQL:

  DBDB
  DBUSER
  DBPASSWORD

The following ODBC Data Sources must exist and must already contain
the user name and password (or not require it):

  test-pg      (driver = PostgreSQL Unicode)
  test-my      (driver = MySQL)
  test-sl      (driver = SQLite3)

|#

(define (db-unit connect dbsystem [dbdb #f])
  (let ([dbuser #f] [dbpassword #f])
    (unit-from-context database^)))

(define postgresql@ (db-unit postgresql-connect postgresql-dbsystem))
(define mysql@ (db-unit mysql-connect mysql-dbsystem))
(define sqlite3@ (db-unit sqlite3-connect sqlite3-dbsystem))
(define odbc@ (db-unit odbc-connect odbc-dbsystem))

(define odbc-pg@ (db-unit odbc-connect odbc-dbsystem "test-pg"))
(define odbc-my@ (db-unit odbc-connect odbc-dbsystem "test-my"))
(define odbc-sl@ (db-unit odbc-connect odbc-dbsystem "test-sl"))

(define-unit db-test@
  (import database^
          (tag connect (prefix connect: test^))
          (tag query (prefix query: test^))
          (tag sql-types (prefix sql-types: test^))
          (tag concurrent (prefix concurrent: test^)))
  (export test^)

  (define test
    (make-test-suite
     (format "~a~a tests"
             (dbsystem-name dbsystem)
             (if dbdb (format " (~a)" dbdb) ""))
     (list connect:test
           query:test
           sql-types:test
           concurrent:test))))

(define (specialize-test@ db@)
  (compound-unit
   (import)
   (export DB-TEST)
   (link (((DB : database^)) db@)
         (((CONFIG : config^)) config@ DB)
         (((CONNECT-TEST : test^)) db-connection@ CONFIG)
         (((QUERY-TEST : test^)) db-query@ DB CONFIG)
         (((SQL-TYPES-TEST : test^)) db-sql-types@ CONFIG DB)
         (((CONCURRENT-TEST : test^)) db-concurrent@ CONFIG DB)
         (((DB-TEST : test^)) db-test@
                               DB
                               (tag connect CONNECT-TEST)
                               (tag query QUERY-TEST)
                               (tag sql-types SQL-TYPES-TEST)
                               (tag concurrent CONCURRENT-TEST)))))

(define (specialize-test db@)
  (define-values/invoke-unit (specialize-test@ db@) (import) (export test^))
  test)

(define postgresql:test (specialize-test postgresql@))
(define mysql:test (specialize-test mysql@))
(define sqlite3:test (specialize-test sqlite3@))
(define odbc:test (specialize-test odbc@))

;; ----

(define all-tests
  (let ([opg:test (specialize-test odbc-pg@)]
        [omy:test (specialize-test odbc-my@)]
        [osl:test (specialize-test odbc-sl@)])
    (make-test-suite "All tests"
      (list (make-test-suite "Generic tests (no db)"
              (list gen-sql-types:test))
            postgresql:test
            mysql:test
            sqlite3:test
            opg:test
            omy:test
            osl:test))))

;; ----

(define-syntax-rule (setup-debug db@ c)
  (begin (define-values/invoke-unit db@ (import) (export database^))
         (define-values/invoke-unit config@ (import database^) (export config^))
         (define c (connect-and-setup))))
