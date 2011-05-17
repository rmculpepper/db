;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/cmdline
         racket/file
         rackunit
         rackunit/text-ui
         racket/unit
         "../main.rkt"
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

Prefs file maps symbol => <conf>

<conf> ::= (profile <conf> ...)
         | (db <string> <connector> <flags> <args>)
         | (ref <symbol>)

<connector> ::= postgresql | mysql | sqlite3 | odbc
<flags> ::= (<datum> ...)
<args> ::= (<arg> ...)
<arg>  ::= <datum> | { <kw> <datum> }

Profiles are flattened, not hierarchical.
Flags are useful for, eg, indicating SQL dialect.

|#

;; ----------------------------------------

(define pref-file
  (make-parameter (build-path (find-system-path 'pref-dir) "ryanc-db-test.rktd")))

(define (get-dbconf name)
  (parse-dbconf
   (get-preference name
                   (lambda () (error 'get-dbconf "no such dbconf: ~e" name))
                   'timestamp
                   (pref-file))))

(define (put-dbconf name dbconf)
  (put-preferences (list name)
                   (list (dbconf->sexpr dbconf))
                   (lambda () (error 'put-dbconf "locked"))
                   (pref-file)))

(struct dbconf (name connector flags args) #:transparent)

(define (dbconf->sexpr x)
  (match x
    [(dbconf name connector flags args)
     `(db ,name ,connector ,flags
          ,(let ([pargs (car args)] [kwargs (cadr args)])
             (append pargs (apply append kwargs))))]))

(define-syntax-rule (expect name pred)
  (unless (pred name) (error 'parse "bad ~a: ~e" 'name name)))

;; parse-dbconf : sexpr -> (listof dbconf?)
(define (parse-dbconf x)
  (match x
    [(list 'profile dbconfs ...)
     (apply append (map parse-dbconf dbconfs))]
    [(list 'db name connector flags args)
     (expect name string?)
     (expect connector connector?)
     (expect flags list?)
     (expect args list?)
     (list (dbconf name connector flags (parse-args args)))]
    [(list 'ref conf-name)
     (expect conf-name symbol?)
     (get-dbconf conf-name)]))

(define (connector? x)
  (memq x '(postgresql mysql sqlite3 odbc)))

(define (parse-args x)
  (let loop ([x x] [pargs null] [kwargs null])
    (cond [(null? x)
           (list (reverse pargs)
                 (sort kwargs keyword<? #:key car))]
          [(keyword? (car x))
           (unless (pair? (cdr x)) (error 'parse "keyword without argument: ~a" (car x)))
           (loop (cddr x) pargs (cons (list (car x) (cadr x)) kwargs))]
          [else (loop (cdr x) (cons (car x) pargs) kwargs)])))

;; ----

(define (dbconf->unit x)
  (match x
    [(dbconf dbtestname dbsys dbflags dbargs)
     (let* ([pargs (car dbargs)]
            [kwargs (cadr dbargs)]
            [connector
             (case dbsys
               ((postgresql) postgresql-connect)
               ((mysql) mysql-connect)
               ((sqlite3) sqlite3-connect)
               ((odbc) odbc-connect))]
            [connect
             (lambda ()
               (keyword-apply connector (map car kwargs) (map cadr kwargs) pargs))])
       (unit-from-context database^))]))

;; ----

(define-unit db-test@
  (import database^
          (tag connect (prefix connect: test^))
          (tag query (prefix query: test^))
          (tag sql-types (prefix sql-types: test^))
          (tag concurrent (prefix concurrent: test^)))
  (export test^)
  (define test
    (make-test-suite
     (format "~a tests" dbtestname)
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
         (((CONNECT-TEST : test^)) db-connection@ CONFIG DB)
         (((QUERY-TEST : test^)) db-query@ CONFIG DB)
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

(define (odbc-test dsn [flags null])
  (specialize-test (dbconf->unit (dbconf dsn 'odbc flags `(() ((#:dsn ,dsn)))))))

(define generic-tests
  (make-test-suite "Generic tests (no db)"
    (list gen-sql-types:test)))

;; ----

(define (make-all-tests dbconfs generic?)
  (make-test-suite "All tests"
    (append (if generic? (list generic-tests) null)
            (for/list ([dbconf (in-list dbconfs)])
              (specialize-test (dbconf->unit dbconf))))))

;; ----

(define-syntax-rule (setup-debug db@ c)
  (begin (define-values/invoke-unit db@ (import) (export database^))
         (define-values/invoke-unit config@ (import database^) (export config^))
         (define c (connect-and-setup))))

;; ----------------------------------------

(define include-generic? #t)

(command-line
 #:once-each
 [("--no-generic") "Disable generic tests" (set! include-generic? #f)]
 [("-f" "--config-file") file  "Use configuration file" (pref-file file)]
 #:args labels
 (run-tests
  (make-all-tests (apply append (map get-dbconf (map string->symbol labels)))
                  include-generic?)))
