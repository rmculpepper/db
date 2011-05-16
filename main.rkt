;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/runtime-path
         racket/promise
         racket/contract
         "base.rkt")
(provide (all-from-out "base.rkt"))

;; Lazy instantiation 

(define-syntax-rule (define-lazy-functions modpath fun ...)
  (begin (define-runtime-module-path-index mpi modpath)
         (deflazyfun mpi fun) ...))
(define-syntax-rule (deflazyfun mpi fun)
  (begin (define fun-p (delay (dynamic-require mpi 'fun)))
         (define fun
           (wrap-kw-fun
            (make-keyword-procedure
             (lambda (kws kwargs . args)
               (keyword-apply (force fun-p) kws kwargs args)))))))

;; Note: bug in contracts applied to result of make-keyword-procedure
;; Workaround: reduce kw-arity to known kws

(define known-keywords
  (sort '(#:user #:database #:password #:server #:port #:socket
          #:allow-cleartext-password? #:ssl #:ssl-encrypt
          #:notice-handler #:notification-handler
          #:mode #:dsn #:strict-parameter-types?)
        keyword<?))

(define (wrap-kw-fun proc)
  (procedure-reduce-keyword-arity
   proc (procedure-arity proc) '() known-keywords))

;; ----

(define-lazy-functions "private/postgresql/main.rkt"
  postgresql-connect
  postgresql-guess-socket-path)

(define-lazy-functions "private/mysql/main.rkt"
  mysql-connect
  mysql-guess-socket-path)

(define-lazy-functions "private/sqlite3/main.rkt"
  sqlite3-connect)

(define-lazy-functions "private/odbc/main.rkt"
  odbc-connect
  odbc-driver-connect
  odbc-data-sources
  odbc-drivers)

(provide/contract
 ;; Duplicates contracts at postgresql.rkt
 [postgresql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? false/c)
        #:server (or/c string? false/c)
        #:port (or/c exact-positive-integer? false/c)
        #:socket (or/c path-string? false/c)
        #:allow-cleartext-password? boolean?
        #:ssl (symbols 'yes 'no 'optional)
        #:ssl-encrypt (symbols 'sslv2 'sslv3 'sslv2-or-v3)
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:notification-handler (or/c 'output 'error output-port? procedure?))
       any/c)]
 [postgresql-guess-socket-path
  (-> path-string?)]

 ;; Duplicates contracts at mysql.rkt
 [mysql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? false/c)
        #:server (or/c string? false/c)
        #:port (or/c exact-positive-integer? false/c)
        #:socket (or/c path-string? false/c)
        #:allow-cleartext-password? boolean?)
       any/c)]
 [mysql-guess-socket-path
  (-> path-string?)]

 ;; Duplicates contracts at sqlite3.rkt
 [sqlite3-connect
  (->* (#:database (or/c path-string? 'memory 'temporary))
       (#:mode (or/c 'read-only 'read/write 'create))
       any/c)]

 ;; Duplicates contracts at odbc.rkt
 [odbc-connect
  (->* ()
       (#:dsn (or/c string? #f)
        #:database (or/c string? #f)
        #:user (or/c string? #f)
        #:password (or/c string? #f)
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:strict-parameter-types? boolean?)
       connection?)]
 [odbc-driver-connect
  (->* (string?)
       (#:notice-handler (or/c 'output 'error output-port? procedure?)
        #:strict-parameter-types? boolean?)
       connection?)]
 [odbc-data-sources
  (-> (listof (list/c string? string?)))]
 [odbc-drivers
  (-> (listof (cons/c string? any/c)))])
