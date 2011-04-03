;; Copyright 2009-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/runtime-path
         racket/promise
         racket/contract
         "base.rkt")
(provide (all-from-out "base.rkt"))

;; Lazy instantiation 
;; FIXME: Turns into dynamic require of syntax-bound name (due to provide/contract)

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
          #:notice-handler #:notification-handler)
        keyword<?))

(define (wrap-kw-fun proc)
  (procedure-reduce-keyword-arity
   proc (procedure-arity proc) '() known-keywords))

;; ----

(define-lazy-functions "postgresql.rkt"
  postgresql-connect
  postgresql-guess-socket-path)

(define-lazy-functions "mysql.rkt"
  mysql-connect
  mysql-guess-socket-path)

(define-lazy-functions "sqlite3.rkt"
  sqlite3-connect)

(define-lazy-functions "odbc.rkt"
  odbc-connect
  odbc-driver-connect
  odbc-data-sources
  odbc-drivers)

(provide/contract
 ;; Duplicates contracts at db/postgresql/main.rkt
 [postgresql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? false/c)
        #:server (or/c string? false/c)
        #:port (or/c exact-positive-integer? false/c)
        #:socket (or/c string? path? false/c)
        #:allow-cleartext-password? boolean?
        #:ssl (symbols 'yes 'no 'optional)
        #:ssl-encrypt (symbols 'sslv2 'sslv3 'sslv2-or-v3)
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:notification-handler (or/c 'output 'error output-port? procedure?))
       any/c)]
 [postgresql-guess-socket-path
  (-> (or/c string? path?))]

 ;; Duplicates contracts at db/mysql/main.rkt
 [mysql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? false/c)
        #:server (or/c string? false/c)
        #:port (or/c exact-positive-integer? false/c)
        #:socket (or/c string? path? false/c)
        #:allow-cleartext-password? boolean?)
       any/c)]
 [mysql-guess-socket-path
  (-> (or/c string? path?))]

 ;; Duplicates contracts at db/sqlite3/main.rkt
 [sqlite3-connect
  (-> #:database (or/c string? path? bytes? 'memory 'temporary)
      any/c)]

 ;; Duplicates contracts at db/odbc/main.rkt
 [odbc-connect
  (->* (#:database string?)
       (#:user (or/c string? #f)
        #:password (or/c string? #f))
       connection?)]
 [odbc-driver-connect
  (-> string?
      connection?)]
 [odbc-data-sources
  (-> (listof (list/c string? string?)))]
 [odbc-drivers
  (-> (listof (cons/c string? any/c)))])
