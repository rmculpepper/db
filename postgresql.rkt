;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         "base.rkt"
         "private/postgresql/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [postgresql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? (list/c 'hash string?) #f)
        #:server (or/c string? #f)
        #:port (or/c exact-positive-integer? #f)
        #:socket (or/c path-string? #f)
        #:allow-cleartext-password? boolean?
        #:ssl (symbols 'yes 'no 'optional)
        #:ssl-encrypt (symbols 'sslv2 'sslv3 'sslv2-or-v3)
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:notification-handler (or/c 'output 'error output-port? procedure?))
       any/c)]
 [postgresql-guess-socket-path
  (-> path-string?)]
 [postgresql-password-hash
  (-> string? string? string?)])
