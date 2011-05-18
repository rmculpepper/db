;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         "base.rkt"
         "private/odbc/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [odbc-connect
  (->* ()
       (#:dsn (or/c string? #f)
        #:database (or/c string? #f)
        #:user (or/c string? #f)
        #:password (or/c string? #f)
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:strict-parameter-types? boolean?
        #:character-mode (or/c 'wchar 'utf-8 'latin-1))
       connection?)]
 [odbc-driver-connect
  (->* (string?)
       (#:notice-handler (or/c 'output 'error output-port? procedure?)
        #:strict-parameter-types? boolean?
        #:character-mode (or/c 'wchar 'utf-8 'latin-1))
       connection?)]
 [odbc-data-sources
  (-> (listof (list/c string? string?)))]
 [odbc-drivers
  (-> (listof (cons/c string? any/c)))])
