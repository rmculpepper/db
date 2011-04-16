;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         "base.rkt"
         "private/mysql/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [mysql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? false/c)
        #:server (or/c string? false/c)
        #:port (or/c exact-positive-integer? false/c)
        #:socket (or/c path-string? false/c))
       any/c)]
 [mysql-guess-socket-path
  (-> path-string?)])
