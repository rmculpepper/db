;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         "base.rkt"
         "private/sqlite3/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [sqlite3-connect
  (->* (#:database (or/c path-string? 'memory 'temporary))
       (#:mode (or/c 'read-only 'read/write 'create))
       any/c)])
