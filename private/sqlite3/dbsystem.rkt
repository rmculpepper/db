;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "../generic/query.rkt"
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "ffi-constants.rkt")
(provide dbsystem)

(define sqlite3-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'sqlite3)
    (define/public (get-known-types) '())
    (define/public (has-support? x) #f)
    (define/public (typeids->types typeids) typeids)

    (define/public (get-parameter-handlers param-infos)
      (map (lambda (param-info) check-param)
           param-infos))

    (define/public (get-result-handlers result-infos)
      (error 'get-result-handlers "unsupported"))

    (super-new)))

(define dbsystem
  (new sqlite3-dbsystem%))

;; ========================================

(define (check-param param)
  (unless (or (real? param)
              (string? param)
              (bytes? param))
    ;; FIXME: need fsym propagated
    (error 'bind* "cannot convert to SQLite value: ~e" param))
  param)
