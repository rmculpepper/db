;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require "msg.rkt"
         "../generic/exceptions.rkt")
(provide raise-backend-error)

;; raise-backend-error : symbol ErrorResponse -> raises exn
(define (raise-backend-error function r)
  (define props (ErrorResponse-properties r))
  (define code (cdr (assq 'code props)))
  (define message (cdr (assq 'message props)))
  (internal-raise-backend-error function code message props))
