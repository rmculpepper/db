;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

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
