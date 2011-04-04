;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require "message.rkt"
         "../generic/exceptions.rkt")
(provide raise-backend-error)

;; raise-backend-error : symbol ErrorPacket -> raises exn
(define (raise-backend-error function r)
  (define code (error-packet-sqlstate r))
  (define message (error-packet-message r))
  (define props (list (cons 'errno (error-packet-errno r))
                      (cons 'code code)
                      (cons 'message message)))
  (internal-raise-backend-error function code message props))
