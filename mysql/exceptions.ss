;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require "message.ss"
         "../generic/exceptions.ss")
(provide raise-backend-error)

;; raise-backend-error : symbol ErrorPacket -> raises exn
(define (raise-backend-error function r)
  (define code (error-packet-sqlstate r))
  (define message (error-packet-message r))
  (define props (list (cons 'errno (error-packet-errno r))
                      (cons 'code code)
                      (cons 'message message)))
  (internal-raise-backend-error function code message props))
