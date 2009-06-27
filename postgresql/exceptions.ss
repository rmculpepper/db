;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require "msg.ss")
(provide (struct-out exn:fail:backend)
         raise-backend-error)

;; exn:fail:backend
;; Represents an ErrorResponse sent by the backend.
(define-struct (exn:fail:backend exn:fail:user) (properties))

;; raise-backend-error : symbol ErrorResponse -> raises exn
(define (raise-backend-error function r)
  (define code (cdr (assq 'code (ErrorResponse-properties r))))
  (define message (cdr (assq 'message (ErrorResponse-properties r))))
  (raise 
   (make-exn:fail:backend
    (string-append (if function
                       (string-append (symbol->string function) ": ")
                       "")
                   message
                   " (SQL code " code ")")
    (current-continuation-marks)
    (ErrorResponse-properties r))))
