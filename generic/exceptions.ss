;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(provide (struct-out exn:fail:backend)
         internal-raise-backend-error)

;; exn:fail:backend
;; Represents an error sent by the backend.
(define-struct (exn:fail:backend exn:fail:user) (properties))

;; internal-raise-backend-error : symbol/#f string/#f string alist -> raises exn
(define (internal-raise-backend-error function code message props)
  (raise 
   (make-exn:fail:backend
    (string-append (if function
                       (format "~a: " function)
                       "")
                   message
                   " (SQL code " code ")")
    (current-continuation-marks)
    props)))
