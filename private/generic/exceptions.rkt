;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(provide (struct-out exn:fail:sql)
         raise-sql-error)

#|
Exceptions

Only errors with an associated SQLSTATE are represented by
exn:fail:sql, specifically only errors originating from a database
backend or library. Other errors are typically raised using 'error',
producing plain old exn:fail.
|#

;; exn:fail:sql
;; Represents an error with an associated SQLSTATE
(define-struct (exn:fail:sql exn:fail) (sqlstate info))

;; raise-sql-error : symbol string string alist -> raises exn
(define (raise-sql-error who sqlstate message info)
  (raise 
   (make-exn:fail:sql (format "~a: ~a (SQLSTATE ~a)" who message sqlstate)
                      (current-continuation-marks)
                      sqlstate
                      info)))
