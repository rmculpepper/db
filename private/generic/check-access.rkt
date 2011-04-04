;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(provide (except-out (all-defined-out) define-mz))

(define-ffi-definer define-mz #f)

(define SCHEME_GUARD_FILE_READ    #x01)
(define SCHEME_GUARD_FILE_WRITE   #x02)
(define SCHEME_GUARD_FILE_EXECUTE #x04)
(define SCHEME_GUARD_FILE_DELETE  #x08)
(define SCHEME_GUARD_FILE_EXISTS  #x10)

(define-mz scheme_security_check_file
  (_fun _string _path _int
        -> _void))
