;; Copyright 2011-2013 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang setup/infotab

(define name "db (PLaneT)")
(define scribblings '(("scribblings/db.scrbl" (multi-page))))
(define compile-omit-paths '("tests"))

(define blurb
  '("Obsolete: this library is now part of the Racket distribution; use "
    (tt "(require db)") " instead."))
(define categories '(io net))
(define can-be-loaded-with 'all)
(define primary-file "main.rkt")
(define required-core-version "5.2")
(define repositories '("4.x"))

(define release-notes
  '("Redirect to db library distributed with Racket."))
