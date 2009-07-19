;; Copyright 2000-2009 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang setup/infotab
(define name "db")
(define compile-omit-paths
  '("samples"
    "tests"))
(define blurb
  '("This library provides a high-level interface to PostgreSQL "
    "and MySQL database servers. It is implemented in Scheme and "
    "requires no native client libraries."))
(define version "1.0")
(define scribblings '(("scribblings/doc.scrbl" (multi-page))))
(define categories '(net))
(define can-be-loaded-with 'all)
(define required-core-version "4.1")
