;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang setup/infotab
(define name "db")
(define compile-omit-files '("samples/sample1.ss"))
(define blurb
  '("The spgsql library provides a high-level interface to PostgreSQL "
    "database servers. It is implemented in Scheme and requires "
    "no C client libraries."))
(define primary-file "spgsql.ss")
(define homepage "http://schematics.sourceforge.net/spgsql.html")
(define version "1.0")
(define scribblings '(("scribblings/doc.scrbl" (multi-page))))
(define doc.txt "doc.txt")
(define categories '(net))
(define can-be-loaded-with 'all)
(define required-core-version "4.1")
