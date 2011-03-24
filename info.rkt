;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang setup/infotab
(define name "db")
(define compile-omit-paths
  '("samples"
    "tests"))
(define blurb
  '("This library provides a high-level, functional interface to PostgreSQL, "
    "MySQL, and SQLite databases. PostgreSQL and MySQL support is implemented "
    "in pure Racket, but SQLite access requires the SQLite native client library."))
(define scribblings '(("scribblings/database.scrbl" (multi-page))))
(define categories '(net))
(define can-be-loaded-with 'all)
(define required-core-version "5.1")
