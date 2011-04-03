;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang setup/infotab
(define name "db")
(define compile-omit-paths
  '("tests"))
(define blurb
  '("A database interface for functional programmers."
    "This library provides a high-level, functional interface to PostgreSQL, "
    "MySQL, and SQLite databases as well as ODBC data sources. "
    "PostgreSQL and MySQL support is implemented in pure Racket. "
    "The optional SQLite and ODBC support requires the appropriate "
    "native client libraries."))
(define scribblings '(("scribblings/db.scrbl" (multi-page))))
(define categories '(net))
(define can-be-loaded-with 'all)
(define required-core-version "5.1")
