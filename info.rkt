;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang setup/infotab

(define name "db")
(define compile-omit-paths
  '("tests"))
(define scribblings '(("scribblings/db.scrbl" (multi-page))))

(define blurb
  '("A database interface for functional programmers. "
    "This library provides a high-level, functional interface to PostgreSQL, "
    "MySQL, and SQLite databases as well as ODBC data sources. "
    "PostgreSQL and MySQL support is implemented in pure Racket. "
    "The optional SQLite and ODBC support requires the appropriate "
    "native client libraries."))
(define release-notes
  '("Initial release."))
(define categories '(io net))
(define can-be-loaded-with 'all)
(define primary-file "main.rkt")
(define required-core-version "5.1")
(define repositories '("4.x"))
