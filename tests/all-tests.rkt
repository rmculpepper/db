;; Copyright 2000-2009 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 7))
         (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 7))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 7))
         scheme/unit
         scheme/class
         "../generic/signatures.ss"
         "../postgresql/unit.ss"
         "../mysql/unit.ss"
         "config.ss"
         "connection.ss"
         "sql-types.ss"
         "concurrent.ss")

(define-unit all-tests@
  (import (tag query (prefix query: test^))
          (tag sql-types (prefix sql-types: test^))
          (tag concurrent (prefix concurrent: test^)))
  (export test^)

  (define test
    (test-suite "All tests"
      query:test
      sql-types:test
      concurrent:test)))

(define (specialize-test db@ #;db-specific-test@)
  (compound-unit
    (import)
    (export ALL-TESTS)
    (link (((DB : database^)) db@)
          (((CONFIG : config^)) config@ DB)
          (((QUERY-TEST : test^)) query-test@ CONFIG)
          (((SQL-TYPES-TEST : test^)) sql-types-test@ CONFIG DB)
          (((CONCURRENT-TEST : test^)) concurrent-test@ CONFIG)
          (((ALL-TESTS : test^)) all-tests@
                                 (tag query QUERY-TEST)
                                 (tag sql-types SQL-TYPES-TEST)
                                 (tag concurrent CONCURRENT-TEST)))))

(define-values/invoke-unit (specialize-test postgresql@)
  (import)
  (export (prefix postgresql: test^)))

(define-values/invoke-unit (specialize-test mysql@)
  (import)
  (export (prefix mysql: test^)))

;; (putenv "DBPASSWORD" ???)
;; (test/graphical-ui postgresql:test)
;; (test/graphical-ui mysql:test)

#|
(begin (define-values/invoke-unit postgresql@ (import) (export database^))
       (define-values/invoke-unit config@ (import database^) (export config^))
       (define c (connect-and-setup)))
|#
