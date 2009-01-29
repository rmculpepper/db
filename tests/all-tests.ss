;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 7))
         (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 7))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 7))
         scheme/unit
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
    (link (((BASIS : sql-basis^) (FORMAT : sql-format^) (CONNECT : connect^))
           db@)
          (((CONFIG : config^)) config@ CONNECT)
          (((QUERY-TEST : test^)) query-test@ CONFIG)
          (((SQL-TYPES-TEST : test^)) sql-types-test@ CONFIG BASIS FORMAT)
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

;; (putenv "PGPASSWORD" ???)
;; (test/graphical-ui postgresql:test)
;; (test/graphical-ui mysql:test)
