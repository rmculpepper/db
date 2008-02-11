;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module info (lib "infotab.ss" "setup")
  (define name "spgsql")
  (define compile-omit-files '("samples/sample1.ss"))
  (define blurb
    '("The spgsql library provides a high-level interface to PostgreSQL "
      "database servers. It is implemented in Scheme and requires "
      "no C client libraries."))
  (define primary-file "spgsql.ss")
  (define homepage "http://schematics.sourceforge.net/spgsql.html")
  (define version "5.1")
  (define scribblings '(("doc.scrbl" ())))
  (define doc.txt "doc.txt")
  (define categories '(net))
  (define can-be-loaded-with 'all)
  (define required-core-version "371.3")
  (define release-notes
    '("Spgsql 5.1 adds automatic connection synchronization "
      "and improves handling of time values. "
      "Depends on 371.3 for SRFI-19 bugfixes.")))
