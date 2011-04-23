;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         (only-in "message.rkt" field-dvec->typeid))
(provide dbsystem)

(define mysql-dbsystem%
  (class* object% (dbsystem<%>)

    (define/public (get-short-name) 'mysql)
    (define/public (get-known-types) supported-types)

    (define/public (has-support? option)
      (case option
        ((real-infinities) #f)
        ((numeric-infinities) #f)
        (else #f)))

    (define/public (get-parameter-handlers param-typeids)
      ;; All params sent as binary data, so handled in message.rkt
      ;; Just need to check params for legal values here
      ;; FIXME: for now, only possible param type is var-string;
      ;; when that changes, will need to refine check-param.
      (map (lambda (param-typid) check-param)
           param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map field-dvec->typeid dvecs))

    (define/public (describe-typeids typeids)
      (map describe-typeid typeids))

    (super-new)))

(define dbsystem
  (new mysql-dbsystem%))


;; ========================================

(define (check-param fsym index param)
  (unless (or (string? param)
              (rational? param)
              (bytes? param)
              (sql-date? param)
              (sql-time? param)
              (sql-timestamp? param)
              (sql-day-time-interval? param))
    (error fsym "cannot marshal as MySQL parameter: ~e" param))
  param)

;; ========================================

(define-type-table (supported-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    describe-typeid
                    type->type-reader
                    type->type-writer)

  (newdecimal  decimal     ()    #t #f #f)
  (tiny        tinyint     ()    #t #f #f)
  (short       smallint    ()    #t #f #f)
  (int24       mediumint   ()    #t #f #f)
  (long        integer     (int) #t #f #f)
  (longlong    bigint      ()    #t #f #f)
  (float       real        ()    #t #f #f)
  (double      double      ()    #t #f #f)
  (newdate     date        ()    #t #f #f)
  (time        time        ()    #t #f #f)
  (datetime    datetime    ()    #t #f #f)
  (varchar     varchar     ()    #t #f #f)
  (var-string  var-string  ()    #t #f #f)
  (tiny-blob   tinyblob    ()    #t #f #f)
  (medium-blob mediumblob  ()    #t #f #f)
  (long-blob   longblob    ()    #t #f #f)
  (blob        blob        ()    #t #f #f))

;; decimal, date typeids not used (?)
