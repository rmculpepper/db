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
    (define/public (typeids->types typeids)
      (map typeid->type typeids))
    (define/public (get-known-types) known-types+aliases)

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

    (super-new)))

(define dbsystem
  (new mysql-dbsystem%))


;; ========================================

(define (check-param fsym index param)
  (unless (or (string? param)
              (rational? param)
              (sql-date? param)
              (sql-time? param)
              (sql-timestamp? param)
              (sql-day-time-interval? param))
    (error fsym "cannot marshal as var-string: ~e" param))
  param)

;; ========================================

(define-type-table (known-type-aliases
                    known-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    type->type-reader
                    type->type-writer)

  (newdecimal  decimal     ()    #f #f)
  (tiny        tinyint     ()    #f #f)
  (short       smallint    ()    #f #f)
  (int24       mediumint   ()    #f #f)
  (long        integer     (int) #f #f)
  (longlong    bigint      ()    #f #f)
  (float       real        ()    #f #f)
  (double      double      ()    #f #f)
  (newdate     date        ()    #f #f)
  (time        time        ()    #f #f)
  (datetime    datetime    ()    #f #f)
  (varchar     varchar     ()    #f #f)
  (var-string  var-string  ()    #f #f)
  (tiny-blob   tinyblob    ()    #f #f)
  (medium-blob mediumblob  ()    #f #f)
  (long-blob   longblob    ()    #f #f)
  (blob        blob        ()    #f #f))

;; decimal, date typeids not used (?)

(define known-types+aliases
  (append known-types known-type-aliases))
