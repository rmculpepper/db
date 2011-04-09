;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/query.rkt"
         "../generic/sql-data.rkt"
         "../generic/sql-convert.rkt")
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

    (define/public (get-parameter-handlers param-infos)
      ;; All params sent as binary data, so handled in message.rkt
      ;; Just need to check params for legal values here
      ;; FIXME: for now, only possible param type is var-string;
      ;; when that changes, will need to refine check-param.
      (map (lambda (param-info) check-param)
           param-infos))

    (define/public (get-result-handlers result-infos)
      ;; We force all queries through prepared statement path so that
      ;; all data transfer is done in binary format.
      ;; To re-enable text format for string statement queries,
      ;; change check-statement in connection.rkt and uncomment
      ;; type->type-reader code below.
      (error 'get-result-handlers "unsupported")
      #|
      (map (lambda (result-info)
             (let ([type (typeid->type (get-fi-typeid result-info))])
               (type->type-reader type)))
           result-infos)
      |#)

    (super-new)))

(define dbsystem
  (new mysql-dbsystem%))


;; ========================================

(define (check-param fsym index param-info param)
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

  (newdecimal  decimal     ()    parse-decimal   #f)
  (tiny        tinyint     ()    parse-integer   #f)
  (short       smallint    ()    parse-integer   #f)
  (int24       mediumint   ()    parse-integer   #f)
  (long        integer     (int) parse-integer   #f)
  (longlong    bigint      ()    parse-integer   #f)
  (float       real        ()    parse-real      #f)
  (double      double      ()    parse-real      #f)
  (newdate     date        ()    parse-date      #f)
  (time        time        ()    #f              #f)
  (datetime    datetime    ()    parse-timestamp #f)
  (varchar     varchar     ()    parse-string    #f)
  (var-string  var-string  ()    parse-string    #f)
  (tiny-blob   tinyblob    ()    #f              #f)
  (medium-blob mediumblob  ()    #f              #f)
  (long-blob   longblob    ()    #f              #f)
  (blob        blob        ()    #f              #f))

;; decimal, date typeids not used (?)
;; type-readers retained for debugging

(define known-types+aliases
  (append known-types known-type-aliases))
