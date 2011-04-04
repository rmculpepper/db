;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/query.rkt"
         "../generic/sql-data.rkt"
         "../generic/sql-convert.rkt")
(provide (all-defined-out))

(define mysql-dbsystem%
  (class* object% (dbsystem<%>)

    (define/public (get-short-name) 'mysql)
    (define/public (typeids->types typeids)
      (map wire-typeid->type typeids))
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
      (map (lambda (result-info)
             (let ([type (wire-typeid->type (get-fi-typeid result-info))])
               (type->type-reader type)))
           result-infos))

    (super-new)))

(define dbsystem
  (new mysql-dbsystem%))


;; ========================================

(define (check-param param)
  (unless (or (string? param)
              (rational? param)
              (sql-date? param)
              (sql-time? param)
              (sql-timestamp? param))
    ;; FIXME: need fsym propagation
    (error 'bind* "cannot marshal as var-string: ~e" param))
  param)


;; ========================================

(define (wire-typeid->type id)
  (case id
    ((decimal newdecimal) 'decimal) ;; ???
    ((tiny) 'tinyint)
    ((short) 'smallint)
    ((int24) 'mediumint)
    ((long) 'int)
    ((longlong) 'bigint)
    ((float) 'float)
    ((double) 'double)
    ((null) 'null) ;; ???!!!
    ((date newdate) 'date) ;; ???
    ((time) 'time)
    ((datetime) 'datetime)
    ((varchar) 'varchar)
    ((var-string) 'var-string)
    ((tiny-blob) 'tinyblob)
    ((medium-blob) 'mediumblob)
    ((long-blob) 'longblob)
    ((blob) 'blob)
    (else (error 'typeid->type "unknown type id: ~s" id))))

(define known-types
  '(decimal
    tinyint smallint mediumint int bigint
    float double
    date time datetime
    varchar var-string
    tinyblob mediumblob longblob blob))

(define known-type-aliases
  '(integer real numeric))

(define known-types+aliases
  (append known-type-aliases known-types))

;; type-alias->type : symbol -> symbol
;; FIXME: fill in?
(define (type-alias->type alias)
  (case alias
    ((integer) 'int)
    ((real) 'float)
    ((numeric) 'decimal)
    (else alias)))

;; FIXME: Only non-param'd query path uses type-readers;
;; would be better to always take binary path, eliminate redundancy.

;; type->type-reader : symbol -> (string -> datum) or #f
(define (type->type-reader type)
  (case type
    ((decimal) parse-decimal)
    ((tinyint smallint mediumint int bigint) parse-integer)
    ((float double) parse-real)
    ;; null timestamp year 
    ((date) parse-date)
    ((time) parse-time)
    ((datetime) parse-timestamp)
    ((varchar var-string) parse-string)
    ((tinyblob mediumblob longblob blob) parse-string)
    ;; bit
    ;; enum
    ;; set
    ;; geometry
    (else #f)))

#|

;; Parameters sent as binary data, so handled at lower level.

;; type->type-writer : symbol -> (datum -> string) or #f
(define (type->type-writer type)
  (case type
    ((decimal) marshal-decimal)
    ((tinyint) marshal-int1)
    ((smallint) marshal-int2)
    ((mediumint) marshal-int3)
    ((int) marshal-int4)
    ((bigint) marshal-int8)
    ((float double) marshal-real)
    ;; null
    ;; timestamp year 
    ((date) marshal-date)
    ((time) marshal-time)
    ((datetime) marshal-timestamp)
    ((varchar var-string) marshal-string)
    ((tinyblob mediumblob longblob blob) marshal-string)
    ;; bit
    ;; enum
    ;; set
    ;; geometry
    (else #f)))

|#
