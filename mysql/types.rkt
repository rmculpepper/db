;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require "../generic/sql-data.rkt")
(provide wire-typeid->type
         known-types+aliases
         type-alias->type
         type->type-reader
         type->type-writer
         sql-parse
         sql-marshal)

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
    ((var-string) 'varstring)
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
    varchar varstring
    tinyblob mediumblob longblob blob))

(define known-type-aliases
  '(integer biginteger
    real double-precision
    numeric
    character-varying
    time-without-time-zone
    timestamp timestamp-without-time-zone))

(define known-types+aliases
  (append known-type-aliases known-types))

;; type-alias->type : symbol -> symbol
;; FIXME: fill in?
(define (type-alias->type alias)
  (case alias
    ((integer) 'int)
    ((biginteger) 'bigint)
    ((real) 'float)
    ((double-precision) 'double)
    ((numeric) 'decimal)
    ((character-varying) 'varchar)
    ((time-without-time-zone) 'time)
    ;; ((time-with-time-zone) 'timetz) ???
    ((timestamp timestamp-without-time-zone) 'datetime) ;; ???
    ;; ((timestamp-with-time-zone) 'timestamptz) ???
    (else alias)))

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
    ((varchar varstring) parse-string)
    ((tinyblob mediumblob longblob blob) parse-string)
    ;; bit
    ;; enum
    ;; set
    ;; geometry
    (else #f)))

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
    ((varchar varstring) marshal-string)
    ((tinyblob mediumblob longblob blob) marshal-string)
    ;; bit
    ;; enum
    ;; set
    ;; geometry
    (else #f)))

(define (sql-parse type s)
  (let ([parser (type->type-reader type)])
    (unless parser
      (raise-type-error 'sql-parse "type symbol" type))
    (parser s)))

(define (sql-marshal type d)
  (let ([writer (type->type-writer type)])
    (unless writer
      (raise-type-error 'sql-marshal "type symbol" type))
    (writer d)))
