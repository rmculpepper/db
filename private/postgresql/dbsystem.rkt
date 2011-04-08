;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/query.rkt"
         "../generic/sql-convert.rkt")
(provide dbsystem)

(define postgresql-dbsystem%
  (class* object% (dbsystem<%>)

    (define/public (get-short-name) 'postgresql)
    (define/public (get-known-types) known-types+aliases)
    (define/public (typeids->types typeids) (map typeid->type typeids))

    (define/public (has-support? option)
      (case option
        ((real-infinities) #t)
        ((numeric-infinities) #t)
        (else #f)))

    (define/public (get-parameter-handlers param-infos)
      (map (lambda (param-info)
             (let ([type (typeid->type (get-fi-typeid param-info))])
               (or (type->type-writer type)
                   (make-default-marshal type))))
           param-infos))

    (define/public (get-result-handlers result-infos)
      (map (lambda (result-info)
             (let ([type (typeid->type (get-fi-typeid result-info))])
               (type->type-reader type)))
           result-infos))

    (super-new)))

(define dbsystem
  (new postgresql-dbsystem%))

;; ========================================

;; Derived from 
;; http://www.us.postgresql.org/users-lounge/docs/7.2/postgres/datatype.html

(define-type-table (known-type-aliases
                    known-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    type->type-reader
                    type->type-writer)
  (16   boolean    (bool)          parse-boolean      marshal-bool)
  (17   bytea      ()              parse-bytea        marshal-bytea)
  (18   char1      ()              parse-char1        marshal-char1)
  (19   name       ()              parse-string       marshal-string)
  (20   bigint     (int8)          parse-integer      marshal-int8)
  (21   smallint   (int2)          parse-integer      marshal-int2)
  (23   integer    (int int4)      parse-integer      marshal-int4)
  (25   text       ()              parse-string       marshal-string)
  (26   oid        ()              parse-integer      marshal-int4)
  (700  real       (float float4)  parse-real         marshal-real)
  (701  double     (float8)        parse-real         marshal-real)
  (1042 character  (bpchar)        parse-string       marshal-string)
  (1043 varchar    ()              parse-string       marshal-string)
  (1082 date       ()              parse-date         marshal-date)
  (1083 time       ()              parse-time         marshal-time)
  (1114 timestamp  ()              parse-timestamp    marshal-timestamp)
  (1184 timestamptz()              parse-timestamp-tz marshal-timestamp-tz)
  (1186 interval   ()              parse-interval     marshal-interval)
  (1266 timetz     ()              parse-time-tz      marshal-time-tz)
  ;(1560 bit        ())
  ;(1562 varbit     ())
  (1700 decimal    (numeric)       parse-decimal     marshal-decimal))

(define known-types+aliases
  (append known-type-aliases known-types))
