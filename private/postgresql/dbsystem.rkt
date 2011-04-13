;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/sql-convert.rkt"
         (only-in "msg.rkt" field-dvec->typeid))
(provide dbsystem
         typeid->type
         type->type-reader)

(define postgresql-dbsystem%
  (class* object% (dbsystem<%>)

    (define/public (get-short-name) 'postgresql)
    (define/public (get-known-types) supported-types)
    (define/public (typeids->types typeids) (map typeid->type typeids))

    (define/public (has-support? option)
      (case option
        ((real-infinities) #t)
        ((numeric-infinities) #t)
        (else #f)))

    (define/public (get-parameter-handlers param-typeids)
      (map (lambda (param-typeid)
             (let ([type (typeid->type param-typeid)])
               (or (type->type-writer type)
                   (make-default-marshal type))))
           param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map field-dvec->typeid dvecs))

    (super-new)))

(define dbsystem
  (new postgresql-dbsystem%))

;; ========================================

;; Derived from 
;; http://www.us.postgresql.org/users-lounge/docs/7.2/postgres/datatype.html

(define-type-table (supported-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    type->type-reader
                    type->type-writer)
  (16   boolean    (bool)    #t     parse-boolean      marshal-bool)
  (17   bytea      ()        #t     parse-bytea        marshal-bytea)
  (18   char1      ()        #t     parse-char1        marshal-char1)
  (19   name       ()        #t     parse-string       marshal-string)
  (20   bigint     (int8)    #t     parse-integer      marshal-int8)
  (21   smallint   (int2)    #t     parse-integer      marshal-int2)
  (23   integer    (int4)    #t     parse-integer      marshal-int4)
  (25   text       ()        #t     parse-string       marshal-string)
  (26   oid        ()        #t     parse-integer      marshal-int4)
  (700  real       (float4)  #t     parse-real         marshal-real)
  (701  double     (float8)  #t     parse-real         marshal-real)
  (1042 character  (bpchar)  #t     parse-string       marshal-string)
  (1043 varchar    ()        #t     parse-string       marshal-string)
  (1082 date       ()        #t     parse-date         marshal-date)
  (1083 time       ()        #t     parse-time         marshal-time)
  (1114 timestamp  ()        #t     parse-timestamp    marshal-timestamp)
  (1184 timestamptz()        #t     parse-timestamp-tz marshal-timestamp-tz)
  (1186 interval   ()        #t     parse-interval     marshal-interval)
  (1266 timetz     ()        #t     parse-time-tz      marshal-time-tz)
  (1700 decimal    (numeric) #t     parse-decimal      marshal-decimal)

  ;; "string" literals have type unknown; just treat as string
  (705 unknown     ()        #t     parse-string       marshal-string)

  ;; The following types are not supported.
  ;; (But putting their names here yields better not-supported errors.)

  (1560 bit        ()        #f #f #f)
  (1562 varbit     ()        #f #f #f)

  (142 xml       () #f #f #f)
  (600 point     () #f #f #f)
  (601 lseg      () #f #f #f)
  (602 path      () #f #f #f)
  (603 box       () #f #f #f)
  (604 polygon   () #f #f #f)
  (628 line      () #f #f #f)
  (702 abstime   () #f #f #f)
  (703 reltime   () #f #f #f)
  (704 tinterval () #f #f #f)
  (718 circle    () #f #f #f)
  (790 money     () #f #f #f)
  (829 macaddr   () #f #f #f)
  (869 inet      () #f #f #f)
  (650 cidr      () #f #f #f))
