#lang scheme/base
(require scheme/unit
         "../generic/sql-data.ss"
         "../generic/sql-format.ss")
(provide (all-defined-out))

;; Derived from 
;; http://www.us.postgresql.org/users-lounge/docs/7.2/postgres/datatype.html

;; type-alias->type : symbol -> symbol
(define (type-alias->type alias)
  (case alias
    ((boolean) 'bool)
    ((character) 'char)
    ((string) 'text)
    ((smallint) 'int2)
    ((integer int serial serial4) 'int4)
    ((bigint serial8) 'int8)
    ((float real) 'float4)
    ((double double-precision) 'float8)
    ((decimal) 'numeric)
    ((character-varying) 'varchar)
    ((time-without-time-zone) 'time)
    ((time-with-time-zone) 'timetz)
    ((timestamp-without-time-zone) 'timestamp)
    ((timestamp-with-time-zone) 'timestamptz)
    (else alias)))

;; type->type-reader : symbol -> (string -> datum) or #f
(define (type->type-reader type)
  (case type
    [(int2 int4 int8 tid xid cid oid) parse-integer]
    [(float4 float8) parse-real]
    [(numeric) parse-decimal]
    [(text varchar char) parse-string]
    [(bytea) parse-bytea]
    [(bool) parse-boolean]
    [(date) parse-date]
    [(time) parse-time]
    [(timetz) parse-time-tz]
    [(timestamp) parse-timestamp]
    [(timestamptz) parse-timestamp-tz]
    [else #f]))

;; type->type-writer : symbol -> (datum -> string) or #f
(define (type->type-writer type)
  (case type
    [(int2) marshal-int2]
    [(int4 xid cid oid) marshal-int4]
    [(int8 tid) marshal-int8]
    [(float4 float8) marshal-real]
    [(numeric) marshal-decimal]
    [(text varchar char) marshal-string]
    [(bytea) marshal-bytea]
    [(bool) marshal-bool]
    [(date) marshal-date]
    [(time) marshal-time]
    [(timetz) marshal-time-tz]
    [(timestamp) marshal-timestamp]
    [(timestamptz) marshal-timestamp-tz]
    [else #f]))

;; type <=> typeoid from:
;; http://doxygen.postgresql.org/include_2catalog_2pg__type_8h-source.html

(define (std:typeoid->type typeoid)
  (case typeoid
    ((16) 'bool)
    ((17) 'bytea)
    ((18) 'char)
    ((19) 'name)
    ((20) 'int8)
    ((21) 'int2)
    ((23) 'int4)
    ((25) 'text)
    ((26) 'oid)
    ((27) 'tid)
    ((700) 'float4)
    ((701) 'float8)
    ((1042) 'bpchar)
    ((1043) 'varchar)
    ((1082) 'date)
    ((1083) 'time)
    ((1114) 'timestamp)
    ((1184) 'timestamptz)
    ((1266) 'timetz)
    ((1560) 'bit)
    ((1562) 'varbit)
    ((1700) 'numeric)
    (else #f)))

(define (std:type->typeoid type)
  (case type
    ((bool) 16)
    ((bytea) 17)
    ((char) 18)
    ((name) 19)
    ((int8) 20)
    ((int2) 21)
    ((int4) 23)
    ((text) 25)
    ((oid) 26)
    ((tid) 27)
    ((float4) 700)
    ((float8) 701)
    ((bpchar) 1042)
    ((varchar) 1043)
    ((date) 1082)
    ((time) 1083)
    ((timestamp) 1114)
    ((timestamptz) 1184)
    ((timetz) 1266)
    ((bit) 1560)
    ((varbit) 1562)
    ((numeric) 1700)
    (else #f)))

(define-unit basis@
  (import)
  (export sql-basis^)

  (define (escape-name preserve-case? s)
    (let ([s (if preserve-case? s (string-downcase s))])
      (if (regexp-match? #rx"^[A-Za-z]*$" s)
          s
          (escape-name* s))))

  ;; escape-name : string -> string
  (define (escape-name* s)
    (string-append "\""
                   (regexp-replace #rx"\"" s "\"\"")
                   "\""))

  (define (sql-parse type s)
    (let ([parser (type->type-reader (type-alias->type type))])
      (unless parser
        (raise-type-error 'sql-parse "type symbol" type))
      (parser s)))

  (define (sql-marshal type d)
    (let ([writer (type->type-writer (type-alias->type type))])
      (unless writer
        (raise-type-error 'sql-marshal "type symbol" type))
      (writer d)))

  ;; literal-expression : string datum -> string
  (define (literal-expression cast-type literal)
    (format "CAST( E~a AS ~a)" (quote-literal literal) cast-type)))

(define-compound-unit/infer postgresql-sql-format@
  (import)
  (export sql-basis^ sql-format^)
  (link basis@ sql-format@))
