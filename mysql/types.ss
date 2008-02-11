
#lang scheme/base
(require scheme/unit
         "../generic/sql-data.ss"
         "../generic/sql-format.ss")
(provide type->type-reader
         type->type-writer
         mysql-sql-basis@
         mysql-sql-format@)

;; type->type-reader : symbol -> (string -> datum) or #f
(define (type->type-reader type)
  (case type
    ((decimal newdecimal) parse-decimal)
    ((tiny short long longlong int24) parse-integer)
    ((float double) parse-real)
    ;; null timestamp year 
    ((date newdate) parse-date)
    ((time) parse-time)
    ((datetime) parse-timestamp)
    ((varchar var-string) parse-string)
    ((tiny-blob medium-blob long-blob blob) parse-string)
    ;; bit
    ;; enum
    ;; set
    ;; geometry
    (else #f)))

;; type->type-writer : symbol -> (datum -> string) or #f
(define (type->type-writer type)
  (case type
    ((decimal newdecimal) marshal-decimal)
    ((tiny) marshal-int1)
    ((short) marshal-int2)
    ((int24) marshal-int3)
    ((long) marshal-int4)
    ((longlong) marshal-int8)
    ((float double) marshal-real)
    ;; null
    ;; timestamp year 
    ((date newdate) marshal-date)
    ((time) marshal-time)
    ((datetime) marshal-timestamp)
    ((varchar var-string) marshal-string)
    ((tiny-blob medium-blob long-blob blob) marshal-string)
    ;; bit
    ;; enum
    ;; set
    ;; geometry
    (else #f)))

(define-unit mysql-sql-basis@
  (import)
  (export sql-basis^)
  
  (define (escape-name preserve-case? s)
    (let ([s (if preserve-case? s (string-downcase s))])
      (if (regexp-match? #rx"^[A-Za-z]*$" s)
          s
          (escape-name* s))))
  
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
  
  ;; escape-name : string -> string
  (define (escape-name* s)
    (error 'mysql:escape-name* "don't know how to escape complicated names: ~e" s))
  
  ;; literal-expression : string datum -> string
  (define (literal-expression cast-type literal)
    (quote-literal literal)))

(define-compound-unit/infer mysql-sql-format@
  (import)
  (export sql-basis^ sql-format^)
  (link mysql-sql-basis@ sql-format@))
