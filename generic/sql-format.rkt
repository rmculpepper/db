;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require (for-syntax racket/base)
         racket/unit
         racket/class
         "interfaces.rkt"
         "sql-data.rkt")
(provide format-sql
         concat-sql)

(begin-for-syntax
  (define (format-part? part)
    (string? (syntax-e part)))

  (define (ok-format-string? str)
    (regexp-match? #rx"^(?:[^~]|(?:~[an%~]))*$" str))

  (define (partition parts)
    (let loop ([fparts null] [parts parts])
      (cond [(and (pair? parts) (format-part? (car parts)))
             (unless (ok-format-string? (syntax-e (car parts)))
               (raise-syntax-error
                'format-sql
                "expected format string with only '~a' placeholders"
                (car parts)))
             (loop (cons " " (cons (syntax-e (car parts)) fparts))
                   (cdr parts))]
            [else
             (values (reverse fparts) parts)]))))


(define-syntax (format-sql stx)
  (syntax-case stx ()
    [(format-sql obj-expr part ...)
     (let-values ([(format-parts type-parts)
                   (partition (syntax->list #'(part ...)))])
       (with-syntax ([(part ...) type-parts])
         #`(let ([obj (convert-to-dbsystem obj-expr)])
             (format (quote #,(apply string-append format-parts))
                     (interpret-spec obj part) ...))))]))

(define-syntax (concat-sql stx)
  (syntax-case stx ()
    [(concat-sql obj-expr fragment ...)
     (let ()
       (define (process fragment)
         (if (string? (syntax-e fragment))
             #`(quote #,fragment)
             #`(interpret-spec obj #,fragment)))
       (with-syntax ([(string-expr* ...)
                      (map process (syntax->list #'(fragment ...)))])
         (with-syntax ([((string-expr ...) ...) #'((string-expr* '" ") ...)])
           #`(let ([obj (convert-to-dbsystem obj-expr)])
               (string-append string-expr ... ...)))))]))

(define (convert-to-dbsystem obj)
  (cond [(is-a? obj dbsystem<%>) obj]
        [(is-a? obj connection:admin<%>)
         (send obj get-system)]
        [else
         (raise-type-error 'concat-sql
                           "instance of connection:admin<%> or dbsystem<%>"
                           obj)]))

(define-syntax (interpret-spec stx)
  (syntax-case stx ()
    [(interpret-spec obj fragment)
     (syntax-case #'fragment ()
       [(#:trust datum type)
        #'(private-sql-marshal-literal/trust obj datum type)]
       [(#:name datum)
        #'(private-sql-marshal-name/no-preserve-case obj datum)]
       [(#:Name datum)
        #'(private-sql-marshal-name/preserve-case obj datum)]
       [(#:sql code)
        #'(private-sql-marshal-sql obj code)]
       [(type datum)
        (not (memq (syntax-e #'type) '(#:trust #:Name #:name #:sql)))
        (begin
          (unless (identifier? #'type)
            (raise-syntax-error 'concat-sql "expected SQL type name"
                                #'type))
          #'(private-sql-marshal-literal/type+datum obj 'type datum))]
       [else
        (raise-syntax-error 'concat-sql "bad sql-spec" #'fragment)])]))

;; private-sql-marshal-name/no-preserve-case : dbsystem string -> string
(define (private-sql-marshal-name/no-preserve-case obj datum)
  (unless (string? datum)
    (raise-type-error 'concat-sql "string" datum))
  (send obj escape-name #:preserve-case? #f datum))

;; private-sql-marshal-name/preserve-case : dbsystem string -> string
(define (private-sql-marshal-name/preserve-case obj datum)
  (unless (string? datum)
    (raise-type-error 'concat-sql "string" datum))
  (send obj escape-name #:preserve-case? #t datum))

;; private-sql-marshal-sql : dbsystem string -> string
(define (private-sql-marshal-sql obj code)
  (unless (string? code)
    (raise-type-error 'concat-sql "string" code))
  code)

;; private-sql-marshal-literal/type+datum : dbsystem symbol datum -> string
(define (private-sql-marshal-literal/type+datum obj type datum)
  ;; (error 'private-sql-marshal-literal/type+datum "unimplemented")
  (unless (symbol? type)
    (raise-type-error 'private-sql-marshal-literal/type+datum
                      "symbol" type))
  (if (sql-null? datum)
      "NULL"
      (send obj sql:literal-expression
            type
            (sql-marshal obj type datum))))

(define (sql-marshal dbsystem typealias s)
  (define type (send dbsystem typealias->type typealias))
  (define writer (send dbsystem get-type-writer type))
  (unless writer
    (error 'sql-marshal "no writer for type: ~s (~s)" typealias type))
  (writer s))

;; private-sql-marshal-literal/trust : dbsystem datum string -> string
(define (private-sql-marshal-literal/trust obj datum typename)
  ;; FIXME: do more checking on valid typenames (ex: no "--")
  (unless (string? typename)
    (raise-type-error 'concat-sql "string" typename))
  (if (sql-null? datum)
      "NULL"
      (send obj sql:literal-expression typename datum)))
