#lang scheme/base
(require (for-syntax scheme/base)
         scheme/match
         scheme/unit
         "sql-data.ss")
(provide sql-format^
         sql-format@
         sql-basis^)

(define-signature sql-format^
  [;; for macro use only
   private-sql-marshal-literal/type+datum
   private-sql-marshal-literal/trust
   private-sql-marshal-name/no-preserve-case
   private-sql-marshal-name/preserve-case
   private-sql-marshal-sql
   
   (define-syntaxes (format-sql concat-sql)
     (let ()
       (define (type-spec->expr function tp)
         (syntax-case tp ()
           [(#:trust datum type)
            #'(private-sql-marshal-literal/trust datum type)]
           [(#:name datum)
            #'(private-sql-marshal-name/no-preserve-case datum)]
           [(#:Name datum)
            #'(private-sql-marshal-name/preserve-case datum)]
           [(#:sql code)
            #'(private-sql-marshal-sql code)]
           [(type datum)
            (not (memq (syntax-e #'type) '(#:trust #:Name #:name #:sql)))
            (begin
              (unless (identifier? #'type)
                (raise-syntax-error 'format-sql "expected SQL type name"
                                    #'type))
              #'(private-sql-marshal-literal/type+datum 'type datum))]
           [else
            (raise-syntax-error function "bad type-spec" tp)]))
       (values
        ;; format-sql SYNTAX
        ;; (format-sql format-string [type datum] ...)
        (lambda (stx)
          (syntax-case stx ()
            [(format-sql part ...)
             (let ()
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
                          (values (reverse fparts) parts)])))
               (define-values (format-parts type-parts)
                 (partition (syntax->list #'(part ...))))
               (with-syntax ([(part-as-string ...)
                              (map (lambda (tp) (type-spec->expr 'format-sql tp))
                                   type-parts)])
                 #`(format (quote #,(apply string-append format-parts))
                           part-as-string ...)))]))
        ;; concat-sql SYNTAX
        (lambda (stx)
          (syntax-case stx ()
            [(concat-sql fragment ...)
             (let ()
               (define (process fragment)
                 (if (string? (syntax-e fragment))
                     #`(quote #,fragment)
                     (type-spec->expr 'concat-sql fragment)))
               (with-syntax ([(string-expr* ...)
                              (map process (syntax->list #'(fragment ...)))])
                 (with-syntax ([((string-expr ...) ...) #'((string-expr* '" ") ...)])
                   #'(string-append string-expr ... ...))))])))))])

(define-signature sql-basis^
  [;; escape-name : boolean string -> string
   ;; (escape-name preserve-case? name)
   escape-name

   ;; literal-expression : string string -> string
   ;; (literal-expression typename external-representation)
   literal-expression

   ;; sql-parse : symbol string -> datum
   sql-parse
   
   ;; sql-marshal : symbol datum -> string
   sql-marshal])

(define-unit sql-format@
  (import sql-basis^)
  (export sql-format^)

  ;; private-sql-marshal-name/no-preserve-case : string -> string
  (define (private-sql-marshal-name/no-preserve-case datum)
    (unless (string? datum)
      (raise-type-error 'sql "string" datum))
    (escape-name #f datum))

  ;; private-sql-marshal-name/preserve-case : string -> string
  (define (private-sql-marshal-name/preserve-case datum)
    (unless (string? datum)
      (raise-type-error 'sql "string" datum))
    (escape-name #t datum))

  ;; private-sql-marshal-sql : string -> string
  (define (private-sql-marshal-sql code)
    (unless (string? code)
      (raise-type-error 'format-sql "string" code))
    code)

  ;; private-sql-marshal-literal/type+datum : symbol datum -> string
  (define (private-sql-marshal-literal/type+datum type datum)
    (unless (symbol? type)
      (raise-type-error 'private-sql-marshal-literal/type+datum
                        "symbol" type))
    (if (sql-null? datum)
        "NULL"
        (literal-expression type (sql-marshal type datum))))

  ;; private-sql-marshal-literal/trust : datum string -> string
  (define (private-sql-marshal-literal/trust datum typename)
    ;; FIXME: do more checking on valid typenames (ex: no "--")
    (unless (string? typename)
      (raise-type-error 'format-sql "string" typename))
    (if (sql-null? datum)
        "NULL"
        (literal-expression typename datum)))
  )
