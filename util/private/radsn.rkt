;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/match
         racket/file
         racket/list
         "../../main.rkt")

#|
RaDSN v0.1 format

A RaDSN (prefs) file maps symbol => <data-source>

<data-source> ::= (db <connector> <args> <extensions>)

<connector> ::= postgresql | mysql | sqlite3 | odbc | odbc-driver

<args> ::= (<arg> ...)
<arg>  ::= <datum> | { <kw> <datum> }

<extensions> ::= ((<symbol> <datum>) ...)

Extensions associate arbitrary extra information with a data-source (for
example, SQL dialect information, testing flags, etc). Extension keys
starting with 'radsn:', 'db:', 'racket:', and 'plt:' are
reserved. Keys may occur multiple times, but the order should not be
considered important.

  db:description ::= <string>, short description
  db:comment ::= <string>, or maybe <xexpr>

|#

(struct data-source (connector args extensions) #:transparent #:mutable)

;; ----------------------------------------

(define none (gensym 'none))

(define (datum? x)
  (or (symbol? x)
      (string? x)
      (number? x)
      (boolean? x)
      (null? x)
      (and (pair? x)
           (datum? (car x))
           (datum? (cdr x)))))

(define (connector? x)
  (memq x '(postgresql mysql sqlite3 odbc odbc-driver)))

(define (parse-arglist x [default none])
  (define (fail . args)
    (cond [(eq? default none) (apply error 'parse-arglist args)]
          [(procedure? default) (default)]
          [else default]))
  (if (list? x)
      (let loop ([x x] [pargs null] [kwargs null])
        (cond [(null? x)
               (list (reverse pargs)
                     (reverse kwargs))]
              [(keyword? (car x))
               (cond [(null? (cdr x)) (fail "keyword without argument: ~a" (car x))]
                     [(datum? (cadr x))
                      (loop (cddr x) pargs (cons (list (car x) (cadr x)) kwargs))]
                     [else
                      (fail "expected readable datum: ~e" (cadr x))])]
              [(datum? (car x))
               (loop (cdr x) (cons (car x) pargs) kwargs)]
              [else (fail "expected readable datum: ~e" (car x))]))
      (fail "expected list")))

(define (arglist? x)
  (and (parse-arglist x #f) #t))

(define (parse-extensions x [default none])
  (let/ec escape
    (define (fail . args)
      (cond [(eq? default none) (apply error 'parse-extensions args)]
            [(procedure? default) (escape (default))]
            [else (escape default)]))
    (if (list? x)
        (map (lambda (x)
               (match x
                 [(list (? symbol? key) (? datum? value))
                  x]
                 [else (fail "expected extension entry: ~e" x)]))
             x)
        (fail "expected list: ~e" x))))

(define (extensions? x)
  (and (parse-extensions x #f) #t))

(define (sexpr->data-source x)
  (let/ec escape
    (match x
      [(list 'db (? connector? connector) (? arglist? args) (? extensions? exts))
       (data-source connector args exts)]
      [_ #f])))

(define (data-source->sexpr x)
  (match x
    [(data-source connector args exts)
     `(db ,connector ,args ,exts)]))

;; ----------------------------------------

(define current-radsn-file
  (make-parameter (build-path (find-system-path 'pref-dir) "db-radsn-0.rktd")))

(define (get-radsn name [default #f] #:radsn-file [file (current-radsn-file)])
  (let* ([sexpr (get-preference name (lambda () #f) 'timestamp file)])
    (or (and sexpr (sexpr->data-source sexpr))
        (if (procedure? default) (default) default))))

(define (put-radsn name value #:radsn-file [file (current-radsn-file)])
  (let* ([sexpr (and value (data-source->sexpr value))])
    (put-preferences (list name)
                     (list sexpr)
                     (lambda () (error 'put-radsn "RaDSN file locked"))
                     file)))

;; ----------------------------------------

(define (get-connect x)
  (case x
    ((postgresql) postgresql-connect)
    ((mysql) mysql-connect)
    ((sqlite3) sqlite3-connect)
    ((odbc) odbc-connect)
    ((odbc-driver) odbc-driver-connect)))

(define radsn-connect
  (make-keyword-procedure
   (lambda (kws kwargs name . pargs)
     (let* ([kws (map list kws kwargs)]
            [file-entry (assq '#:radsn-file kws)]
            [kws* (if file-entry (remq file-entry kws) kws)]
            [file (if file-entry (cdr file-entry) (current-radsn-file))])
       (unless (or (symbol? name) (data-source? name))
         (error 'radsn-connect
                "expected symbol for first argument, got: ~e" name))
       (unless (or (path-string? file) (not file))
         (error 'radsn-connect
                "expected path or string for #:radsn-file keyword, got: ~e"
                file))
       (let ([r (if (data-source? name) name (get-radsn name #f #:radsn-file file))])
         (unless r
           (error 'radsn-connect "cannot find data source named ~e" name))
         (let* ([rargs (parse-arglist (data-source-args r))]
                [rpargs (first rargs)]
                [rkwargs (second rargs)]
                [allpargs (append rpargs pargs)]
                [allkwargs (sort (append rkwargs kws*) keyword<? #:key car)]
                [connect (get-connect (data-source-connector r))])
           (keyword-apply connect (map car allkwargs) (map cadr allkwargs) allpargs)))))))

;; ----

(define (mk-specialized name connector arity kws)
  (procedure-rename
   (procedure-reduce-keyword-arity
    (make-keyword-procedure
     (lambda (kws kwargs . pargs)
       (data-source connector (apply append pargs (map list kws kwargs)) '())))
    arity '() (sort kws keyword<?))
   name))

(define postgresql-data-source
  (mk-specialized 'postgresql-data-source 'postgresql 0
                  '(#:user #:database #:password #:server #:port #:socket
                    #:allow-cleartext-password? #:ssl
                    #:notice-handler #:notification-handler)))

(define mysql-data-source
  (mk-specialized 'mysql-data-source 'mysql 0
                  '(#:user #:database #:password #:server #:port #:socket
                    #:notice-handler)))

(define sqlite3-data-source
  (mk-specialized 'sqlite3-data-source 'sqlite3 0
                  '(#:database #:mode #:busy-retry-limit #:busy-retry-delay)))

(define odbc-data-source
  (mk-specialized 'odbc-data-source 'odbc 0
                  '(#:dsn #:database #:user #:password #:notice-handler
                    #:strict-parameter-types? #:character-mode)))

(define odbc-driver-data-source
  (mk-specialized 'odbc-driver-data-source 'odbc-driver 1
                  '(#:strict-parameter-types? #:character-mode)))

(provide/contract
 [struct data-source
         ([connector connector?]
          [args arglist?]
          [extensions (listof (list/c symbol? datum?))])]
 [radsn-connect procedure?] ;; Can't express "or any kw at all" w/ ->* contract.
 [current-radsn-file (parameter/c path-string?)]
 [get-radsn
  (->* (symbol?) (any/c #:radsn-file path-string?) any)]
 [put-radsn
  (->* (symbol? (or/c data-source? #f)) (#:radsn-file path-string?) void?)]
 [postgresql-data-source
  (->* ()
       (#:user string?
        #:database string?
        #:server string?
        #:port exact-positive-integer?
        #:socket string?
        #:password (or/c string? #f)
        #:allow-cleartext-password? boolean?
        #:ssl (or/c 'yes 'optional 'no)
        #:notice-handler (or/c 'output 'error)
        #:notification-handler (or/c 'output 'error))
       data-source?)]
 [mysql-data-source
  (->* ()
       (#:user string?
        #:database string?
        #:server string?
        #:port exact-positive-integer?
        #:socket string?
        #:password (or/c string? #f)
        #:notice-handler (or/c 'output 'error))
       data-source?)]
 [sqlite3-data-source
  (->* ()
       (#:database (or/c string? 'memory 'temporary)
        #:mode (or/c 'read-only 'read/write 'create)
        #:busy-retry-limit (or/c exact-nonnegative-integer? +inf.0)
        #:busy-retry-delay (and/c rational? (not/c negative?)))
       data-source?)]
 [odbc-data-source
  (->* ()
       (#:dsn string?
        #:user string?
        #:password string?
        #:notice-handler (or/c 'output 'error)
        #:strict-parameter-types? boolean?
        #:character-mode (or/c 'wchar 'utf-8 'latin-1))
       data-source?)])
