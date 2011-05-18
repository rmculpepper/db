;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/file
         racket/list
         "../main.rkt")
(provide (all-defined-out))

#|
RaDSN v0.1 format

A RaDSN (prefs) file maps symbol => <radsn>

<radsn> ::= (db <connector> <args> <extensions>)

<connector> ::= postgresql | mysql | sqlite3 | odbc | odbc-driver

<args> ::= (<arg> ...)
<arg>  ::= <datum> | { <kw> <datum> }

<extensions> ::= ((<symbol> <datum>) ...)

Extensions associate arbitrary extra information with a RaDSN (for
example, SQL dialect information, testing flags, etc). Extension keys
starting with 'radsn:', 'db:', 'racket:', and 'plt:' are
reserved. Keys may occur multiple times, but the order should not be
considered important.

  db:description ::= <string>, short description
  db:comment ::= <string>, or maybe <xexpr>

|#

(struct radsn (connector args extensions) #:transparent)

;; ----------------------------------------

(define none (gensym 'none))

(define (datum? x) #t) ;; FIXME: want only readable

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
               (cond [(pair? (cdr x))
                      (loop (cddr x) pargs (cons (list (car x) (cadr x)) kwargs))]
                     [else (fail "keyword without argument: ~a" (car x))])]
              [else (loop (cdr x) (cons (car x) pargs) kwargs)]))
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

(define (sexpr->radsn x)
  (let/ec escape
    (match x
      [(list 'db (? connector? connector) (? arglist? args) (? extensions? exts))
       (radsn connector args exts)]
      [_ #f])))

(define (radsn->sexpr x)
  (match x
    [(radsn connector args exts)
     `(db ,connector ,args ,exts)]))

;; ----------------------------------------

(define (default-file)
  (build-path (find-system-path 'pref-dir) "ryanc-db-radsn-0.rktd"))

(define (get-radsn name [default #f] [file #f])
  (let* ([file (or file (default-file))]
         [sexpr (get-preference name (lambda () #f) 'timestamp file)])
    (or (and sexpr (sexpr->radsn sexpr))
        (if (procedure? default) (default) default))))

(define (put-radsn name value [file #f])
  (let* ([file (or file (default-file))]
         [sexpr (radsn->sexpr value)])
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
            [file (and file-entry (cdr file-entry))])
       (unless (or (symbol? name) (radsn? name))
         (error 'radsn-connect
                "expected symbol for first argument, got: ~e" name))
       (unless (or (path-string? file) (not file))
         (error 'radsn-connect
                "expected path, string, or #f for #:radsn-file keyword, got: ~e"
                file))
       (let ([r (if (radsn? name) name (get-radsn name #f file))])
         (unless r
           (error 'radsn-connect "cannot find RaDSN named ~e" name))
         (let* ([rargs (parse-arglist (radsn-args r))]
                [rpargs (first rargs)]
                [rkwargs (second rargs)]
                [allpargs (append rpargs pargs)]
                [allkwargs (sort (append rkwargs kws*) keyword<? #:key car)]
                [connect (get-connect (radsn-connector r))])
           (keyword-apply connect (map car allkwargs) (map cadr allkwargs) allpargs)))))))

(provide (all-defined-out))
