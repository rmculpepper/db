;; Copyright 2009 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/class)

(define-syntax-rule (defproc* def ...)
  (begin (defproc . def) ...))
(define-syntax defproc
  (syntax-rules ()
    [(_ [proc method] . formals)
     (begin (provide proc)
            (define (proc c . formals)
              (send c method . formals)))]
    [(_ proc . formals)
     (defproc [proc proc] . formals)]))

(defproc*
  (query stmt)
  (query-multiple stmts)
  ([query-exec exec] . stmts)
  (query-rows stmt)
  (query-list stmt)
  (query-row stmt)
  (query-maybe-rows stmt)
  (query-value stmt)
  (query-maybe-value stmt)
  ([query-map map] stmt proc)
  ([query-for-each for-each] stmt proc)
  ([query-mapfilter mapfilter] stmt mapproc filterproc)
  ([query-fold fold] stmt proc base)

  (prepare stmt)
  (prepare-multiple stmts)
  (bind-prepared-statement prep params)
  (prepare-exec stmt)
  (prepare-query-rows stmt)
  (prepare-query-list stmt)
  (prepare-query-row stmt)
  (prepare-query-maybe-row stmt)
  (prepare-query-value stmt)
  (prepare-query-maybe-value stmt)
  (prepare-map stmt proc)
  (prepare-for-each stmt proc)
  (prepare-mapfilter stmt mapproc filterproc)
  (prepare-fold stmt proc base))
