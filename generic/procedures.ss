;; Copyright 2009 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/class
         "interfaces.ss")
(provide (except-out (all-defined-out)
                     defproc*
                     defproc))

(define-syntax-rule (defproc* ifc def ...)
  (begin (defproc ifc . def) ...))
(define-syntax defproc
  (syntax-rules ()
    [(_ ifc [proc method] . formals)
     (begin (define (proc c . formals)
              (unless (is-a? c ifc)
                (raise-type-error 'proc
                                  (format "instance of ~s" 'ifc)
                                  c))
              (send c method . formals)))]
    [(_ ifc proc . formals)
     (defproc ifc [proc proc] . formals)]))

(define (connection:admin? x)
  (is-a? x connection:admin<%>))

(defproc* connection:admin<%>
  (connected?)
  (disconnect)
  (get-system))

(define (connection:query? x)
  (is-a? x connection:query<%>))

(defproc* connection:query<%>
  (query stmt)
  (query-multiple stmts)
  (query-exec . stmts)
  (query-rows stmt)
  (query-list stmt)
  (query-row stmt)
  (query-maybe-row stmt)
  (query-value stmt)
  (query-maybe-value stmt)
  (query-map stmt proc)
  (query-for-each stmt proc)
  (query-mapfilter stmt mapproc filterproc)
  (query-fold  stmt proc base))

(define (connection:query/prepare? x)
  (is-a? x connection:query/prepare<%>))

(defproc* connection:query/prepare<%>
  (prepare stmt)
  (prepare-multiple stmts)
  (bind-prepared-statement prep params)
  (prepare-query-exec stmt)
  (prepare-query-rows stmt)
  (prepare-query-list stmt)
  (prepare-query-row stmt)
  (prepare-query-maybe-row stmt)
  (prepare-query-value stmt)
  (prepare-query-maybe-value stmt)
  (prepare-query-map stmt proc)
  (prepare-query-for-each stmt proc)
  (prepare-query-mapfilter stmt mapproc filterproc)
  (prepare-query-fold stmt proc base))

(define (dbsystem? x)
  (is-a? x dbsystem<%>))
