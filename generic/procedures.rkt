;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/contract
         racket/class
         "interfaces.rkt")

(define-syntax-rule (defproc* ifc def ...)
  (begin (defproc ifc . def) ...))

(define-syntax defproc
  (syntax-rules ()
    [(_ ifc [proc method] . formals)
     (begin (define (proc c . formals)
              #|
              (unless (is-a? c ifc)
                (raise-type-error 'proc
                                  (format "instance of ~s" 'ifc)
                                  c))
              |#
              (send c method . formals)))]
    [(_ ifc proc . formals)
     (defproc ifc [proc proc] . formals)]))

(define (connection? x)
  (and (is-a? x connection:admin<%>)
       (is-a? x connection:query<%>)
       (is-a? x connection:query/prepare<%>)))

(define (connection:admin? x)
  (is-a? x connection:admin<%>))

(defproc* connection:admin<%>
  (connected?)
  (disconnect)
  (connection-dbsystem))

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

(define (dbsystem-name x)
  (send x get-short-name))

(define (statement? x)
  (or (string? x) (StatementBinding? x)))

(define (prepared-statement? x)
  (is-a? x prepared-statement<%>))

(define (bind-prepared-statement pst params)
  (send pst bind params))

(provide/contract
 [connection?
  (-> any/c any)]
 [disconnect
  (-> connection? any)]
 [connected?
  (-> connection? any)]
 [connection-dbsystem
  (-> connection? dbsystem?)]
 [dbsystem?
  (-> any/c any)]
 [dbsystem-name
  (-> dbsystem? symbol?)]

 [statement?
  (-> any/c any)]
 [prepared-statement?
  (-> any/c any)]

 [query
  (-> connection? statement? any)]
 [query-multiple
  (-> connection? (listof statement?) any)]
 [query-exec
  (->* (connection?) () #:rest (listof statement?) any)]
 [query-rows
  (-> connection? statement? any)]
 [query-list
  (-> connection? statement? any)]
 [query-row
  (-> connection? statement? any)]
 [query-maybe-row
  (-> connection? statement? any)]
 [query-value
  (-> connection? statement? any)]
 [query-maybe-value
  (-> connection? statement? any)]
 [query-map
  (-> connection? statement? procedure? any)]
 [query-for-each
  (-> connection? statement? procedure? any)]
 [query-mapfilter
  (-> connection? statement? procedure? procedure? any)]
 [query-fold
  (-> connection? statement? procedure? any/c any)]

 [prepare
  (-> connection? string? any)]
 [prepare-multiple
  (-> connection? (listof string?) any)]
 [bind-prepared-statement
  (-> prepared-statement? list? any)]

 [prepare-query-exec
  (-> connection? string? any)]
 [prepare-query-rows
  (-> connection? string? any)]
 [prepare-query-list
  (-> connection? string? any)]
 [prepare-query-row
  (-> connection? string? any)]
 [prepare-query-maybe-row
  (-> connection? string? any)]
 [prepare-query-value
  (-> connection? string? any)]
 [prepare-query-maybe-value
  (-> connection? string? any)]
 [prepare-query-map
  (-> connection? string? procedure? any)]
 [prepare-query-for-each
  (-> connection? string? procedure? any)]
 [prepare-query-mapfilter
  (-> connection? string? procedure? procedure? any)]
 [prepare-query-fold
  (-> connection? string? procedure? any/c any)])
