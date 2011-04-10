;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class)
(provide connection<%>
         dbsystem<%>
         prepared-statement<%>

         (struct-out simple-result)
         (struct-out recordset)

         (struct-out statement-binding)
         (struct-out statement-generator)

         init-private

         define-type-table

         connector<%>)

;; ==== Connection

;; connection<%>
(define connection<%>
  (interface ()
    ;; connected? : -> boolean
    connected?

    ;; disconnect : -> void
    disconnect

    ;; get-dbsystem : -> (is-a/c dbsystem<%>)
    get-dbsystem

    ;; query* : symbol (listof Statement) Collector -> (listof QueryResult)
    query*

    ;; prepare* : symbol (listof Preparable) -> (listof PreparedStatement)
    prepare*))


;; ==== DBSystem

;; dbsystem<%>
;; Represents brand of database system, SQL dialect, etc
(define dbsystem<%>
  (interface ()
    get-short-name      ;; -> symbol
    get-known-types     ;; -> (listof symbol)
    typeids->types      ;; (listof typeid) -> (listof type)

    get-parameter-handlers ;; (listof typeid) -> (listof ParameterHandler)
    field-dvecs->typeids   ;; (listof field-dvec) -> (listof typeid)
    ))

;; ParameterHandler = (fsym index datum -> ???)
;; Each system gets to choose its checked-param representation.
;; Maybe check and convert to string. Maybe just check, do binary conversion later.

;; ==== Prepared

;; prepared-statement<%>
(define prepared-statement<%>
  (interface ()
    get-param-count    ;; -> nat or #f
    get-param-typeids  ;; -> (listof typeid)
    get-param-types    ;; -> (listof type)

    get-result-dvecs   ;; -> (listof vector)
    get-result-count   ;; -> nat or #f
    get-result-typeids ;; -> (listof typeid) or #f
    get-result-types   ;; -> (listof type) or #f

    check-owner        ;; symbol connection any -> #t (or error)
    bind               ;; symbol (listof param) -> statement-binding
    ))


;; ==== Auxiliary structures

;; A statement-binding is:
;;   - (statement-binding prepared-statement ??? (listof ???))
;;     meta might include information such as text vs binary format
(struct statement-binding (pst meta params))

;; A statement-generator is:
;;   - (statement-generator table gen)
;;     where table is a weak-hasheq[connection => prepared-statement]
;;     and gen is (dbsystem -> string)
(struct statement-generator (table gen))

;; A YesNoOptional is one of 'yes, 'no, 'optional
;; An SSLMode is one of 'sslv2-or-v3, 'sslv2, 'sslv3, 'tls

;; An query-result is one of:
;;  - (simple-result alist)
;;  - (recordset Header/#f data), determined by collector
;;    for user-visible recordsets: headers present, data is (listof vector)
(struct simple-result (info) #:transparent)
(struct recordset (headers rows) #:transparent)

;; A Header is (listof FieldInfo)
;; A FieldInfo is an alist, contents dbsys-dependent

;; Collector = (nat order -> headers? init combine finish)
;;   where init : A
;;         combine : A vector -> A
;;         finish : A -> A
;;         order is #t for normal order, #f for reversed


;; === Class utilities

;; Here just because ...

(define-syntax-rule (init-private iid ...)
  (begin (init-private1 iid) ...))

(define-syntax-rule (init-private1 iid)
  (begin (init ([private-iid iid]))
         (define iid private-iid)))


;; === Util for defining type tables

(define-syntax-rule (define-type-table (supported-types
                                        type-alias->type
                                        typeid->type
                                        type->typeid
                                        type->type-reader
                                        type->type-writer)
                      (typeid type (alias ...) supported? reader writer) ...)
  (begin
    (define all-types '((type supported?) ...))
    (define supported-types
      (sort (map car (filter cadr all-types))
            string<?
            #:key symbol->string
            #:cache-keys? #t))
    (define (type-alias->type x)
      (case x
        ((alias ...) 'type) ...
        (else x)))
    (define (typeid->type x)
      (case x
        ((typeid) 'type) ...
        (else #f)))
    (define (type->typeid x)
      (case x
        ((type) 'typeid) ...
        (else #f)))
    (define (type->type-reader x)
      (case x
        ((type) reader) ...
        (else #f)))
    (define (type->type-writer x)
      (case x
        ((type) writer) ...
        (else #f)))))


;; == Internal staging interfaces

;; connector<%>
;; Manages making connections
(define connector<%>
  (interface ()
    attach-to-ports            ;; input-port output-port -> void
    start-connection-protocol  ;; string string string/#f -> void
    ))
