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
         (struct-out field-info)

         (struct-out statement-binding)
         (except-out (struct-out auto-prepare-statement)
                     auto-prepare-statement)

         init-private)

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
    has-support?        ;; any -> boolean

    ;; get-parameter-handlers : (listof alist) -> (listof (datum -> ???))
    ;; Each system gets to choose its checked-param representation.
    ;; Maybe check and convert to string. Maybe just check, do binary conversion later.
    get-parameter-handlers

    ;; get-result-handlers : (listof alist) -> (listof (U #f (-> ??? datum)))
    ;; Not used by all systems.
    get-result-handlers))


;; ==== Prepared

;; prepared-statement<%>
(define prepared-statement<%>
  (interface ()
    get-param-infos    ;; 
    get-param-count    ;; -> nat or #f
    get-param-types    ;; -> (listof type)

    get-result-infos   ;; -> (listof alist)
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

;; An auto-prepare-statement is:
;;   - (auto-prepare-statement table gen)
;;     where table is a weak-hasheq[connection => prepared-statement]
;;     and gen is (dbsystem -> string)
(define-struct auto-prepare-statement (table gen))

;; A YesNoOptional is one of 'yes, 'no, 'optional
;; An SSLMode is one of 'sslv2-or-v3, 'sslv2, 'sslv3, 'tls

;; A query-result is one of:
;;  - (simple-result alist)
;;  - (recordset Header value)
(struct simple-result (info) #:transparent)
(struct recordset (info data) #:transparent)

;; A Header is one of
;;  - field-info
;;  - list of field-info
;;  - #f

;; A field-info is (field-info string alist)
(struct field-info (name info) #:transparent)

;; A Collector is
;;   (-> (listof alist) boolean
;;       (values b (b vector -> b) (b -> c) Header))
;;
;; (collector row-descriptions binary?)
;;   = (values init-accum (accum row -> accum) (accum -> final) header)

;; FIXME: in collector, why binary? flag?
;;   - not used by generic/functions

;; === Class utilities

;; Here just because ...

(define-syntax-rule (init-private iid ...)
  (begin (init-private1 iid) ...))

(define-syntax-rule (init-private1 iid)
  (begin (init ([private-iid iid]))
         (define iid private-iid)))
