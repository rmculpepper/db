;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class)

(provide connection<%>

         dbsystem<%>

         (struct-out simple-result)
         (struct-out recordset)
         (struct-out field-info)

         prepared-statement<%>
         (struct-out statement-binding)
         (except-out (struct-out auto-prepare-statement)
                     auto-prepare-statement)

         connector<%>
         ssl-connector<%>
         connection:admin<%>

         init-private)

;; ==== Connection Interfaces

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


;; ==== DBSystem Interface

;; dbsystem<%>
;; Represents brand of database system, SQL dialect, etc
(define dbsystem<%>
  (interface ()
    ;; get-short-name : -> symbol
    get-short-name

    ;; get-known-types : #:can-read? [bool #t] #:can-write? [bool #t]
    ;;                -> (listof symbol)
    get-known-types

    ;; typeids->types : (listof typeid) -> (listof type)
    typeids->types

    ;; typeids->type-readers : (listof typeid) -> (listof (U #f (-> string datum)))
    typeids->type-readers

    ;; typeids->type-writers : (listof typeid) -> (listof (-> datum string))
    typeids->type-writers

    ;; has-support? : any -> boolean?
    has-support?))

;; ==== Auxiliary Interfaces & Structures

;; prepared-statement<%>
(define prepared-statement<%>
  (interface ()

    get-param-count    ;; -> nat or #f
    get-param-typeids  ;; -> (listof typeid) or #f
    get-param-types    ;; -> (listof type) or #f

    get-result-count   ;; -> nat or #f
    get-result-typeids ;; -> (listof typeid) or #f
    get-result-types   ;; -> (listof type) or #f

    ;; bind : symbol (listof param) -> StatementBinding
    bind))

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


;; == Internal staging interfaces

;; connector<%>
;; Manages making connections
(define connector<%>
  (interface ()
    ;; attach-to-ports : input-port output-port -> void
    attach-to-ports

    ;; start-connection-protocol : string string string/#f -> void
    start-connection-protocol
    ))

;; ssl-connector<%>
(define ssl-connector<%>
  (interface (connector<%>)
    ;; set-ssl-options : YesNoOptional SSLMode -> void
    set-ssl-options))

;; connection:admin<%>
(define connection:admin<%>
  (interface ()
    connected?
    disconnect
    get-dbsystem))

;; === Class Utilities

;; Here just because ...

(define-syntax-rule (init-private iid ...)
  (begin (init-private1 iid) ...))

(define-syntax-rule (init-private1 iid)
  (begin (init ([private-iid iid]))
         (define iid private-iid)))
