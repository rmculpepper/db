;; Copyright 2000-2010 Ryan Culpepper
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

         connector<%>
         ssl-connector<%>
         connection:admin<%>
         primitive-query<%>)

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

    ;; query* : (listof Statement) Collector -> (listof QueryResult)
    query*

    ;; prepare-multiple : (listof Preparable) -> (listof PreparedStatement)
    prepare-multiple))


;; ==== DBSystem Interface

;; dbsystem<%>
;; Represents brand of database system, SQL dialect, etc
(define dbsystem<%>
  (interface ()
    ;; get-short-name : -> symbol
    get-short-name

    ;; get-description : -> string
    get-description

    ;; typeid->type : TypeID -> symbol
    typeid->type

    ;; typealias->type : TypeAlias -> symbol
    typealias->type

    ;; get-known-types : #:can-read? [bool #t] #:can-write? [bool #t]
    ;;                -> (listof symbol)
    get-known-types

    ;; get-type-reader : symbol #:options [any #f] -> (string -> any)
    get-type-reader

    ;; get-type-writer : symbol #:options [any #f] -> (string -> any)
    get-type-writer

    ;; has-support? : any -> boolean?
    has-support?))

;; ==== Auxiliary Interfaces & Structures

;; prepared-statement<%>
(define prepared-statement<%>
  (interface ()
    ;; get-result-count : -> number/#f
    get-result-count

    ;; bind : (listof param) -> StatementBinding
    bind))

;; A statement is one of:
;;   - string
;;   - (statement-binding prepared-statement (list-of string))
(struct statement-binding (pst params))

;; A YesNoOptional is one of 'yes, 'no, 'optional
;; An SSLMode is one of 'sslv2-or-v3, 'sslv2, 'sslv3, 'tls

;; A query-result is one of:
;;  - (simple-result string)
;;  - (recordset Header value)
(struct simple-result (command) #:transparent)
(struct recordset (info data) #:transparent)

;; A Header is one of
;;  - field-info
;;  - list of field-info
;;  - #f

;; A field-info is (field-info string)
(struct field-info (name) #:transparent)

;; A Statement is one of
;;  - string
;;  - statement-binding

;; A Collector = RowDescription boolean -> b (b a ... -> b) (b -> c) Header


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

(define primitive-query<%>
  (interface ()
    ;; query* entry point from connection<%>
    query*

    ;; query*/no-conversion : (listof statement) Collector
    ;;                     -> (listof query-result)
    ;; Hook method to be overridden.
    query*/no-conversion

    ;; get-type-writers : (listof TypeId) -> (listof (datum -> string))
    ;; For binding prepared statements.
    ;; Could be moved directly to dbsystem, bypass connections (???)
    get-type-writers))
