;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class)

(provide connection:admin<%>
         connection:query<%>
         connection:query/prepare<%>

         dbsystem<%>

         (struct-out SimpleResult)
         (struct-out Recordset)
         (struct-out FieldInfo)

         (struct-out PreparedStatement)
         (struct-out StatementBinding)

         connector<%>
         ssl-connector<%>
         primitive-query<%>
         primitive-query/prepare<%>)

;; ==== Connection Interfaces

;; connection<%>
(define connection:admin<%>
  (interface ()
    ;; connected? : -> boolean
    connected?

    ;; disconnect : -> void
    disconnect

    ;; get-system : -> (is-a/c dbsystem<%>)
    get-system
    ))

;; connection:query<%>
(define connection:query<%>
  (interface ()
    ;; query : Statement -> QueryResult
    query

    ;; query-multiple : (list-of Statement) -> (list-of QueryResult)
    query-multiple

    ;; query-exec : Statement ... -> void
    query-exec

    ;; query-rows : Statement -> (listof (vectorof value))
    query-rows

    ;; query-list : Statement -> (list-of value)
    query-list

    ;; query-row : Statement -> (vector-of value)
    query-row

    ;; query-maybe-row : Statement -> (vector-of value) or #f
    query-maybe-row

    ;; query-value : Statement -> value
    query-value

    ;; query-maybe-value : Statement -> value or #f
    query-maybe-value

    ;; query-map : Statement (value ... -> a) -> (list-of a)
    query-map

    ;; query-for-each : Statement (value ... -> void) -> void
    query-for-each

    ;; query-mapfilter : Statement (value ... -> a) (value ... -> boolean)
    ;;           -> (list-of a)
    query-mapfilter

    ;; query-fold : Statement (a value ... -> a) a -> a
    query-fold))

;; connection:query/prepare<%>
(define connection:query/prepare<%>
  (interface (connection:query<%>)
    ;; prepare : Preparable -> PreparedStatement
    prepare

    ;; prepare-multiple : (list-of Preparable) -> (list-of PreparedStatement)
    prepare-multiple

    ;; bind-prepared-statement : PreparedStatement (list-of param) -> Statement
    bind-prepared-statement

    ;; prepare-query-exec : Preparable -> datum ... -> void
    prepare-query-exec

    ;; prepare-query-rows : Preparable -> datum ... -> void
    prepare-query-rows

    ;; prepare-query-list : Preparable -> datum ... -> (list-of value)
    prepare-query-list

    ;; prepare-query-row : Preparable -> datum ... -> (vector-of value)
    prepare-query-row

    ;; prepare-query-maybe-row : Preparable
    ;;                        -> datum ... -> (vectorof value) or #f
    prepare-query-maybe-row

    ;; prepare-query-value : Preparable -> datum ... -> value
    prepare-query-value

    ;; prepare-query-maybe-value : Preparable -> datum ... -> value or #f
    prepare-query-maybe-value

    ;; prepare-query-map : Preparable ('a ... -> 'b)
    ;;                  -> datum ... -> (list-of 'b)
    prepare-query-map

    ;; prepare-query-for-each : Preparable ('a ... -> void)
    ;;                       -> datum ... -> void
    prepare-query-for-each

    ;; prepare-query-mapfilter : Preparable ('a ... -> 'b) ('a ... -> boolean)
    ;;                        -> datum ... -> (list-of 'b)
    prepare-query-mapfilter

    ;; prepare-query-fold : Preparable ('b 'a ... -> 'b) 'b -> datum ... -> 'b
    prepare-query-fold))

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

    ;; sql:escape-name : string #:preserve-case [boolean #f] -> string
    sql:escape-name

    ;; sql:literal-expression : string/symbol any -> string
    ;; Constructs SQL expression evaluating to given value
    sql:literal-expression

    ;; has-support? : any -> boolean?
    has-support?))

;; ==== Auxiliary Interfaces & Structures

;; A PreparedStatement is:
;;   (make-PreparedStatement number/#f)
(define-struct PreparedStatement (results))

;; A Statement is one of:
;;   - string
;;   - (make-StatementBinding PreparedStatement (list-of string))
(define-struct StatementBinding (pst params) #:transparent)

;; A YesNoOptional is one of 'yes, 'no, 'optional
;; An SSLMode is one of 'sslv2-or-v3, 'sslv2, 'sslv3, 'tls

;; A QueryResult is one of:
;;  - (make-SimpleResult string)
;;  - (make-Recordset Header value)
(define-struct SimpleResult (command) #:transparent)
(define-struct Recordset (info data) #:transparent)

;; A Header is one of
;;  - FieldInfo
;;  - list of FieldInfo
;;  - #f

;; A FieldInfo is (make-FieldInfo string)
(define-struct FieldInfo (name) #:transparent)

;; A Statement is one of
;;  - string
;;  - StatementBinding

;; A Preparable is one of
;;  - string

;; A Collector = RowDescription boolean -> b (b a ... -> b) (b -> c) Header


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

;; primitive-query<%>
(define primitive-query<%>
  (interface ()
    ;; query* : (listof Statement) Collector -> (listof QueryResult)
    query*

    ;; query*/no-conversion : (listof Statement) Collector
    ;;                     -> (listof QueryResult)
    query*/no-conversion

    ;; datum->external-representation : TypeID datum -> string
    datum->external-representation
    ))

;; primitive-query/prepare<%>
(define primitive-query/prepare<%>
  (interface (primitive-query<%>)
    ;; prepare-multiple : (listof Preparable) -> (listof PreparedStatement)
    prepare-multiple

    ;; bind-prepared-statement : PreparedStatement (list-of value) -> Statement
    bind-prepared-statement
    ))
