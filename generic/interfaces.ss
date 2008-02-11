;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/class)

(provide (struct-out SimpleResult)
         (struct-out Recordset)
         (struct-out FieldInfo)
         
         (struct-out PreparedStatement)
         (struct-out StatementBinding)
         
         backend-link<%>
         backend-link<%>/sub

         connection<%>
         connector<%>
         ssl-connector<%>
         primitive-query<%>
         primitive-query/prepare<%>
         primitive-query/conversion<%>
         
         query<%>
         query/prepare<%>)

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

;; connection<%>
(define connection<%>
  (interface ()
    ;; connected? : -> boolean
    connected?

    ;; disconnect : -> void
    disconnect))

;; backend-link<%>
(define backend-link<%>
  (interface ()
    ;; new-exchange : ??? -> stream
    new-exchange
    
    ;; end-exchange : -> void
    end-exchange
    
    ;; close : -> void
    close
    
    ;; encode : msg -> void
    encode
    
    ;; flush : -> void
    flush
    
    ;; alive? : -> boolean
    alive?
    ))

;; backend-link<%>/sub
(define backend-link<%>/sub
  (interface (backend-link<%>)
    ;; mk-get-next-message : ??? -> (-> msg)
    mk-get-next-message
    
    ;; mk-end-message? : ??? -> (msg -> boolean)
    mk-end-message?
    ))

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
    
    ;; datum->external-representation : number datum -> string
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

;; query/conversion<%>
(define primitive-query/conversion<%>
  (interface ()
    ;; query*/no-conversion
    query*/no-conversion
    
    ;; typeid->type : TypeID -> symbol
    typeid->type
    
    ;; get-type-reader : symbol -> (External -> datum)
    get-type-reader
    
    ;; get-type-writer : symbol -> (datum -> External)
    get-type-writer
    ))

;; query<%>
(define query<%>
  (interface ()
    ;; query : Statement -> QueryResult
    query
    
    ;; query-multiple : (list-of Statement) -> (list-of QueryResult)
    query-multiple
    
    ;; exec : Statement ... -> void
    exec
    
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
    
    ;; map : Statement (value ... -> a) -> (list-of a)
    map
    
    ;; for-each : Statement (value ... -> void) -> void
    for-each
    
    ;; mapfilter : Statement (value ... -> a) (value ... -> boolean)
    ;;           -> (list-of a)
    mapfilter
    
    ;; fold : Statement (a value ... -> a) a -> a
    fold))

;; query/prepare<%>
(define query/prepare<%>
  (interface (query<%>)
    ;; prepare : Preparable -> PreparedStatement
    prepare
    
    ;; prepare-multiple : (list-of Preparable) -> (list-of PreparedStatement)
    prepare-multiple
    
    ;; bind-prepared-statement : PreparedStatement (list-of param) -> Statement
    bind-prepared-statement
    
    ;; prepare-exec : Preparable -> datum ... -> void
    prepare-exec
    
    ;; prepare-query-list : Preparable -> datum ... -> (list-of value)
    prepare-query-list
    
    ;; prepare-query-row : Preparable -> datum ... -> (vector-of value)
    prepare-query-row
    
    ;; prepare-query-maybe-row : Preparable -> datum ... -> (vector-of value) or #f
    prepare-query-maybe-row
    
    ;; prepare-query-value : Preparable -> datum ... -> value
    prepare-query-value
    
    ;; prepare-query-maybe-value : Preparable -> datum ... -> value or #f
    prepare-query-maybe-value
    
    ;; prepare-map : Preparable ('a ... -> 'b) -> datum ... -> (list-of 'b)
    prepare-map
    
    ;; prepare-for-each : Preparable ('a ... -> void) -> datum ... -> void
    prepare-for-each
    
    ;; prepare-mapfilter : Preparable ('a ... -> 'b) ('a ... -> boolean)
    ;;                  -> datum ... -> (list-of 'b)
    prepare-mapfilter
    
    ;; prepare-fold : Preparable ('b 'a ... -> 'b) 'b -> datum ... -> 'b
    prepare-fold))
