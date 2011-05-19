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

         no-cache-prepare<%>
         connector<%>

         make-handler)

;; ==== Connection

;; connection<%>
(define connection<%>
  (interface ()
    connected?    ;; -> boolean
    disconnect    ;; -> void
    get-dbsystem  ;; -> dbsystem<%>
    query         ;; symbol statement collector -> QueryResult
    prepare       ;; symbol preparable boolean -> prepared-statement<%>

    start-transaction  ;; symbol -> void
    end-transaction    ;; symbol (U 'commit 'rollback) -> (U 'commit 'rollback)
    transaction-status ;; symbol -> boolean

    free-statement)) ;; prepared-statement<%> -> void

;; no-cache-prepare<%>
;; Interface to identify connections such as connection-generators:
;; prepare method must be called with close-on-exec? = #t and result must
;; not be cached.
(define no-cache-prepare<%>
  (interface ()))

;; ==== DBSystem

;; dbsystem<%>
;; Represents brand of database system, SQL dialect, etc
(define dbsystem<%>
  (interface ()
    get-short-name         ;; -> symbol

    get-parameter-handlers ;; (listof typeid) -> (listof ParameterHandler)
    field-dvecs->typeids   ;; (listof field-dvec) -> (listof typeid)

    ;; inspection only
    get-known-types        ;; -> (listof symbol)
    describe-typeids))     ;; (listof typeid) -> (listof TypeDesc)


;; ParameterHandler = (fsym index datum -> ???)
;; Each system gets to choose its checked-param representation.
;; Maybe check and convert to string. Maybe just check, do binary conversion later.

;; TypeDesc = (list boolean symbol/#f typeid)

;; ==== Prepared

;; prepared-statement<%>
(define prepared-statement<%>
  (interface ()
    get-handle         ;; -> Handle (depends on database system)
    set-handle         ;; Handle -> void

    after-exec         ;; -> void (for close-after-exec)

    get-param-count    ;; -> nat or #f
    get-param-typeids  ;; -> (listof typeid)

    get-result-dvecs   ;; -> (listof vector)
    get-result-count   ;; -> nat or #f
    get-result-typeids ;; -> (listof typeid) or #f

    check-owner        ;; symbol connection any -> #t (or error)
    bind               ;; symbol (listof param) -> statement-binding

    ;; extension hooks: usually shouldn't need to override
    finalize           ;; -> void
    register-finalizer ;; -> void

    ;; inspection only
    get-param-types    ;; -> (listof TypeDesc)
    get-result-types   ;; -> (listof TypeDesc)
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
                                        describe-typeid)
                      (typeid type (alias ...) supported?) ...)
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
    (define (describe-typeid x)
      (let ([t (typeid->type x)]
            [ok? (case x ((typeid) supported?) ... (else #f))])
        (list ok? t x)))))


;; == Internal staging interfaces

;; connector<%>
;; Manages making connections
(define connector<%>
  (interface ()
    attach-to-ports            ;; input-port output-port -> void
    start-connection-protocol  ;; string string string/#f -> void
    ))

;; == Notice/notification handler maker

;; make-handler : output-port/symbol string -> string string -> void
(define (make-handler out header)
  (if (procedure? out)
      out
      (lambda (code message)
        (fprintf (case out
                   ((output) (current-output-port))
                   ((error) (current-error-port))
                   (else out))
                 "~a: ~a (SQLSTATE ~a)\n" header message code))))
