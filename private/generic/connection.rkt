;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "interfaces.rkt"
         "query.rkt"
         "sql-data.rkt")
(provide connector<%>
         connection:admin<%>

         prepared-statement-base%)

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

;; connection:admin<%>
(define connection:admin<%>
  (interface ()
    connected?
    disconnect
    get-dbsystem))


;; ========================================

;; prepared-statement-base%
(define prepared-statement-base%
  (class* object% (prepared-statement<%>)
    (init-private param-infos   ;; list
                  result-infos) ;; list or #f
    (init ([-owner owner]))

    (define owner (make-weak-box -owner))
    (define dbsystem (send -owner get-dbsystem))

    (define param-handlers (send dbsystem get-parameter-handlers param-infos))
    (define result-typeids (and result-infos (map get-fi-typeid result-infos)))

    (define/public (get-param-infos) param-infos)
    (define/public (get-param-count) (length param-infos))
    (define/public (get-param-types)
      (send dbsystem typeids->types (map get-fi-typeid param-infos)))

    (define/public (get-result-infos) result-infos)
    (define/public (get-result-count) (and result-infos (length result-infos)))
    (define/public (get-result-typeids) result-typeids)
    (define/public (get-result-types)
      (and (pair? result-typeids)
           (send dbsystem typeids->types result-typeids)))

    (define/public (check-owner fsym c obj)
      (or (eq? c (weak-box-value owner))
          (error fsym "prepared statement owned by another connection: ~e" obj)))

    (define/public (bind fsym params)
      (check-param-count fsym params param-infos)
      (let* ([params
              (for/list ([handler (in-list param-handlers)]
                         [index (in-naturals)]
                         [param-info (in-list param-infos)]
                         [param (in-list params)])
                (cond [(sql-null? param) sql-null]
                      [else (handler fsym index param-info param)]))])
        (statement-binding this #f params)))

    (define/private (check-param-count fsym params param-infos)
      (define len (length params))
      (define tlen (length param-infos))
      (when (not (= len tlen))
        (error fsym "prepared statement requires ~s parameters, given ~s" tlen len)))

    (super-new)))
