;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "interfaces.rkt"
         "sql-data.rkt")
(provide prepared-statement%)

;; prepared-statement%
(define prepared-statement%
  (class* object% (prepared-statement<%>)
    (init-private handle            ;; handle, determined by database system
                  param-typeids     ;; (listof typeid)
                  result-dvecs)     ;; (listof vector), layout depends on dbsys
    (init ([-owner owner]))

    (define owner (make-weak-box -owner))
    (define dbsystem (send -owner get-dbsystem))

    (define param-handlers (send dbsystem get-parameter-handlers param-typeids))
    (define result-typeids (send dbsystem field-dvecs->typeids result-dvecs))

    (define/public (get-handle) handle)
    (define/public (set-handle h) (set! handle h))

    (define/public (get-param-count) (length param-typeids))
    (define/public (get-param-typeids) param-typeids)
    (define/public (get-param-types) (send dbsystem typeids->types param-typeids))

    (define/public (get-result-dvecs) result-dvecs)
    (define/public (get-result-count) (length result-dvecs))
    (define/public (get-result-typeids) result-typeids)
    (define/public (get-result-types)
      (send dbsystem typeids->types result-typeids))

    (define/public (check-results fsym checktype obj)
      (case checktype
        ((recordset)
         (unless (positive? (get-result-count))
           (error fsym "expected statement producing recordset, got ~e" obj)))
        ((column)
         (unless (= (get-result-count) 1)
           (error fsym "expected statement producing recordset with single column, got ~e" obj)))
        (else (void))))

    (define/public (check-owner fsym c obj)
      (unless (eq? c (weak-box-value owner))
        (error fsym "prepared statement owned by another connection: ~e" obj)))

    (define/public (bind fsym params)
      (check-param-count fsym params param-typeids)
      (let* ([params
              (for/list ([handler (in-list param-handlers)]
                         [index (in-naturals)]
                         [param (in-list params)])
                (cond [(sql-null? param) sql-null]
                      [else (handler fsym index param)]))])
        (statement-binding this #f params)))

    (define/private (check-param-count fsym params param-typeids)
      (define len (length params))
      (define tlen (length param-typeids))
      (when (not (= len tlen))
        (error fsym "prepared statement requires ~s parameters, given ~s" tlen len)))

    (define/public (finalize)
      (let ([owner (weak-box-value owner)])
        (when owner
          (send owner free-statement this))))

    (define/public (register-finalizer)
      (thread-resume finalizer-thread)
      (will-register will-executor this (lambda (pst) (send pst finalize))))

    (super-new)
    (register-finalizer)))

;; ----

(define will-executor (make-will-executor))

(define finalizer-thread
  (thread/suspend-to-kill
   (lambda ()
     (let loop ()
       (with-handlers
           ([(lambda (e) #t)
             (lambda (e)
               ((error-display-handler)
                (cond [(exn? e)
                       (format "prepared statement finalizer thread handled exception:\n~a"
                               (exn-message e))]
                      [else
                       "prepared statement finalizer thread handled non-exception"])
                e))])
         (will-execute will-executor)
         (loop))))))
