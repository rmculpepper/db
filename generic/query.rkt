;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         racket/vector
         "interfaces.rkt"
         "sql-data.rkt")
(provide compose-collector-with-conversions
         get-fi-name
         get-fi-type
         prepared-statement-base%)

(define (get-fi-name alist)
  (cond [(assq 'name alist)
         => cdr]
        [else #f]))

(define (get-fi-type alist)
  (cond [(assq '*type* alist)
         => cdr]
        [else #f]))

;; compose-collector-with-conversions : dbsystem Collector -> Collector
(define (compose-collector-with-conversions dbsystem collector)
  (lambda (field-infos binary?)
    (let* ([type-function-v
            (list->vector
             (send dbsystem typeids->type-readers (map get-fi-type field-infos)))]
           [convert-row
            (lambda (row)
              ;; FIXME: vector-map vs vector-map! ... which is better?
              (vector-map! (lambda (field type-reader)
                             (cond [(sql-null? field) sql-null]
                                   [type-reader (type-reader field)]
                                   [else field]))
                           row ;; vector-map! mutates and returns this vector
                           type-function-v))])
      (let-values ([(base combine finish info) (collector field-infos binary?)])
        (values base 
                (if binary?
                    combine
                    (lambda (b argv) (combine b (convert-row argv))))
                finish
                info)))))


;; ========================================

;; prepared-statement-base%
(define prepared-statement-base%
  (class* object% (prepared-statement<%>)
    (init-private param-infos
                  result-infos)
    (init ([-owner owner]))

    (define owner (make-weak-box -owner))
    (define type-writers
      (send (send -owner get-dbsystem)
            typeids->type-writers (map get-fi-type param-infos)))

    (define/public (get-param-count) (length param-infos))
    (define/public (get-param-types) (map get-fi-type param-infos))
    (define/public (get-result-count) (length result-infos))
    (define/public (get-result-types) (map get-fi-type result-infos))

    (define/public (check-owner c)
      (eq? c (weak-box-value owner)))

    (define/public (bind params)
      (check-param-count params param-infos)
      (let* ([params
              (map (lambda (tw p)
                     (cond [(sql-null? p) sql-null]
                           [else (tw p)]))
                   type-writers
                   params)])
        (statement-binding this #f params)))

    (define/private (check-param-count params param-infos)
      (define len (length params))
      (define tlen (length param-infos))
      (when (not (= len tlen))
        (error 'bind-prepared-statement
               "prepared statement requires ~s parameters, given ~s" tlen len)))

    (super-new)))
