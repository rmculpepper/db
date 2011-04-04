;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/vector
         "interfaces.rkt"
         "sql-data.rkt")
(provide compose-collector-with-conversions
         get-fi-name
         get-fi-typeid)

(define (get-fi-name alist)
  (cond [(assq 'name alist)
         => cdr]
        [else #f]))

(define (get-fi-typeid alist)
  (cond [(assq '*type* alist)
         => cdr]
        [else #f]))

;; compose-collector-with-conversions : dbsystem Collector -> Collector
(define (compose-collector-with-conversions dbsystem collector)
  (lambda (field-infos binary?)
    (let* ([type-function-v
            (list->vector
             (send dbsystem get-result-handlers field-infos))]
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

