;; Copyright 2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         (planet jaymccarthy/sqlite:4)
         "../generic/query.rkt"
         "../generic/interfaces.rkt")
(provide connection%
         dbsystem)

(define prepared-statement%
  (class* object% (prepared-statement<%>)
    (init real-pst
          ;;param-count
          owner)

    (define -real-pst real-pst)
    ;; (define -param-count param-count)
    (define -owner (make-weak-box owner))

    (define/public (get-result-count) #f)
    (define/public (get-real-pst) -real-pst)

    (define/public (check-owner c)
      (eq? c (weak-box-value -owner)))

    (define/public (bind params)
      #|
      (unless (= (length params) -param-count)
        (raise-user-error 'bind-prepared-statement
                          "prepared statement requires ~s arguments, given ~s"
                          -param-count (length params)))
      |#
      (make-StatementBinding this params))

    (super-new)))

(define sqlite-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'sqlite)
    (define/public (get-description) "SQLite")
    (define/public (typeid->type x) #f)
    (define/public (typealias->type x) #f)
    (define/public (get-known-types) '())
    (define/public (get-type-reader) #f)
    (define/public (get-type-writer) #f)
    (define/public (has-support? x) #f)
    (super-new)))

(define dbsystem
  (new sqlite-dbsystem%))

;; == Connection

(define base%
  (class* object% (connection:admin<%>)
    (init db)

    (define -db db)

    (define/public (get-db) -db)

    (define/public (get-dbsystem) dbsystem)
    (define/public (connected?) (and -db #t))
    (define/public (disconnect)
      (when -db
        (close -db)
        (set! -db #f)))

    (super-new)))

(define (query-mixin %)
  (class %
    (inherit get-db)

    (define/public (query* fsym stmts collector)
      (let ([db (get-db)])
        (for/list ([stmt (in-list stmts)])
          (query1 db fsym stmt collector))))

    (define/private (query1 db fsym stmt collector)
      (cond [(string? stmt)
             (let ([real-pst (prepare db stmt)])
               (begin0
                   (query1/p db fsym real-pst null collector)
                 (finalize real-pst)))]
            [(StatementBinding? stmt)
             (let ([pst (StatementBinding-pst stmt)]
                   [params (StatementBinding-params stmt)])
               (unless (is-a? pst prepared-statement%)
                 (raise-type-error fsym
                                   "StatementBinding containing prepared statement"
                                   stmt))
               (unless (send pst check-owner this)
                 (raise-mismatch-error fsym
                                       "prepared statement owned by another connection"
                                       stmt))
               (query1/p db fsym (send pst get-real-pst) params collector))]))

    (define/private (query1/p db fsym real-pst params collector)
      (apply load-params real-pst params)
      (let ([columns-v (statement-names real-pst)]
            [rows (step* real-pst)])
        (reset real-pst)
        (let-values ([(init combine finalize info)
                      (collector (for/list ([name (in-vector columns-v)])
                                   `((name ,name)))
                                 #f)])
          (make-Recordset
           info
           (finalize
            (for/fold ([accum init]) ([row (in-list rows)])
              (apply combine accum (vector->list row))))))))

    (define/public (prepare-multiple stmts)
      (let ([db (get-db)])
        (for/list ([stmt stmts])
          (let ([real-pst (prepare (get-db) stmt)])
            (new prepared-statement%
                 (real-pst real-pst)
                 (owner this))))))

    (super-new)))

(define connection%
  (class* (query-mixin base%) (connection<%>)
    (super-new)))
