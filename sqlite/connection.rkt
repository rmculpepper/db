;; Copyright 2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         "../generic/query.rkt"
         "../generic/interfaces.rkt")
(provide connection%
         dbsystem)

;; FIXME
(define sqlite-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'sqlite)
    (define/public (get-description) "SQLite")
    (define/public (typeid->type x) x)
    (define/public (typealias->type x) x)
    (define/public (get-known-types) '(varchar integer real numeric))
    (define/public (get-type-reader) values)
    (define/public (get-type-writer) values)
    (define/public (has-support? x) #f)
    (super-new)))

(define dbsystem
  (new sqlite-dbsystem%))

;; ----

#|
Statement management

Disconnecting requires finalizing all statements.  The statement-table
stores reachable statements. The global statement-will-executor will
finalize unreachable statements... eventually. But we must watch out
for statements that have become unreachable but have not been
finalized by the will-executor, however.

|#

(define statement-will-executor (make-will-executor))
(thread
 (lambda ()
   (let loop ()
     (will-execute statement-will-executor)
     (loop))))

(define prepared-statement%
  (class* object% (prepared-statement<%>)
    (init stmt ;; a sqlite_stmt
          counter ;; box of nonnegative integer
          owner)

    (define -stmt stmt)
    (define -counter counter)
    (define param-count
      (sqlite3_bind_parameter_count -stmt))
    (define result-count
      (sqlite3_column_count stmt))
    (define -owner (make-weak-box owner))

    (define/public (get-result-count) result-count)
    (define/public (get-stmt)
      (unless -stmt
        (error 'prepared-statement "the statement has been destroyed"))
      -stmt)

    (define/public (check-owner c)
      (eq? c (weak-box-value -owner)))

    (define/public (bind params)
      (unless (= (length params) param-count)
        (raise-user-error 'bind-prepared-statement
                          "prepared statement requires ~s arguments, given ~s"
                          param-count (length params)))
      (make-StatementBinding this params))

    (define/public (finalize)
      (call-as-atomic
       (lambda ()
         (let ([stmt -stmt])
           (when stmt
             (set! -stmt #f)
             (sqlite3_finalize stmt)
             (set-box! -counter (sub1 (unbox counter)))
             (void))))))

    (super-new)))

;; == Connection

(define connection%
  (class* object% (connection<%>)
    (init db)

    (define -db db)

    (define statement-counter (box 0))
    (define statement-table (make-weak-hasheq))

    (define/public (get-db fsym)
      (unless -db
        (error fsym "not connected"))
      -db)

    (define/public (get-dbsystem) dbsystem)
    (define/public (connected?) (and -db #t))

    (define/public (query* fsym stmts collector)
      (let ([db (get-db fsym)])
        (for/list ([stmt (in-list stmts)])
          (query1 db fsym stmt collector))))

    (define/private (query1 db fsym stmt collector)
      (cond [(string? stmt)
             (let* ([pst (prepare1 stmt)]
                    [sb (send pst bind null)])
               (query1 db fsym sb collector))]
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
              (combine accum row)))))))

    (define/public (prepare-multiple stmts)
      (let ([db (get-db 'prepare-multiple)])
        (for/list ([stmt stmts])
          (prepare1 db stmt))))

    (define/private (prepare1 sql)
      (call-as-atomic
       (lambda ()
         (set-box! statement-counter (add1 (unbox statement-counter)))))
      (let ([stmt (prepare db sql)]
            [pst (new prepared-statement%
                      (stmt stmt)
                      (counter statement-counter)
                      (owner this))])
        (will-register statement-will-executor
                       pst
                       (lambda (pst)
                         (send pst finalize)))
        (hash-set! statement-table s #t)
        pst))

    (define/public (disconnect)
      (when -db
        (let ([db -db]
              [statements (hash-map statement-table (lambda (k v) k))])
          (set! -db #f)
          (set! statement-table #f)
          (for ([pst (in-list statements)])
            (send pst finalize))
          ;; Try to clean up any unreachable but unfinalized stmts.
          ;; (But be aware finalizer thread is doing same, so beware races.)
          (let loop ()
            (unless (zero? (unbox statement-counter))
              (let ([result (sync/timeout 1 statement-will-executor)])
                (cond [(eq? result statement-will-executor)
                       (will-try-execute statement-will-executor)
                       (loop)]
                      [(zero? (unbox statement-counter))
                       'ok]
                      [else
                       (error 'disconnect
                              "internal error: statements could not be finalized")]))))
          (close db))))

    (super-new)))
