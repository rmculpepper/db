;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

;; Based in part on code Copyright 2009, 2010 Jay McCarthy

#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/atomic
         "../generic/interfaces.rkt"
         "../generic/prepared.rkt"
         "../generic/sql-data.rkt"
         "ffi.rkt"
         "dbsystem.rkt")
(provide connection%
         handle-status)

;; == Connection

(define connection%
  (class* object% (connection<%>)
    (init db)

    (define -db db)
    (define statement-table (make-weak-hasheq))
    (define lock (make-semaphore 1))

    (define-syntax-rule (with-lock . body)
      (begin (semaphore-wait lock)
             (with-handlers ([values (lambda (e) (semaphore-post lock) (raise e))])
               (begin0 (let () . body)
                 (semaphore-post lock)))))

    (define/public (get-db fsym)
      (or -db (error/not-connected fsym)))

    (define/public (get-dbsystem) dbsystem)
    (define/public (connected?) (and -db #t))

    (define/public (query fsym stmt collector)
      (let* ([result
              (with-lock
               (let ([db (get-db fsym)])
                 (query1 fsym stmt)))]
             [stmt (car result)]
             [info0 (cadr result)]
             [rows (cddr result)])
        (statement:after-exec stmt)
        (cond [(pair? info0)
               (let-values ([(init combine finalize headers?)
                             (collector (length info0) #t)])
                 (recordset (and headers? info0)
                            (finalize
                             (for/fold ([accum init]) ([row (in-list rows)])
                               (combine accum row)))))]
              [else (simple-result '())])))

    (define/private (query1 fsym stmt)
      (let* ([stmt (cond [(string? stmt)
                          (let* ([pst (prepare1 fsym stmt #t)])
                            (send pst bind fsym null))]
                         [(statement-binding? stmt)
                          stmt])]
             [pst (statement-binding-pst stmt)]
             [params (statement-binding-params stmt)])
        (send pst check-owner fsym this stmt)
        (let ([db (get-db fsym)]
              [stmt (send pst get-handle)])
          (handle-status fsym (sqlite3_reset stmt) db)
          (handle-status fsym (sqlite3_clear_bindings stmt) db)
          (for ([i (in-naturals 1)]
                [param (in-list params)])
            (load-param fsym db stmt i param))
          (let* ([info
                  (for/list ([i (in-range (sqlite3_column_count stmt))])
                    `((name ,(sqlite3_column_name stmt i))
                      (decltype ,(sqlite3_column_decltype stmt i))))]
                 [rows (step* fsym db stmt)])
            (handle-status fsym (sqlite3_reset stmt) db)
            (handle-status fsym (sqlite3_clear_bindings stmt) db)
            (cons stmt (cons info rows))))))

    (define/private (load-param fsym db stmt i param)
      (handle-status
       fsym
       (cond [(int64? param)
              (sqlite3_bind_int64 stmt i param)]
             [(real? param) ;; includes >64-bit exact integers
              (sqlite3_bind_double stmt i (exact->inexact param))]
             [(string? param)
              (sqlite3_bind_text stmt i param)]
             [(bytes? param)
              (sqlite3_bind_blob stmt i param)]
             [(sql-null? param)
              (sqlite3_bind_null stmt i)]
             [else
              (error/internal fsym "bad parameter: ~e" param)])
       db))

    (define/private (step* fsym db stmt)
      (let ([c (step fsym db stmt)])
        (if c (cons c (step* fsym db stmt)) null)))

    (define/private (step fsym db stmt)
      (let ([s (sqlite3_step stmt)])
        (cond [(= s SQLITE_DONE) #f]
              [(= s SQLITE_ROW)
               (let* ([column-count (sqlite3_column_count stmt)]
                      [vec (make-vector column-count)])
                 (for ([i (in-range column-count)])
                   (vector-set! vec i
                                (let ([type (sqlite3_column_type stmt i)])
                                  (cond [(= type SQLITE_NULL)
                                         sql-null]
                                        [(= type SQLITE_INTEGER)
                                         (sqlite3_column_int64 stmt i)]
                                        [(= type SQLITE_FLOAT)
                                         (sqlite3_column_double stmt i)]
                                        [(= type SQLITE_TEXT)
                                         (sqlite3_column_text stmt i)]
                                        [(= type SQLITE_BLOB)
                                         (sqlite3_column_blob stmt i)]
                                        [else
                                         (error/internal
                                          fsym "unknown column type: ~e" type)]))))
                 vec)]
              [else (handle-status fsym s db)])))

    (define/public (prepare fsym stmt close-on-exec?)
      (with-lock (prepare1 fsym stmt close-on-exec?)))

    (define/private (prepare1 fsym sql close-on-exec?)
      ;; no time between sqlite3_prepare and table entry
      (let-values ([(db) (get-db fsym)]
                   [(stmt prep-status tail)
                    (sqlite3_prepare_v2 (get-db fsym) sql)])
        (define (free!) (when stmt (sqlite3_finalize stmt)))
        (when (string=? sql tail)
          (free!) (uerror fsym "SQL syntax error in ~e" tail))
        (when (not (zero? (string-length tail)))
          (free!) (uerror fsym "multiple SQL statements given: ~e" tail))
        (handle-status fsym prep-status db)
        (unless stmt (begin (free!) (error/internal fsym "prepare failed")))
        (let* ([param-typeids
                (for/list ([i (in-range (sqlite3_bind_parameter_count stmt))])
                  'any)]
               [result-dvecs
                (for/list ([i (in-range (sqlite3_column_count stmt))])
                  '#(any))]
               [pst (new prepared-statement%
                         (handle stmt)
                         (close-on-exec? close-on-exec?)
                         (param-typeids param-typeids)
                         (result-dvecs result-dvecs)
                         (owner this))])
          (hash-set! statement-table pst #t)
          pst)))

    (define/public (disconnect)
      (with-lock
       (when -db
         (let ([db -db]
               [statements (hash-map statement-table (lambda (k v) k))])
           (set! -db #f)
           (set! statement-table #f)
           (for ([pst (in-list statements)])
             (let ([stmt (send pst get-handle)])
               (when stmt
                 (send pst set-handle #f)
                 (handle-status 'disconnect (sqlite3_finalize stmt) db))))
           (handle-status 'disconnect (sqlite3_close db) db)
           (void)))))

    (define/public (free-statement pst)
      (with-lock
       (let ([stmt (send pst get-handle)])
         (when stmt
           (send pst set-handle #f)
           (handle-status 'free-statement (sqlite3_finalize stmt) -db)
           (void)))))


    ;; == Transactions

    (define/public (transaction-status fsym)
      (with-lock
       (let ([db (get-db fsym)])
         (not (sqlite3_get_autocommit db)))))

    (define/public (start-transaction fsym flags)
      ;; FIXME: modes are DEFERRED | IMMEDIATE | EXCLUSIVE
      (let ([stmt
             (with-lock
              (let ([db (get-db fsym)])
                (when (not (sqlite3_get_autocommit db))
                  (error/already-in-tx fsym))
                (let ([r (query1 fsym "BEGIN TRANSACTION")])
                  (car r))))])
        (statement:after-exec stmt)
        (void)))

    (define/public (end-transaction fsym mode)
      (let ([stmt
             (with-lock
              (let ([db (get-db fsym)])
                (when (not (sqlite3_get_autocommit db))
                  (let ([r (case mode
                             ((commit)
                              (query1 fsym "COMMIT TRANSACTION"))
                             ((rollback)
                              (query1 fsym "ROLLBACK TRANSACTION")))])
                    (car r)))))])
        (statement:after-exec stmt)
        (void)))

    ;; ----

    (super-new)
    (register-finalizer this (lambda (obj) (send obj disconnect)))

    #|
    (define custodian-shutdown-trick
      ;; When the custodian is shutdown, the box is emptied, and the
      ;; dummy box becomes unreachable, triggering the finalizer.
      (make-custodian-box
       (current-custodian)
       (let ([dummy (box this)])
         (register-finalizer dummy (lambda (dummy) (send (unbox dummy) disconnect)))
         dummy)))
    |#))

;; ----------------------------------------

;; handle-status : symbol integer -> integer
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status who s db)
  (if (or (= s SQLITE_OK)
          (= s SQLITE_ROW)
          (= s SQLITE_DONE))
      s
      (uerror who "~a" (lookup-status-message s db))))

(define error-table
  `([,SQLITE_ERROR . "unknown error"]
    [,SQLITE_INTERNAL . "an internal logic error in SQLite"]
    [,SQLITE_PERM . "access permission denied"]
    [,SQLITE_ABORT . "callback routine requested an abort"]
    [,SQLITE_BUSY . "the database file is locked"]
    [,SQLITE_LOCKED . "table in the database is locked"]
    [,SQLITE_NOMEM . "malloc() failed"]
    [,SQLITE_READONLY . "attempt to write a readonly database"]
    [,SQLITE_INTERRUPT . "operation terminated by sqlite3_interrupt()"]
    [,SQLITE_IOERR . "some kind of disk I/O error occurred"]
    [,SQLITE_CORRUPT . "the database disk image is malformed"]
    [,SQLITE_NOTFOUND . "(internal only) table or record not found"]
    [,SQLITE_FULL . "insertion failed because database is full"]
    [,SQLITE_CANTOPEN . "unable to open the database file"]
    [,SQLITE_PROTOCOL . "database lock protocol error"]
    [,SQLITE_EMPTY . "database is empty"]
    [,SQLITE_SCHEMA . "database schema changed"]
    [,SQLITE_TOOBIG . "too much data for one row of a table"]
    [,SQLITE_CONSTRAINT . "abort due to contraint violation"]
    [,SQLITE_MISMATCH . "data type mismatch"]
    [,SQLITE_MISUSE . "library used incorrectly"]
    [,SQLITE_NOLFS . "uses OS features not supported on host"]
    [,SQLITE_AUTH . "authorization denied"]
    [,SQLITE_FORMAT . "auxiliary database format error"]
    [,SQLITE_RANGE . "2nd parameter to sqlite3_bind out of range"]
    [,SQLITE_NOTADB . "file opened that is not a database file"]))

;; lookup-status-message : integer db/#f -> string
(define (lookup-status-message s db)
  (cond [(and (eq? s SQLITE_ERROR) db)
         (sqlite3_errmsg db)]
        [(assoc s error-table) => cdr]
        [else "unknown condition"]))
