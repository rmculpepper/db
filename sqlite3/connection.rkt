;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/atomic
         "../generic/query.rkt"
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "ffi.rkt")
(provide connection%
         handle-status
         dbsystem)

(define sqlite-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'sqlite3)
    (define/public (typeids->types x) null)
    (define/public (typeids->type-readers x) null)
    (define/public (typeids->type-writers x) null)
    (define/public (get-known-types) '())
    (define/public (has-support? x) #f)
    (super-new)))

(define dbsystem
  (new sqlite-dbsystem%))

;; ----

(define statement-will-executor (make-will-executor))

(define statement-finalizer-thread
  (thread/suspend-to-kill
   (lambda ()
     (let loop ()
       (will-execute statement-will-executor)
       (loop)))))

(define prepared-statement%
  (class* object% (prepared-statement<%>)
    (init stmt ;; a sqlite_stmt
          owner)

    (define -stmt stmt)
    (define -owner (make-weak-box owner))

    (define param-count
      (sqlite3_bind_parameter_count -stmt))
    (define result-count ;; ???
      (sqlite3_column_count stmt))

    (define/public (get-param-count) param-count)
    (define/public (get-param-typeids) #f)
    (define/public (get-param-types) #f)
    (define/public (get-result-count) result-count)
    (define/public (get-result-typeids) #f)
    (define/public (get-result-types) #f)

    (define/public (get-stmt) -stmt)

    (define/public (check-owner c)
      (eq? c (weak-box-value -owner)))

    (define/public (bind params)
      (unless (= (length params) param-count)
        (raise-user-error 'bind-prepared-statement
                          "prepared statement requires ~s arguments, given ~s"
                          param-count (length params)))
      (statement-binding this params))

    (define/public (finalize)
      (call-as-atomic
       (lambda ()
         (let ([stmt -stmt])
           (when stmt
             (set! -stmt #f)
             (sqlite3_finalize stmt)
             (void))))))

    (super-new)))

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
      (unless -db
        (error fsym "not connected"))
      -db)

    (define/public (get-dbsystem) dbsystem)
    (define/public (connected?) (and -db #t))

    (define/public (query* fsym stmts collector)
      (for/list ([stmt (in-list stmts)])
        (query1 fsym stmt collector)))

    (define/private (query1 fsym stmt collector)
      (cond [(string? stmt)
             (let* ([pst (prepare1 fsym stmt)]
                    [sb (send pst bind null)])
               (query1 fsym sb collector))]
            [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)]
                   [params (statement-binding-params stmt)])
               (unless (and (is-a? pst prepared-statement%)
                            (send pst check-owner this))
                 (raise-mismatch-error fsym
                                       "prepared statement owned by another connection"
                                       stmt))
               (query1/p fsym pst params collector))]))

    (define/private (query1/p fsym pst params collector)
      (define-values (info0 rows)
        (with-lock
         (let ([db (get-db fsym)]
               [stmt (send pst get-stmt)])
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
             (values info rows)))))
      (let-values ([(init combine finalize info)
                    (collector info0 #f)])
        (cond [(or (pair? info0) (pair? rows))
               (recordset
                info
                (finalize
                 (for/fold ([accum init]) ([row (in-list rows)])
                   (combine accum row))))]
              [else
               (simple-result '())])))

    (define/private (load-param fsym db stmt i param)
      (handle-status
       fsym
       (cond [(integer64? param)
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
              (error fsym "bad parameter: ~e" param)])
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
                                         (error 'query* "unknown column type: ~e" type)]))))
                 vec)]
              [else (handle-status fsym s db)])))

    (define/public (prepare* fsym stmts)
      (for/list ([stmt stmts])
        (prepare1 fsym stmt)))

    (define/private (prepare1 fsym sql)
      (with-lock
       ;; no time between sqlite3_prepare and table entry
       (let-values ([(db) (get-db fsym)]
                    [(stmt prep-status tail)
                     (sqlite3_prepare_v2 (get-db fsym) sql)])
         (define (free!) (sqlite3_finalize stmt))
         (when (string=? sql tail)
           (free!) (error fsym "SQL syntax error in ~e" tail))
         (when (not (zero? (string-length tail)))
           (free!) (error fsym "multiple SQL statements given: ~e" tail))
         (when (handle-status fsym prep-status db)
           (or stmt
               (begin (free!) (error fsym "internal error in prepare"))))
         (let ([pst (new prepared-statement%
                         (stmt stmt)
                         (owner this))])
           (hash-set! statement-table pst #t)
           (thread-resume statement-finalizer-thread)
           (will-register statement-will-executor pst (lambda (pst) (send pst finalize)))
           pst))))

    (define/public (disconnect)
      (with-lock
       (when -db
         (let ([db -db]
               [statements (hash-map statement-table (lambda (k v) k))])
           (set! -db #f)
           (set! statement-table #f)
           (for ([pst (in-list statements)])
             (send pst finalize))
           (handle-status 'disconnect (sqlite3_close db))
           (void)))))

    (super-new)

    (register-finalizer this (lambda (obj) (send obj disconnect)))))

;; ----------------------------------------

;; handle-status : symbol integer -> integer
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status who s [db #f])
  (if (or (= s SQLITE_OK)
          (= s SQLITE_ROW)
          (= s SQLITE_DONE))
      s
      (error who "~a" (lookup-status-message s db))))

;; lookup-status-message : integer db/#f -> string
(define (lookup-status-message s db)
  (if (and (eq? s SQLITE_ERROR) db)
      (sqlite3_errmsg db)
      (cdr (assoc s
                  `([,SQLITE_ERROR . "Generic error, perhaps call errmsg?"]
                    [,SQLITE_INTERNAL . "An internal logic error in SQLite"]
                    [,SQLITE_PERM . "Access permission denied"]
                    [,SQLITE_ABORT . "Callback routine requested an abort"]
                    [,SQLITE_BUSY . "The database file is locked"]
                    [,SQLITE_LOCKED . "table in the database is locked"]
                    [,SQLITE_NOMEM . "A malloc() failed"]
                    [,SQLITE_READONLY . "Attempt to write a readonly database"]
                    [,SQLITE_INTERRUPT . "Operation terminated by sqlite3_interrupt()"]
                    [,SQLITE_IOERR . "Some kind of disk I/O error occurred"]
                    [,SQLITE_CORRUPT . "The database disk image is malformed"]
                    [,SQLITE_NOTFOUND . "(Internal Only) Table or record not found"]
                    [,SQLITE_FULL . "Insertion failed because database is full"]
                    [,SQLITE_CANTOPEN . "Unable to open the database file"]
                    [,SQLITE_PROTOCOL . "Database lock protocol error"]
                    [,SQLITE_EMPTY . "Database is empty"]
                    [,SQLITE_SCHEMA . "The database schema changed"]
                    [,SQLITE_TOOBIG . "Too much data for one row of a table"]
                    [,SQLITE_CONSTRAINT . "Abort due to contraint violation"]
                    [,SQLITE_MISMATCH . "Data type mismatch"]
                    [,SQLITE_MISUSE . "Library used incorrectly"]
                    [,SQLITE_NOLFS . "Uses OS features not supported on host"]
                    [,SQLITE_AUTH . "Authorization denied"]
                    [,SQLITE_FORMAT . "Auxiliary database format error"]
                    [,SQLITE_RANGE . "2nd parameter to sqlite3_bind out of range"]
                    [,SQLITE_NOTADB . "File opened that is not a database file"])))))

(define integer64?
  (let ([min-64 (- 0 (expt 2 63))]
        [max-64 (- (expt 2 63) 1)])
    (lambda (x)
      (and (exact-integer? x)
           (>= x min-64)
           (<= x max-64)))))
