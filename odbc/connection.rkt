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
         "ffi.rkt"
         "ffi-constants.rkt"
         "dbsystem.rkt")
(provide connection%
         handle-status
         dbsystem)

(define will-executor (make-will-executor))

(define finalizer-thread
  (thread/suspend-to-kill
   (lambda ()
     (let loop ()
       (will-execute will-executor)
       (loop)))))

(define prepared-statement%
  (class prepared-statement-base%
    (init-private stmt) ;; a hstmt
    (define/public (get-stmt) stmt)

    (define/public (finalize)
      (call-as-atomic
       (lambda ()
         (let ([stmt* stmt])
           (when stmt*
             (set! stmt #f)
             (handle-status 'finalize-statement (SQLFreeStmt stmt* SQL_CLOSE) stmt*)
             (handle-status 'finalize-statement (SQLFreeHandle SQL_HANDLE_STMT stmt*) stmt*)
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
                    [sb (send pst bind fsym null)])
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
         (let* ([db (get-db fsym)]
                [stmt (send pst get-stmt)]
                ;; FIXME: reset/clear
                [pinned-buffers
                 (for/list ([i (in-naturals 1)]
                            [param (in-list params)]
                            #:when #t
                            [buf (in-list (load-param fsym db stmt i param))])
                   buf)]
                [result-infos (send pst get-result-infos)]
                [_ (handle-status fsym (SQLExecute stmt) stmt)]
                [rows (fetch* fsym stmt (send pst get-result-typeids))])
           (handle-status fsym (SQLFreeStmt stmt SQL_CLOSE) stmt)
           (handle-status fsym (SQLFreeStmt stmt SQL_RESET_PARAMS) stmt)
           (for ([buf (in-list pinned-buffers)]) (free buf))
           (values result-infos rows))))
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
      ;; FIXME: is there an easier way to alloc immobile bytes?
      ;; param buffers must not move between bind and execute
      (define (copy-buffer buffer)
        (let* ([n (bytes-length buffer)]
               [copy (make-sized-byte-string (malloc n 'raw) n)])
          (memcpy copy buffer n)
          copy))
      (let ([buffer (copy-buffer (param-buffer param))]
            [lenbuffer #f #| (copy-buffer (param-lenbuffer param)) |#]) ;; FIXME
        (SQLBindParameter stmt i SQL_PARAM_INPUT
                          (param-ctype param)
                          (param-sqltype param)
                          (param-size param)
                          (param-digits param)
                          buffer
                          lenbuffer)
        (list buffer lenbuffer)))

    (define/private (fetch* fsym stmt result-typeids)
      (let ([c (fetch fsym stmt result-typeids)])
        (if c (cons c (fetch* fsym stmt result-typeids)) null)))

    (define/private (fetch fsym stmt result-typeids)
      (let ([s (SQLFetch stmt)])
        (cond [(= s SQL_NO_DATA) #f]
              [(= s SQL_SUCCESS)
               (let* ([column-count (length result-typeids)]
                      [vec (make-vector column-count)])
                 (for ([i (in-range column-count)]
                       [typeid (in-list result-typeids)])
                   (vector-set! vec i (get-column fsym stmt (add1 i) typeid)))
                 vec)]
              [else (handle-status fsym s stmt)])))

    (define/private (get-column fsym stmt i typeid)
      (let-values ([(status len-or-ind)
                    (SQLGetData stmt i SQL_C_CHAR #f)])  ;; FIXME: proper ctypes
        (handle-status fsym status stmt #:ignore-ok/info #t)
        (cond [(= len-or-ind SQL_NULL_DATA)
               sql-null]
              [(= len-or-ind SQL_NO_TOTAL)
               (error 'get-column "NO_TOTAL")]
              [else
               (let ([buf (make-bytes (add1 len-or-ind))]) ;; for nul-terminated string
                 (let-values ([(status _li) (SQLGetData stmt i SQL_C_CHAR buf)]) ;; FIXME
                   (handle-status fsym status stmt)
                   ;; FIXME: convert
                   buf))])))

    (define/public (prepare* fsym stmts)
      (for/list ([stmt stmts])
        (prepare1 fsym stmt)))

    (define/private (prepare1 fsym sql)
      (with-lock
       ;; no time between prepare and table entry
       (let* ([stmt
               (let*-values ([(db) (get-db fsym)]
                             [(status stmt) (SQLAllocHandle SQL_HANDLE_STMT db)])
                 ;; FIXME: if error, free stmt handle
                 (handle-status fsym status db)
                 (let ([status (SQLPrepare stmt sql)])
                   (handle-status fsym status stmt)
                   stmt))]
              [param-infos
               (let-values ([(status param-count) (SQLNumParams stmt)])
                 (handle-status fsym status stmt)
                 (for/list ([i (in-range 1 (add1 param-count))])
                   (describe-param fsym stmt i)))]
              [result-infos
               (let-values ([(status result-count) (SQLNumResultCols stmt)])
                 (handle-status fsym status stmt)
                 (for/list ([i (in-range 1 (add1 result-count))])
                   (describe-result-column fsym stmt i)))])
         (let ([pst (new prepared-statement%
                         (stmt stmt)
                         (owner this)
                         (param-infos param-infos)
                         (result-infos result-infos))])
           (hash-set! statement-table pst #t)
           (thread-resume finalizer-thread)
           (will-register will-executor pst (lambda (pst) (send pst finalize)))
           pst))))

    (define/private (describe-param fsym stmt i)
      (let-values ([(status type size digits nullable)
                    (SQLDescribeParam stmt i)])
        (handle-status fsym status stmt)
        `((*type* . ,type) (size . ,size) (digits . ,digits))))

    (define/private (describe-result-column fsym stmt i)
      (let-values ([(status name type size digits nullable)
                    (SQLDescribeCol stmt i)])
        (handle-status fsym status stmt)
        `((name . ,name) (*type* . ,type) (size . ,size) (digits . ,digits))))

    (define/public (disconnect)
      (with-lock
       (when -db
         (let ([db -db]
               [statements (hash-map statement-table (lambda (k v) k))])
           (set! -db #f)
           (set! statement-table #f)
           (for ([pst (in-list statements)])
             (send pst finalize))
           (handle-status 'disconnect (SQLDisconnect db) db)
           (void)))))

    (super-new)

    (register-finalizer this (lambda (obj) (send obj disconnect)))))

;; ----------------------------------------

;; handle-status : symbol integer -> integer
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status who s [handle #f] #:ignore-ok/info [ignore-ok/info? #f])
  (cond [(= s SQL_SUCCESS_WITH_INFO)
         (when (and handle (not ignore-ok/info?))
           (diag-info who handle 'print))
         s]
        [(= s SQL_ERROR)
         (when handle (diag-info who handle 'error))
         (error who "error: ~e" s)]
        [else s]))
;; FIXME: check codes, what to allow, what to get error on

(define (diag-info who handle mode)
  (let ([handle-type
         (cond [(sqlhenv? handle) SQL_HANDLE_ENV]
               [(sqlhdbc? handle) SQL_HANDLE_DBC]
               [(sqlhstmt? handle) SQL_HANDLE_STMT]
               [else
                (error 'diag-info "internal error: unknown handle type: ~e" handle)])])
    (let-values ([(status sqlstate native-errcode message)
                  (SQLGetDiagRec handle-type handle 1)])
      (case mode
        ((error)
         (error who "~a: ~a" sqlstate message))
        ((print)
         (eprintf "~a: ~a: ~a\n" who sqlstate message))))))
