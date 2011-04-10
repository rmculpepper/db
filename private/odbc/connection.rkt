;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/atomic
         "../generic/interfaces.rkt"
         "../generic/prepared.rkt"
         "../generic/sql-data.rkt"
         "../generic/sql-convert.rkt"
         "../generic/io.rkt"
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
    (init env)

    (define -db db)
    (define -env env)
    (define statement-table (make-weak-hasheq))
    (define lock (make-semaphore 1))

    (define has-describe-param?
      (let-values ([(status supported?) (SQLGetFunctions db SQL_API_SQLDESCRIBEPARAM)])
        (handle-status 'connect status db)
        supported?))

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
               (send pst check-owner fsym this stmt)
               (query1/p fsym pst params collector))]))

    (define/private (query1/p fsym pst params collector)
      (define-values (dvecs rows)
        (with-lock
         (let* ([db (get-db fsym)]
                [stmt (send pst get-stmt)]
                ;; FIXME: reset/clear
                [result-dvecs (send pst get-result-dvecs)])
           (for ([i (in-naturals 1)]
                 [param (in-list params)])
             (load-param fsym db stmt i param))
           (handle-status fsym (SQLExecute stmt) stmt)
           (let ([rows (fetch* fsym stmt (send pst get-result-typeids))])
             (handle-status fsym (SQLFreeStmt stmt SQL_CLOSE) stmt)
             (handle-status fsym (SQLFreeStmt stmt SQL_RESET_PARAMS) stmt)
             (values result-dvecs rows)))))
      (let-values ([(init combine finalize headers?)
                    (collector (length dvecs) #t)])
        (cond [(pair? dvecs)
               (recordset (and headers? (map field-dvec->field-info dvecs))
                          (finalize
                           (for/fold ([accum init]) ([row (in-list rows)])
                             (combine accum row))))]
              [else (simple-result '())])))

    (define/private (load-param fsym db stmt i param)
      ;; FIXME: for now we assume typeid is SQL_UNKNOWN_TYPE, but should
      ;; have paraminfos around in case (also need size, digits, etc?)
      ;; NOTE: param buffers must not move between bind and execute
      (define (copy-buffer buffer)
        (let* ([buffer (if (string? buffer) (string->bytes/utf-8 buffer) buffer)]
               [n (bytes-length buffer)]
               [copy (make-sized-byte-string (malloc n 'atomic-interior) n)])
          (memcpy copy buffer n)
          copy))
      (define (int->buffer n) (copy-buffer (integer->integer-bytes n 4 #t)))
      (define (bind ctype sqltype buf)
        (let* ([lenbuf
                (int->buffer (if buf (bytes-length buf) SQL_NULL_DATA))]
               [status
                (SQLBindParameter stmt i SQL_PARAM_INPUT ctype sqltype 0 0 buf lenbuf)])
          (handle-status fsym status stmt)
          (if buf (list buf lenbuf) (list lenbuf))))
      (cond [(string? param)
             (bind SQL_C_CHAR SQL_VARCHAR (copy-buffer param))]
            [(bytes? param)
             (bind SQL_C_BINARY SQL_BINARY (copy-buffer param))]
            [(exact-integer? param)
             (bind SQL_C_CHAR SQL_NUMERIC (copy-buffer (number->string param)))]
            [(rational? param)
             (bind SQL_C_CHAR SQL_DOUBLE
                   (copy-buffer (number->string (exact->inexact  param))))]
            [(sql-date? param)
             (bind SQL_C_TYPE_DATE SQL_TYPE_DATE
                   (copy-buffer
                    (let* ([x param]
                           [y (sql-date-year x)]
                           [m (sql-date-month x)]
                           [d (sql-date-day x)])
                      (bytes-append (integer->integer-bytes y 2 #t)
                                    (integer->integer-bytes m 2 #f)
                                    (integer->integer-bytes d 2 #f)))))]
            [(sql-time? param)
             (bind SQL_C_TYPE_TIME SQL_TYPE_TIME
                   (copy-buffer
                    (let* ([x param]
                           [h (sql-time-hour x)]
                           [m (sql-time-minute x)]
                           [s (sql-time-second x)])
                      (bytes-append (integer->integer-bytes h 2 #f)
                                    (integer->integer-bytes m 2 #f)
                                    (integer->integer-bytes s 2 #f)))))]
            [(sql-timestamp? param)
             (bind SQL_C_TYPE_TIMESTAMP SQL_TYPE_TIMESTAMP
                   (copy-buffer
                    (let ([x param])
                      (bytes-append
                       (integer->integer-bytes (sql-timestamp-year x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-month x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-day x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-hour x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-minute x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-second x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-nanosecond x) 4 #f)))))]
            [(sql-null? param)
             (bind SQL_C_CHAR SQL_VARCHAR #f)]
            [else (error 'load-param "cannot convert to unknown type: ~e" param)]))

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
      (define-syntax-rule (get-num size ctype convert convert-arg ...)
        (let ([buf (make-bytes size)])
          (let-values ([(status ind) (SQLGetData stmt i ctype buf)])
            (handle-status fsym status stmt)
            (cond [(= ind SQL_NULL_DATA) sql-null]
                  [else (convert buf convert-arg ...)]))))
      (define (get-int size ctype)
        (get-num size ctype integer-bytes->integer #t))
      (define (get-real ctype)
        (get-num 8 ctype floating-point-bytes->real))
      (define (get-int-list sizes ctype)
        (let ([buf (make-bytes (apply + sizes))])
          (let-values ([(status ind) (SQLGetData stmt i ctype buf)])
            (handle-status fsym status stmt)
            (cond [(= ind SQL_NULL_DATA) sql-null]
                  [else (let ([in (open-input-bytes buf)])
                          (for/list ([size (in-list sizes)])
                            (case size
                              ((2) (io:read-le-int16 in)) ;; FIXME: use host byte order?
                              ((4) (io:read-le-int32 in))
                              (else (error 'get-int-list
                                           "internal error: bad size: ~e" size)))))]))))
      (define (get-string)
        ;; FIXME: use "wide chars" for unicode support?
        (let-values ([(status len-or-ind) (SQLGetData stmt i SQL_C_CHAR #f)])
          (handle-status fsym status stmt #:ignore-ok/info? #t)
          (cond [(= len-or-ind SQL_NULL_DATA) sql-null]
                [(= len-or-ind SQL_NO_TOTAL)
                 (error 'get-column "internal error: SQL_NO_TOTAL")]
                [else
                 (let ([buf (make-bytes (add1 len-or-ind))]) ;; +1 for nul-terminated string
                   (let-values ([(status _li) (SQLGetData stmt i SQL_C_CHAR buf)])
                     (handle-status fsym status stmt)
                     (bytes->string/latin-1 buf #f 0 len-or-ind)))])))
      (define (get-bytes)
        ;; FIXME: use "wide chars" for unicode support?
        (let-values ([(status len-or-ind) (SQLGetData stmt i SQL_C_BINARY #f)])
          (handle-status fsym status stmt #:ignore-ok/info? #t)
          (cond [(= len-or-ind SQL_NULL_DATA) sql-null]
                [(= len-or-ind SQL_NO_TOTAL)
                 (error 'get-column "internal error: SQL_NO_TOTAL")]
                [else
                 (let ([buf (make-bytes len-or-ind)])
                   (let-values ([(status _li) (SQLGetData stmt i SQL_C_BINARY buf)])
                     (handle-status fsym status stmt)
                     buf))])))
      (cond [(or (= typeid SQL_CHAR)
                 (= typeid SQL_VARCHAR)
                 (= typeid SQL_LONGVARCHAR)) ;; long date might need repeated gets (?)
             ;; FIXME: WCHAR, WVARCHAR, WLONGVARCHAR
             (get-string)]
            [(or (= typeid SQL_DECIMAL)
                 (= typeid SQL_NUMERIC))
             (parse-decimal (get-string))]
            [(or (= typeid SQL_SMALLINT)
                 (= typeid SQL_INTEGER)
                 (= typeid SQL_TINYINT))
             (get-int 4 SQL_C_LONG)]
            [(or (= typeid SQL_BIGINT))
             (get-int 8 SQL_C_SBIGINT)]
            [(or (= typeid SQL_REAL)
                 (= typeid SQL_FLOAT)
                 (= typeid SQL_DOUBLE))
             (get-real SQL_C_DOUBLE)]
            [(or (= typeid SQL_BIT))
             (case (get-int 4 SQL_C_LONG)
               ((0) #f)
               ((1) #t)
               (else 'get-column "internal error: SQL_BIT"))]
            [(or (= typeid SQL_BINARY)
                 (= typeid SQL_VARBINARY))
             (get-bytes)]
            [(= typeid SQL_TYPE_DATE)
             (let ([fields (get-int-list '(2 2 2) SQL_C_TYPE_DATE)])
               (cond [(list? fields) (apply sql-date fields)]
                     [(sql-null? fields) sql-null]))]
            [(= typeid SQL_TYPE_TIME)
             (let ([fields (get-int-list '(2 2 2) SQL_C_TYPE_TIME)])
               (cond [(list? fields) (apply sql-time (append fields (list 0 #f)))]
                     [(sql-null? fields) sql-null]))]
            [(= typeid SQL_TYPE_TIMESTAMP)
             (let ([fields (get-int-list '(2 2 2 2 2 2 4) SQL_C_TYPE_TIMESTAMP)])
               (cond [(list? fields) (apply sql-timestamp (append fields (list #f)))]
                     [(sql-null? fields) sql-null]))]
            [else (get-string)]))

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
              [param-typeids
               (let-values ([(status param-count) (SQLNumParams stmt)])
                 (handle-status fsym status stmt)
                 (for/list ([i (in-range 1 (add1 param-count))])
                   (describe-param fsym stmt i)))]
              [result-dvecs
               (let-values ([(status result-count) (SQLNumResultCols stmt)])
                 (handle-status fsym status stmt)
                 (for/list ([i (in-range 1 (add1 result-count))])
                   (describe-result-column fsym stmt i)))])
         (let ([pst (new prepared-statement%
                         (stmt stmt)
                         (owner this)
                         (param-typeids param-typeids)
                         (result-dvecs result-dvecs))])
           (hash-set! statement-table pst #t)
           (thread-resume finalizer-thread)
           (will-register will-executor pst (lambda (pst) (send pst finalize)))
           pst))))

    (define/private (describe-param fsym stmt i)
      (cond [has-describe-param?
             (let-values ([(status type size digits nullable)
                           (SQLDescribeParam stmt i)])
               (handle-status fsym status stmt)
               type)]
            [else SQL_UNKNOWN_TYPE]))

    (define/private (describe-result-column fsym stmt i)
      (let-values ([(status name type size digits nullable)
                    (SQLDescribeCol stmt i)])
        (handle-status fsym status stmt)
        (vector name type size digits)))

    (define/public (disconnect)
      (with-lock
       (when -db
         (let ([db -db]
               [env -env]
               [statements (hash-map statement-table (lambda (k v) k))])
           (set! -db #f)
           (set! -env #f)
           (set! statement-table #f)
           (for ([pst (in-list statements)])
             (send pst finalize))
           (handle-status 'disconnect (SQLDisconnect db) db)
           (handle-status 'disconnect (SQLFreeHandle SQL_HANDLE_DBC db))
           (handle-status 'disconnect (SQLFreeHandle SQL_HANDLE_ENV env))
           (void)))))

    (super-new)

    (register-finalizer this (lambda (obj) (send obj disconnect)))))

;; ----------------------------------------

;; handle-status : symbol integer -> integer
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status who s [handle #f] #:ignore-ok/info? [ignore-ok/info? #f])
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

(define (field-dvec->field-info dvec)
  `((name . ,(vector-ref dvec 0))
    (typeid . ,(vector-ref dvec 1))
    (size . ,(vector-ref dvec 2))
    (digits . ,(vector-ref dvec 3))))
