;; from Jay McCarthy

#lang racket/base
(require ffi/unsafe)

(require "ffi-constants.rkt")
(provide (all-from-out "ffi-constants.rkt"))

(define-ffi-definer define-sqlite (ffi-lib "libsqlite3"))

; Types
(define-cpointer-type _sqlite3_database)
(define-cpointer-type _sqlite3_statement)

(define _pointer-number
  (let ([ptrsize (ctype-sizeof _pointer)])
    (cond
      [(= ptrsize (ctype-sizeof _int)) _int]
      [(= ptrsize (ctype-sizeof _long)) _long]
      [else (error 'sqlite-ffi "Cannot find a number the same size as a pointer!")])))

;; -- Functions --

;; -- DB --

(define-sqlite sqlite3_libversion_number
  (_fun -> _int))

(define-sqlite sqlite3_open
  (_fun (filename) :: (filename : _bytes)
        (db : (_ptr o _sqlite3_database))
        -> (result : _int)
        -> (values db result)))

(define-sqlite sqlite3_close
  (_fun _sqlite3_database
        -> _int))

;; -- Stmt --

(define-sqlite sqlite3_prepare_v2
  (_fun (db zsql) ::
        (db : _sqlite3_database) (zsql : _string) ((string-utf-8-length zsql) : _int)
        ;; bad prepare statements set statement to NULL, with no error reported
        (statement : (_ptr o _sqlite3_statement/null)) (tail : (_ptr o _string))
        -> (result : _int)
        -> (values statement result tail)))

(define-sqlite sqlite3_finalize
  (_fun _sqlite3_statement
        -> _int))

(define-sqlite sqlite3_bind_parameter_count (_fun _sqlite3_statement -> _int))
;;(define-sqlite sqlite3_bind_parameter_name (_fun _sqlite3_statement _int -> _string))
;;(define-sqlite sqlite3_bind_parameter_index (_fun _sqlite3_statement _string -> _int))

;; Can these be called before first 'step'?
(define-sqlite sqlite3_column_count
  (_fun _sqlite3_statement
        -> _int))
(define-sqlite sqlite3_column_name
  (_fun _sqlite3_statement _int
        -> _string))
(define-sqlite sqlite3_column_decltype
  (_fun _sqlite3_statement _int
        -> _string))

;; ----------------------------------------

(define-sqlite sqlite3_last_insert_rowid (_fun _sqlite3_database -> _int64))
(define-sqlite sqlite3_changes (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_total_changes (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_interrupt (_fun _sqlite3_database -> _void))
(define-sqlite sqlite3_complete (_fun _string -> _int))
;; sqlite3_busy_handler
;; sqlite3_busy_timeout
;; sqlite3_ printf functions
;; sqlite3_set_authorizer
;; sqlite3_trace
;; sqlite3_progress_handler
;; sqlite3_commit_hook

(define-sqlite sqlite3_errcode (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_errmsg (_fun _sqlite3_database -> _string))
;; sqlite3_bind functions
(define-sqlite sqlite3_bind_int (_fun _sqlite3_statement _int _int -> _int))
(define-sqlite sqlite3_bind_int64 (_fun _sqlite3_statement _int _int64 -> _int))
(define-sqlite sqlite3_bind_double (_fun _sqlite3_statement _int _double -> _int))
(define-sqlite sqlite3_bind_text (_fun (stmt col the-string) ::
                                       (stmt : _sqlite3_statement)
                                       (col : _int)
                                       (string-ptr : _string = the-string)
                                       (string-len : _int = (string-utf-8-length the-string))
                                       (destructor : _pointer-number = SQLITE_TRANSIENT)
                                       -> _int))
(define-sqlite sqlite3_bind_blob (_fun (stmt col the-bytes) ::
                                       (stmt : _sqlite3_statement)
                                       (col : _int)
                                       (byte-ptr : _bytes = the-bytes)
                                       (byte-len : _int = (bytes-length the-bytes))
                                       (destructor : _pointer-number = SQLITE_TRANSIENT)
                                       -> _int))
(define-sqlite sqlite3_bind_null (_fun _sqlite3_statement _int -> _int))
;(_fun -> _void) -> _int))
; sqlite3_clear_bindings
(define-sqlite sqlite3_step (_fun _sqlite3_statement -> _int))
(define-sqlite sqlite3_data_count (_fun _sqlite3_statement -> _int))
; sqlite3_column functions
(define-sqlite sqlite3_column_type
  (_fun _sqlite3_statement _int
        -> _int))
(define-sqlite sqlite3_column_int (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_int64 (_fun _sqlite3_statement _int -> _int64))
(define-sqlite sqlite3_column_double (_fun _sqlite3_statement _int -> _double))
(define-sqlite sqlite3_column_text (_fun _sqlite3_statement _int -> _string))
(define-sqlite sqlite3_column_bytes (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_blob (_fun (stmt : _sqlite3_statement)
                                         (col : _int)
                                         -> (blob : _bytes)
                                         -> (let ([len (sqlite3_column_bytes stmt col)])
                                              (bytes-copy (make-sized-byte-string blob len)))))
(define-sqlite sqlite3_reset (_fun _sqlite3_statement -> _int))
; sqlite3_ user-defined function functions
(define-sqlite sqlite3_expired (_fun _sqlite3_database -> _int))
;(define-sqlite sqlite3_global_recover (_fun -> _int))

(define status? exact-nonnegative-integer?)

; Contracts
(provide/contract
 [status?
  (c-> any/c boolean?)]
 [sqlite3_open
  (c-> bytes? (values sqlite3_database? status?))]
 [sqlite3_close
  (c-> sqlite3_database? status?)]
 [sqlite3_last_insert_rowid
  (c-> sqlite3_database? exact-nonnegative-integer?)]
 [sqlite3_changes
  (c-> sqlite3_database? exact-nonnegative-integer?)]
 [sqlite3_total_changes
  (c-> sqlite3_database? exact-nonnegative-integer?)]
 [sqlite3_prepare_v2
  (c-> sqlite3_database? string?
       (values (or/c sqlite3_statement? false/c) status? string?))]
 [sqlite3_errmsg
  (c-> sqlite3_database? string?)]
 [sqlite3_step
  (c-> sqlite3_statement? status?)]
 [sqlite3_bind_parameter_count
  (c-> sqlite3_statement? exact-nonnegative-integer?)]
 [sqlite3_bind_int64
  (c-> sqlite3_statement? exact-nonnegative-integer? integer? status?)]
 [sqlite3_bind_double
  (c-> sqlite3_statement? exact-nonnegative-integer? number? status?)]
 [sqlite3_bind_text
  (c-> sqlite3_statement? exact-nonnegative-integer? string? status?)]
 [sqlite3_bind_null
  (c-> sqlite3_statement? exact-nonnegative-integer? status?)]
 [sqlite3_bind_blob
  (c-> sqlite3_statement? exact-nonnegative-integer? bytes? status?)]
 [sqlite3_column_count
  (c-> sqlite3_statement? exact-nonnegative-integer?)]
 [sqlite3_column_name
  (c-> sqlite3_statement? exact-nonnegative-integer? string?)]
 [sqlite3_column_type
  (c-> sqlite3_statement? exact-nonnegative-integer? exact-nonnegative-integer?)]
 [sqlite3_column_decltype
  (c-> sqlite3_statement? exact-nonnegative-integer? (or/c string? false/c))]
 [sqlite3_column_blob
  (c-> sqlite3_statement? exact-nonnegative-integer? bytes?)]
 [sqlite3_column_text
  (c-> sqlite3_statement? exact-nonnegative-integer? string?)]
 [sqlite3_column_int64
  (c-> sqlite3_statement? exact-nonnegative-integer? integer?)]
 [sqlite3_column_double
  (c-> sqlite3_statement? exact-nonnegative-integer? number?)]
 [sqlite3_reset
  (c-> sqlite3_statement? status?)]
 [sqlite3_finalize
  (c-> sqlite3_statement? status?)])

;; ----------------------------------------

(define-cpointer _CustodianReference)

(define scheme_add_managed
  (ffi #f (_fun _racket _racket
                (_fun _racket _pointer/null -> _void)
                _pointer/null
                _boolean
                -> _CustodianReference)))

(define scheme_remove_managed
  (ffi #f (_fun _CustodianReference _racket -> _void)))

(provide/contract
 [scheme_add_managed
  (-> custodian? any/c (-> any/c any/c any) any/c boolean?
      CustodianReference?)]
 [scheme_remove_managed
  (-> CustodianReference? any/c any)])
