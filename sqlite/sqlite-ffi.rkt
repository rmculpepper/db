#lang racket/base
(require ffi/unsafe
         (prefix-in c: racket/contract)
         "constants.ss")

(define sqlite (ffi-lib "libsqlite3"))

; Syntax Helpers
(define-syntax define-sqlite
  (syntax-rules ()
    [(_ id type)
     (define id (get-ffi-obj (quote id) sqlite type))]))

; Types
(define-cpointer-type _sqlite3_database)
(define-cpointer-type _sqlite3_statement)

(define _pointer-number
  (let ([ptrsize (ctype-sizeof _pointer)])
    (cond
      [(= ptrsize (ctype-sizeof _int)) _int]
      [(= ptrsize (ctype-sizeof _long)) _long]
      [else (error 'sqlite-ffi "Cannot find a number the same size as a pointer!")])))

; Functions
(define-sqlite sqlite3_libversion_number (_fun -> _int))
(define-sqlite sqlite3_close (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_last_insert_rowid (_fun _sqlite3_database -> _int64))
(define-sqlite sqlite3_changes (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_total_changes (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_interrupt (_fun _sqlite3_database -> _void))
(define-sqlite sqlite3_complete (_fun _string -> _int))
; sqlite3_complete16
; sqlite3_busy_handler
; sqlite3_busy_timeout
; sqlite3_ printf functions
; sqlite3_set_authorizer
; sqlite3_trace
; sqlite3_progress_handler
; sqlite3_commit_hook
(define-sqlite sqlite3_open
  (_fun (filename) ::
        (filename : _bytes)
        (db : (_ptr o _sqlite3_database))
        -> (result : _int)
        -> (values db result)))
; sqlite3_open16
(define-sqlite sqlite3_errcode (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_errmsg (_fun _sqlite3_database -> _string))
; sqlite3_errmsg16
(define-sqlite sqlite3_prepare_v2
  (_fun (db zsql) ::
        (db : _sqlite3_database) (zsql : _string) ((string-utf-8-length zsql) : _int)
        ;; bad prepare statements set statement to NULL, with no error reported
        (statement : (_ptr o _sqlite3_statement/null)) (tail : (_ptr o _string))
        -> (result : _int)
        -> (values statement result tail)))
; sqlite3_prepare16
; sqlite3_bind functions
(define-sqlite sqlite3_bind_int (_fun _sqlite3_statement _int _int -> _int))
(define-sqlite sqlite3_bind_int64 (_fun _sqlite3_statement _int _int64 -> _int))
(define-sqlite sqlite3_bind_double (_fun _sqlite3_statement _int _double -> _int))
(define-sqlite sqlite3_bind_text
  (_fun (stmt col the-string) ::
        (stmt : _sqlite3_statement)
        (col : _int)
        (string-ptr : _string = the-string)
        (string-len : _int = (string-utf-8-length the-string))
        (destructor : _pointer-number = SQLITE_TRANSIENT)
        -> _int))
(define-sqlite sqlite3_bind_blob
  (_fun (stmt col the-bytes) ::
        (stmt : _sqlite3_statement)
        (col : _int)
        (byte-ptr : _bytes = the-bytes)
        (byte-len : _int = (bytes-length the-bytes))
        (destructor : _pointer-number = SQLITE_TRANSIENT)
        -> _int))
(define-sqlite sqlite3_bind_null (_fun _sqlite3_statement _int -> _int))
;(_fun -> _void) -> _int))
(define-sqlite sqlite3_bind_parameter_count (_fun _sqlite3_statement -> _int))
(define-sqlite sqlite3_bind_parameter_name (_fun _sqlite3_statement _int -> _string))
(define-sqlite sqlite3_bind_parameter_index (_fun _sqlite3_statement _string -> _int))
; sqlite3_clear_bindings
(define-sqlite sqlite3_column_count (_fun _sqlite3_statement -> _int))
(define-sqlite sqlite3_column_name (_fun _sqlite3_statement _int -> _string))
; sqlite3_column_name16
(define-sqlite sqlite3_column_decltype (_fun _sqlite3_statement _int -> _string))
(define-sqlite sqlite3_column_type (_fun _sqlite3_statement _int -> _int))
; sqlite3_column_decltype16
(define-sqlite sqlite3_step (_fun _sqlite3_statement -> _int))
(define-sqlite sqlite3_data_count (_fun _sqlite3_statement -> _int))
; sqlite3_column functions
(define-sqlite sqlite3_column_int (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_int64 (_fun _sqlite3_statement _int -> _int64))
(define-sqlite sqlite3_column_double (_fun _sqlite3_statement _int -> _double))
(define-sqlite sqlite3_column_text (_fun _sqlite3_statement _int -> _string))
(define-sqlite sqlite3_column_bytes (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_blob
  (_fun (stmt : _sqlite3_statement)
        (col : _int)
        -> (blob : _bytes)
        -> (let ([len (sqlite3_column_bytes stmt col)])
             (bytes-copy (make-sized-byte-string blob len)))))
(define-sqlite sqlite3_finalize (_fun _sqlite3_statement -> _int))
(define-sqlite sqlite3_reset (_fun _sqlite3_statement -> _int))
; sqlite3_ user-defined function functions
(define-sqlite sqlite3_expired (_fun _sqlite3_database -> _int))
;(define-sqlite sqlite3_global_recover (_fun -> _int))

; Contracts
(define status? exact-nonnegative-integer?)
(provide/contract
 [status?
  (any/c . c:-> . boolean?)]
 [sqlite3_open
  (bytes? . c:-> . (values sqlite3_database? status?))]
 [sqlite3_close
  (sqlite3_database? . c:-> . status?)]
 [sqlite3_last_insert_rowid
  (sqlite3_database? . c:-> . exact-nonnegative-integer?)]
 [sqlite3_changes
  (sqlite3_database? . c:-> . exact-nonnegative-integer?)]
 [sqlite3_total_changes
  (sqlite3_database? . c:-> . exact-nonnegative-integer?)]
 [sqlite3_prepare_v2
  (c:-> sqlite3_database? string?
        (values (or/c sqlite3_statement? false/c) status? string?))]
 [sqlite3_errmsg
  (sqlite3_database? . c:-> . string?)]
 [sqlite3_step
  (sqlite3_statement? . c:-> . status?)]
 [sqlite3_bind_parameter_count
  (sqlite3_statement? . c:-> . exact-nonnegative-integer?)]
 [sqlite3_bind_int64
  (sqlite3_statement? exact-nonnegative-integer? integer? . c:-> . status?)]
 [sqlite3_bind_double
  (sqlite3_statement? exact-nonnegative-integer? number? . c:-> . status?)]
 [sqlite3_bind_text
  (sqlite3_statement? exact-nonnegative-integer? string? . c:-> . status?)]
 [sqlite3_bind_null
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . status?)]
 [sqlite3_bind_blob
  (sqlite3_statement? exact-nonnegative-integer? bytes? . c:-> . status?)]
 [sqlite3_column_count
  (sqlite3_statement? . c:-> . exact-nonnegative-integer?)]
 [sqlite3_column_name
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . string?)]
 [sqlite3_column_type
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . exact-nonnegative-integer?)]
 [sqlite3_column_decltype
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . (or/c string? false/c))]
 [sqlite3_column_blob
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . bytes?)]
 [sqlite3_column_text
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . string?)]
 [sqlite3_column_int64
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . integer?)]
 [sqlite3_column_double
  (sqlite3_statement? exact-nonnegative-integer? . c:-> . number?)]
 [sqlite3_reset
  (sqlite3_statement? . c:-> . status?)]
 [sqlite3_finalize
  (sqlite3_statement? . c:-> . status?)])
