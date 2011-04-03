#lang racket/base
(require (rename-in racket/contract [-> c->])
         ffi/unsafe
         ffi/unsafe/define
         "ffi-constants.rkt")
(provide (all-from-out "ffi-constants.rkt"))
(provide (all-defined-out))

;; turn into unit, param'd by odbc lib

(define-cpointer-type _sqlhandle)

(define-cpointer-type _sqlhenv)
(define-cpointer-type _sqlhdbc)
(define-cpointer-type _sqlhstmt)

(define _sqllen _long)
(define _sqlulen _ulong)

(define _sqlsmallint _sshort)
(define _sqlusmallint _ushort)
(define _sqlinteger _sint)
(define _sqluinteger _uint)
(define _sqlreturn _sqlsmallint)


#|
Docs at http://msdn.microsoft.com/en-us/library/ms712628%28v=VS.85%29.aspx
|#

(define-ffi-definer define-odbc (ffi-lib "libodbc" '("1" #f)))

(define-odbc SQLAllocHandle
  (_fun (type : _sqlsmallint)
        (parent : _sqlhandle/null)
        (handle : (_ptr o _sqlhandle/null))
        -> (status : _sqlreturn)
        -> (values status
                   (begin (when handle
                            (cpointer-push-tag! handle
                                                (cond [(= type SQL_HANDLE_ENV) sqlhenv-tag]
                                                      [(= type SQL_HANDLE_DBC) sqlhdbc-tag]
                                                      [(= type SQL_HANDLE_STMT) sqlhstmt-tag]
                                                      [else sqlhandle-tag])))
                          handle))))

;; SQLSetEnvAttr
;; must set odbc version env attr before making connection

(define-odbc SQLSetEnvAttr
  (_fun (env : _sqlhenv)
        (attr : _sqlinteger)
        (value-buf : _sqlinteger) ;; (the one case we care about takes int, not ptr)
        (_sqlinteger = 0)
        -> _sqlreturn))

(define-odbc SQLGetFunctions
  (_fun (handle : _sqlhdbc)
        (function-id : _sqlusmallint)
        (supported? : (_ptr o _sqlusmallint))
        -> (status : _sqlreturn)
        -> (values status (positive? supported?))))

(define-odbc SQLConnect
  (_fun (handle server user auth) ::
        (handle : _sqlhdbc)
        (server : _string)
        ((string-utf-8-length server) : _sqlsmallint)
        (user : _string)
        ((if user (string-utf-8-length user) 0) : _sqlsmallint)
        (auth : _string)
        ((if auth (string-utf-8-length auth) 0) : _sqlsmallint)
        -> _sqlreturn))

(define-odbc SQLDriverConnect
  (_fun (handle connection driver-completion) ::
        (handle : _sqlhdbc)
        (#f : _pointer)
        (connection : _string)
        ((if connection (string-utf-8-length connection) 0) : _sqlsmallint)
        (#f : _bytes) ;; FIXME
        (0 : _sqlsmallint)
        (out-length : (_ptr o _sqlsmallint))
        (driver-completion : _sqlusmallint)
        -> (status : _sqlreturn)
        -> status))

(define-odbc SQLDataSources
  (_fun (handle direction) ::
        (handle : _sqlhenv)
        (direction : _sqlusmallint)
        (server-buf : _bytes = (make-bytes 1024)) ;; FIXME: get proper size
        ((bytes-length server-buf) : _sqlsmallint)
        (server-length : (_ptr o _sqlsmallint))
        (descr-buf : _bytes = (make-bytes 1024)) ;; FIXME
        ((bytes-length descr-buf) : _sqlsmallint)
        (descr-length : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (and (or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                        (bytes->string/utf-8 server-buf #f 0 server-length))
                   (and (or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                        (bytes->string/utf-8 descr-buf #f 0 descr-length)))))

(define-odbc SQLDrivers
  (_fun (handle direction) ::
        (handle : _sqlhenv)
        (direction : _sqlusmallint)
        (driver-buf : _bytes = (make-bytes 1024)) ;; FIXME
        ((bytes-length driver-buf) : _sqlsmallint)
        (driver-length : (_ptr o _sqlsmallint))
        (attrs-buf : _bytes = #f) ;; FIXME
        (0 : _sqlsmallint)
        (attrs-length : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (and (or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                        (bytes->string/utf-8 driver-buf #f 0 driver-length))
                   (and (or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                        #f))))

(define-odbc SQLPrepare
  (_fun (handle stmt) ::
        (handle : _sqlhstmt)
        (stmt : _string)
        ((string-utf-8-length stmt) : _sqlinteger)
        -> _sqlreturn))

(define-odbc SQLBindParameter
  (_fun (handle param-num iomode c-type sql-type column-size digits value len-or-ind) ::
        (handle : _sqlhstmt)
        (param-num : _sqlusmallint)
        (iomode : _sqlsmallint)
        (c-type : _sqlsmallint)
        (sql-type : _sqlsmallint)
        (column-size : _sqlulen)
        (digits : _sqlsmallint)
        (value : _pointer) ;; must be pinned until after SQLExecute called
        ((if (bytes? value) (bytes-length value) 0) : _sqllen) ;; ignored for fixed-length data
        (len-or-ind : _pointer) ;; _sqllen-pointer)
        -> _sqlreturn))

(define-odbc SQLExecute
  (_fun (handle : _sqlhstmt)
        -> _sqlreturn))

(define-odbc SQLNumParams
  (_fun (handle : _sqlhstmt)
        (count : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status count)))

(define-odbc SQLDescribeParam
  (_fun (handle : _sqlhstmt)
        (parameter : _sqlusmallint)
        (data-type : (_ptr o _sqlsmallint))
        (size : (_ptr o _sqlulen))
        (digits : (_ptr o _sqlsmallint))
        (nullable : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status data-type size digits nullable)))

(define-odbc SQLNumResultCols
  (_fun (handle : _sqlhstmt)
        (count : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status count)))

(define-odbc SQLDescribeCol
  (_fun (handle column) ::
        (handle : _sqlhstmt)
        (column : _sqlusmallint)
        (column-buf : _bytes = (make-bytes 1024)) ;; FIXME
        ((bytes-length column-buf) : _sqlsmallint)
        (column-len : (_ptr o _sqlsmallint))
        (data-type : (_ptr o _sqlsmallint))
        (size : (_ptr o _sqlulen))
        (digits : (_ptr o _sqlsmallint))
        (nullable : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (bytes->string/utf-8 column-buf #f 0 column-len)
                   data-type size digits nullable)))

(define-odbc SQLFetch
  (_fun _sqlhstmt
        -> _sqlreturn))

(define-odbc SQLGetData
  (_fun (handle column target-type buffer) ::
        (handle : _sqlhstmt)
        (column : _sqlusmallint)
        (target-type : _sqlsmallint)
        (buffer : _bytes) ;; may be null (#f) to get length
        ((if buffer (bytes-length buffer) 0) : _sqllen)
        (len-or-ind : (_ptr o _sqllen))
        -> (status : _sqlreturn)
        -> (values status len-or-ind)))

(define-odbc SQLFreeStmt
  (_fun (handle : _sqlhstmt)
        (option : _sqlusmallint)
        -> _sqlreturn))

(define-odbc SQLCloseCursor
  (_fun (handle : _sqlhstmt)
        -> _sqlreturn))

(define-odbc SQLDisconnect
  (_fun (handle : _sqlhdbc)
        -> _sqlreturn))

(define-odbc SQLFreeHandle
  (_fun (handle-type : _sqlsmallint)
        (handle : _sqlhandle)
        -> _sqlreturn))

(define-odbc SQLGetDiagRec
  (_fun (handle-type handle rec-number) ::
        (handle-type : _sqlsmallint)
        (handle : _sqlhandle)
        (rec-number : _sqlsmallint)
        (sql-state-buf : _bytes = (make-bytes 6))
        (native-errcode : (_ptr o _sqlinteger))
        (message-buf : _bytes = (make-bytes 1024)) ;; FIXME
        ((bytes-length message-buf) : _sqlsmallint)
        (message-len : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (bytes->string/utf-8 sql-state-buf #\? 0 5)
                   native-errcode
                   (bytes->string/utf-8 message-buf #\? 0 message-len))))
