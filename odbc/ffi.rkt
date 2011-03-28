#lang racket/base
(require racket/contract
         unsafe/ffi
         unsafe/ffi/define
         "ffi-constants.rkt")
(provide (all-from "ffi-constants.rkt"))

;; turn into unit, param'd by odbc lib

(define _sqlsmallint _sshort)
(define _sqlusmallint _ushort)
(define _sqlinteger _sint)
(define _sqluinteger _uint)
(define _sqlreturn _sqlsmallint)

#|
Docs at http://msdn.microsoft.com/en-us/library/ms712628%28v=VS.85%29.aspx
|#

(define-ffi-definer define-odbc ???)

(define-odbc SQLAllocHandle
  (_fun _sqlsmallint
        _sqlhandle
        (handle : (_ptr o _sqlhandle))
        -> (status : _sqlreturn)
        -> (values status handle)))

(define-odbc SQLConnect
  (_fun (handle server user auth) ::
        (handle : _sqlhdbc)
        (server : _string)
        ((string-utf-8-length server) : _sqlsmallint)
        (user : _string)
        ((string-utf-8-length user) : _sqlsmallint)
        (auth : _string)
        ((string-utf-8-length auth) : _sqlsmallint)
        -> _sqlreturn))

(define-odbc SQLDataSources
  (_fun (handle direction)
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
                   (bytes->string/utf-8 server-buf 0 server-length)
                   (bytes->string/utf-8 descr-buf 0 descr-length))))

(define-odbc SQLPrepare
  (_fun (handle stmt)
        (handle : _sqlhstmt)
        (stmt : _string)
        ((string-utf-8-length stmt) : _sqlinteger)
        -> _sqlreturn))

(define-odbc SQLBindParameter
  (_fun (handle : _sqlhstmt)
        (param-num : _sqlusmallint)
        (iomode : _sqlsmallint)
        (c-type : _sqlsmallint)
        (sql-type : _sqlsmallint)
        (column-size : _sqlulen)
        (digits : _sqlsmallint)
        (value : _pointer) ;; must be pinned until after SQLExecute called
        (value-len : _sqllen) ;; ignored for fixed-length data
        (str-len-or-ind-ptr : _sqllen-pointer)
        -> _sqlreturn))

(define-odbc SQLExecute
  (_fun (handle : _sqlhstmt)
        -> _sqlreturn))

(define-odbc SQLNumParams
  (_fun (handle : _sqlhstmt)
        (count : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status count)))

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
        (column-size : (_ptr o _sqlulen))
        (digits : (_ptr o _sqlsmallint))
        (nullable : (_ptr o _sqlsmallint))
        -> (status : _sqlstatus)
        -> (values status
                   (list (bytes->string/utf-8 column-buf 0 column-length)
                         data-type column-size digits nullable))))

(define-odbc SQLFetch
  (_fun _sqlhstmt
        -> _sqlreturn))

(define-odbc SQLGetData
  (_fun (handle column target-type buffer) ::
        (handle : _sqlhstmt)
        (column : _sqlusmallint)
        (target-type : _sqlsmallint)
        (buffer : _bytes) ;; may be null (#f) to get length
        ((bytes-length buffer) : _sqllen)
        (len-or-ind : (_ptr o _sqllen))
        -> (status : _sqlreturn)
        -> (values status buffer len-or-ind)))

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
  (_fun (handle-type : _sqlsmallint)
        (handle : _sqlhandle)
        (rec-number : _sqlsmallint)
        (sql-state-buf : _bytes = (make-bytes 6))
        (native-errcode : (_ptr o _sqlinteger))
        (message-buf : _bytes = (make-bytes 1024)) ;; FIXME
        ((bytes-length message-buf) : _sqlsmallint)
        (message-len : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (bytes->string/utf-8 sql-state-buf 0 5)
                   native-errcode
                   (bytes->string/utf-8 message-buf 0 message-len))))
