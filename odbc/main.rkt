;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         racket/contract
         "../generic/main.rkt"
         "../generic/check-access.rkt"
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide dbsystem)
(provide/contract
 [connect (-> #:database string? #:user string? #:password string?
              connection?)])

(define (connect #:database database
                 #:user user
                 #:password auth)
  (let-values ([(status env) (SQLAllocHandle SQL_HANDLE_ENV #f)])
    (handle-status 'odbc-connect status env)
    (let ([status (SQLSetEnvAttr env SQL_ATTR_ODBC_VERSION SQL_OV_ODBC3)])
      (handle-status 'odbc-connect status env)
      (let-values ([(status db) (SQLAllocHandle SQL_HANDLE_DBC env)])
        (handle-status 'odbc-connect status db)
        (let ([status (SQLConnect db database user auth)])
          ;; free db, env on failure
          (handle-status 'odbc-connect status db)
          (new connection% (db db)))))))

;; FIXME: put env in connection?
