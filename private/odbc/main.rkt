;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/contract
         "../generic/main.rkt"
         "../generic/check-access.rkt"
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide odbc-connect
         odbc-driver-connect
         odbc-data-sources
         odbc-drivers
         (rename-out [dbsystem odbc-dbsystem]))

(define (odbc-connect #:database database
                      #:user [user #f]
                      #:password [auth #f])
  (call-with-env 'odbc-connect
    (lambda (env)
      (call-with-db 'odbc-connect env
        (lambda (db)
          (let ([status (SQLConnect db database user auth)])
            (handle-status 'odbc-connect status db)
            (new connection% (env env) (db db))))))))

(define (odbc-driver-connect connection-string)
  (call-with-env 'odbc-driver-connect
    (lambda (env)
      (call-with-db 'odbc-driver-connect env
        (lambda (db)
          (let ([status (SQLDriverConnect db connection-string SQL_DRIVER_NOPROMPT)])
            (handle-status 'odbc-driver-connect status db)
            (new connection% (env env) (db db))))))))

(define (odbc-data-sources)
  (call-with-env 'odbc-data-sources
    (lambda (env)
      (begin0
          (let loop ()
            (let-values ([(status name description) (SQLDataSources env SQL_FETCH_NEXT)])
              (cond [(or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                     (cons (list name description) (loop))]
                    [else ;; SQL_NO_DATA
                     null])))
        (handle-status 'odbc-data-sources (SQLFreeHandle SQL_HANDLE_ENV env))))))

(define (odbc-drivers)
  (call-with-env 'odbc-drivers
   (lambda (env)
     (begin0
         (let loop ()
           (let-values ([(status name _attrs) (SQLDrivers env SQL_FETCH_NEXT)])
             (cond [(or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                    (cons (list name #f) (loop))]
                   [else ;; SQL_NO_DATA
                    null])))
       (handle-status 'odbc-drivers (SQLFreeHandle SQL_HANDLE_ENV env))))))

;; ----

;; Aux functions to free handles on error.

(define (call-with-env fsym proc)
  (let-values ([(status env) (SQLAllocHandle SQL_HANDLE_ENV #f)])
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (SQLFreeHandle SQL_HANDLE_ENV env)
                       (raise e))])
      (handle-status fsym status env)
      (handle-status fsym (SQLSetEnvAttr env SQL_ATTR_ODBC_VERSION SQL_OV_ODBC3))
      (proc env))))

(define (call-with-db fsym env proc)
  (let-values ([(status db) (SQLAllocHandle SQL_HANDLE_DBC env)])
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (SQLFreeHandle SQL_HANDLE_DBC db)
                       (raise e))])
      (handle-status fsym status db)
      (proc db))))
