;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/contract
         "../generic/main.rkt"
         (only-in "../generic/interfaces.rkt" make-handler)
         "../generic/check-access.rkt"
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide odbc-connect
         odbc-driver-connect
         odbc-data-sources
         odbc-drivers
         (rename-out [dbsystem odbc-dbsystem]))

(define (odbc-connect #:dsn [dsn #f]
                      #:database [database #f]
                      #:user [user #f]
                      #:password [auth #f]
                      #:notice-handler [notice-handler void]
                      #:strict-parameter-types? [strict-parameter-types? #f])
  (when (and dsn database)
    (error 'odbc-connect "cannot give both #:dsn and #:database arguments"))
  (unless (or dsn database)
    (error 'odbc-connect "missing #:dsn argument"))
  (let ([dsn (or dsn database)]
        [notice-handler (make-handler notice-handler "notice")])
    (call-with-env 'odbc-connect
      (lambda (env)
        (call-with-db 'odbc-connect env
          (lambda (db)
            (let ([status (SQLConnect db dsn user auth)])
              (handle-status* 'odbc-connect status db)
              (new connection%
                   (env env)
                   (db db)
                   (notice-handler notice-handler)
                   (strict-parameter-types? strict-parameter-types?)))))))))

(define (odbc-driver-connect connection-string
                             #:notice-handler [notice-handler void]
                             #:strict-parameter-types? [strict-parameter-types? #f])
  (let ([notice-handler (make-handler notice-handler "notice")])
    (call-with-env 'odbc-driver-connect
      (lambda (env)
        (call-with-db 'odbc-driver-connect env
          (lambda (db)
            (let ([status (SQLDriverConnect db connection-string SQL_DRIVER_NOPROMPT)])
              (handle-status* 'odbc-driver-connect status db)
              (new connection%
                   (env env)
                   (db db)
                   (notice-handler notice-handler)
                   (strict-parameter-types? strict-parameter-types?)))))))))

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
        (handle-status* 'odbc-data-sources (SQLFreeHandle SQL_HANDLE_ENV env))))))

(define (odbc-drivers)
  (call-with-env 'odbc-drivers
   (lambda (env)
     (let* ([attrlens
             (let loop ()
               (let-values ([(status name attrlen)
                             (SQLDrivers env SQL_FETCH_NEXT #f)])
                 (cond [(or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                        (cons attrlen (loop))]
                       [else ;; SQL_NO_DATA
                        null])))]
            [attr-buf (make-bytes (+ 1 (apply max 0 attrlens)))] ;; +1 for null terminator
            [result
             (let loop ()
               (let-values ([(status name attrlen) ;; & writes to attr-buf
                             (SQLDrivers env SQL_FETCH_NEXT attr-buf)])
                 (cond [(or (= status SQL_SUCCESS) (= status SQL_SUCCESS_WITH_INFO))
                        (cons (list name (parse-driver-attrs attr-buf attrlen))
                              (loop))]
                       [else ;; SQL_NO_DATA
                        null])))])
       (handle-status* 'odbc-drivers (SQLFreeHandle SQL_HANDLE_ENV env))
       result))))

(define (parse-driver-attrs buf len)
  (let* ([attrs (regexp-split #rx#"\0" buf 0 len)])
    (for/list ([p (in-list attrs)]
               #:when (positive? (bytes-length p)))
      (let* ([s (bytes->string/utf-8 p)]
             [m (regexp-match-positions #rx"=" s)])
        (unless m (error 'odbc-drivers "bad attribute syntax: ~e" s))
        (let ([=-pos (caar m)])
          (cons (substring s 0 =-pos) (substring s (+ 1 =-pos))))))))

;; ----

;; Aux functions to free handles on error.

(define (call-with-env fsym proc)
  (let-values ([(status env) (SQLAllocHandle SQL_HANDLE_ENV #f)])
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (SQLFreeHandle SQL_HANDLE_ENV env)
                       (raise e))])
      (handle-status* fsym status env)
      (handle-status* fsym (SQLSetEnvAttr env SQL_ATTR_ODBC_VERSION SQL_OV_ODBC3))
      (proc env))))

(define (call-with-db fsym env proc)
  (let-values ([(status db) (SQLAllocHandle SQL_HANDLE_DBC env)])
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (SQLFreeHandle SQL_HANDLE_DBC db)
                       (raise e))])
      (handle-status* fsym status db)
      (proc db))))
