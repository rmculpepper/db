;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/class
         scheme/unit
         scheme/tcp
         "../generic/main.ss"
         "../generic/socket.ss"
         "connection.ss"
         "types.ss")
(provide postgresql@
         postgresql-connect@
         postgresql-sql-format@)

(define-unit postgresql-connect@
  (import)
  (export connect^)
  
  (define (connect #:user user
                   #:database database
                   #:password [password #f]
                   #:server [server #f]
                   #:port [port #f]
                   #:socket [socket #f]
                   #:input-port [input-port #f]
                   #:output-port [output-port #f]
                   #:allow-cleartext-password? [allow-cleartext-password? #f]
                   #:ssl [ssl 'no]
                   #:ssl-encrypt [ssl-encrypt 'sslv2-or-v3]
                   #:mixin [mixin values])
    (let ([connection-options
           (+ (if (or server port) 1 0)
              (if socket 1 0)
              (if (or input-port output-port) 1 0))])
      (when (> connection-options 1)
        (raise-user-error 'connect
                          (string-append
                           "cannot specify more than one of server/port, "
                           "socket, or input-port/output-port arguments"))))
    (when (or input-port output-port)
      (unless (and input-port output-port)
        (raise-user-error 'connect
                          "must give input-port and output-port arguments together")))
    (let ([c (new (mixin connection%)
                  (allow-cleartext-password? allow-cleartext-password?))])
      (send c set-ssl-options ssl ssl-encrypt)
      (cond [socket
             (let-values ([(in out) (unix-socket-connect socket)])
               (send c attach-to-ports in out))]
            [input-port
             (send c attach-to-port input-port output-port)]
            [else
             (let ([server (or server "localhost")]
                   [port (or port 5432)])
               (let-values ([(in out) (tcp-connect server port)])
                 (send c attach-to-ports in out)))])
      (send c start-connection-protocol database user password)
      c))
  
  #|
  ;; list-databases : connection% -> (listof string)
  (define (list-databases c)
    (send c query-list "select datname::text from pg_database"))

  ;; list-tables : connection% boolean -> (listof string)
  (define (list-tables c)
    (send c query-list
          "select tablename::text from pg_tables where tableowner != 'postgres'"))

  ;; list-views : connection -> (list-of string)
  (define (list-views c)
    (send c query-list
          "select viewname::text from pg_views where viewowner != 'postgres'"))

  ;; list-fields : connection% string -> (list-of string)
  (define (list-fields c table)
    (send c query-list
          (format-sql "select A.attname::text from pg_attribute A, pg_class T "
                      "where A.attrelid = T.relfilenode "
                      "and T.relname = ~a and A.attnum > 0"
                      [string table])))
  |#
  )

(define-compound-unit/infer postgresql@
  (import)
  (export sql-basis^ sql-format^ connect^)
  (link postgresql-sql-format@ postgresql-connect@))
