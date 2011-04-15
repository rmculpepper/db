;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/class
         racket/tcp
         "../generic/main.rkt"
         "../generic/socket.rkt"
         "../generic/find-socket.rkt"
         "connection.rkt"
         "dbsystem.rkt")
(provide mysql-connect
         mysql-guess-socket-path
         (rename-out [dbsystem mysql-dbsystem]))

(define (mysql-connect #:user user
                       #:database database
                       #:password [password #f]
                       #:server [server #f]
                       #:port [port #f]
                       #:socket [socket #f]
                       #:input-port [input-port #f]
                       #:output-port [output-port #f]
                       #:allow-cleartext-password? [allow-cleartext-password? #f])
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
  (let ([c (new connection%)])
    (cond [socket
           (let-values ([(in out)
                         (unix-socket-connect socket)])
             (send c attach-to-ports in out))]
          [input-port
           (send c attach-to-port input-port output-port)]
          [else
           (let ([server (or server "localhost")]
                 [port (or port 3306)])
             (let-values ([(in out) (tcp-connect server port)])
               (send c attach-to-ports in out)))])
    (send c start-connection-protocol database user password)
    c))

(define socket-paths
  '("/var/run/mysqld/mysqld.sock"))

(define (mysql-guess-socket-path)
  (guess-socket-path/paths 'mysql-guess-socket-path socket-paths))
