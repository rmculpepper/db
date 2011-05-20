;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/contract
         racket/tcp
         "../generic/interfaces.rkt"
         "../generic/socket.rkt"
         "../generic/find-socket.rkt"
         "connection.rkt"
         "dbsystem.rkt")
(provide postgresql-connect
         postgresql-guess-socket-path
         (rename-out [dbsystem postgresql-dbsystem]))

(define (postgresql-connect #:user user
                            #:database database
                            #:password [password #f]
                            #:server [server #f]
                            #:port [port #f]
                            #:socket [socket #f]
                            #:allow-cleartext-password? [allow-cleartext-password? #f]
                            #:ssl [ssl 'no]
                            #:ssl-encrypt [ssl-encrypt 'sslv2-or-v3]
                            #:notice-handler [notice-handler void]
                            #:notification-handler [notification-handler void])
  (let ([connection-options
         (+ (if (or server port) 1 0)
            (if socket 1 0))]
        [notice-handler (make-handler notice-handler "notice")]
        [notification-handler
         (if (procedure? notification-handler)
             notification-handler
             (make-print-notification notification-handler))])
    (when (> connection-options 1)
      (uerror 'postgresql-connect "cannot give both server/port and socket arguments"))
    (let ([c (new connection%
                  (notice-handler notice-handler)
                  (notification-handler notification-handler)
                  (allow-cleartext-password? allow-cleartext-password?))])
      (let-values ([(in out)
                    (cond [socket (unix-socket-connect socket)]
                          [else (let ([server (or server "localhost")]
                                      [port (or port 5432)])
                                  (tcp-connect server port))])])
        (send c attach-to-ports in out ssl ssl-encrypt)
        (send c start-connection-protocol database user password)
        c))))

(define socket-paths
  '("/var/run/postgresql/.s.PGSQL.5432"))

(define (postgresql-guess-socket-path)
  (guess-socket-path/paths 'postgresql-guess-socket-path socket-paths))

;; make-print-notification : output-port -> string -> void
(define ((make-print-notification out) condition)
  (fprintf (case out
             ((output) (current-output-port))
             ((error) (current-error-port))
             (else out))
           "notification: ~a\n" condition))
