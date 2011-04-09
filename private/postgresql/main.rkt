;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/contract
         racket/tcp
         "../generic/main.rkt"
         "../generic/socket.rkt"
         "../generic/find-socket.rkt"
         "connection.rkt"
         "dbsystem.rkt")

;; FIXME: Contracts duplicated at db/main.rkt
(provide/contract
 [connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? false/c)
        #:server (or/c string? false/c)
        #:port (or/c exact-positive-integer? false/c)
        #:socket (or/c string? path? false/c)
        #:input-port (or/c input-port? false/c)
        #:output-port (or/c output-port? false/c)
        #:allow-cleartext-password? boolean?
        #:ssl (symbols 'yes 'no 'optional)
        #:ssl-encrypt (symbols 'sslv2 'sslv3 'sslv2-or-v3)
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:notification-handler (or/c 'output 'error output-port? procedure?))
       any/c)]
 [guess-socket-path
  (-> (or/c string? path?))])
(provide dbsystem)

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
                 #:notice-handler [notice-handler 'error]
                 #:notification-handler [notification-handler 'error])
  (let ([connection-options
         (+ (if (or server port) 1 0)
            (if socket 1 0)
            (if (or input-port output-port) 1 0))]
        [notice-handler
         (if (procedure? notice-handler)
             notice-handler
             (make-print-notice notice-handler))]
        [notification-handler
         (if (procedure? notification-handler)
             notification-handler
             (make-print-notification notification-handler))])
    (when (> connection-options 1)
      (raise-user-error 'connect
                        (string-append
                         "cannot specify more than one of server/port, "
                         "socket, or input-port/output-port arguments")))
    (when (or input-port output-port)
      (unless (and input-port output-port)
        (raise-user-error 'connect
                          "must give input-port and output-port arguments together")))
    (let ([c (new connection%
                  (notice-handler notice-handler)
                  (notification-handler notification-handler)
                  (allow-cleartext-password? allow-cleartext-password?))])
      (let-values ([(in out)
                    (cond [socket (unix-socket-connect socket)]
                          [input-port (values input-port output-port)]
                          [else (let ([server (or server "localhost")]
                                      [port (or port 5432)])
                                  (tcp-connect server port))])])
        (send c attach-to-ports in out ssl ssl-encrypt)
        (send c start-connection-protocol database user password)
        c))))

(define socket-paths
  '("/var/run/postgresql/.s.PGSQL.5432"))

(define (guess-socket-path)
  (guess-socket-path/paths 'postgresql-guess-socket-path socket-paths))

;; make-print-notice : output-port -> string string -> void
(define ((make-print-notice out) code message)
  (fprintf (case out
             ((output) (current-output-port))
             ((error) (current-error-port))
             (else out))
           "notice: ~a (SQL code ~a)\n" message code))

;; make-print-notification : output-port -> string string -> void
(define ((make-print-notification out) condition)
  (fprintf (case out
             ((output) (current-output-port))
             ((error) (current-error-port))
             (else out))
           "notification: ~a\n" condition))
