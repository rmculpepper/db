;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/class
         scheme/tcp
         "../generic/main.ss"
         "../generic/socket.ss"
         "connection.ss"
         "dbsystem.ss")
(provide connect
         dbsystem)

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
                #| (allow-cleartext-password? allow-cleartext-password?) |#)])
    #| (send c set-ssl-options ssl ssl-encrypt) |#
    (cond [socket
           (let-values ([(in out) (unix-socket-connect socket)])
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
