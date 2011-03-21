;; Copyright 2007-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; The solaris code is untested (and thus disabled).

#lang racket/base
(require ffi/unsafe)
(provide unix-socket-connect)

#|
(struct unix-socket-output-port (out fd)
        #:property prop:output-port (struct-field-index out))

(define-cstruct _intbox ;; _intbox-pointer can be used as int*
  ([value _int]))
|#

;; unix-socket-connect : pathlike -> input-port output-port
;; Connects to the unix domain socket associated with the given path.
(define (unix-socket-connect path0)
  (define path (check-pathlike 'unix-socket-connect path0))
  (define s (make-socket))
  (unless (positive? s)
    (error 'unix-socket-connect
           "failed to create socket"))
  (define addr (make-unix-sockaddr path))
  (define addrlen (+ (ctype-sizeof _short) (bytes-length path)))
  (define ce (_connect s addr addrlen))
  (unless (zero? ce)
    (_close s)
    (raise-user-error
     'unix-socket-connect
     "failed to connect socket to path: ~s" path))
  #|
  (define se (_setsockopt s SOL_SOCKET SO_PASSCRED (make-intbox 1) (ctype-sizeof _int)))
  (unless (zero? ce)
    (_close s)
    (raise-user-error
     'unix-socket-connect
     "error enabling socket credentials"))
  |#
  (with-handlers ([(lambda (e) #t)
                   (lambda (e)
                     (_close s)
                     (raise e))])
    (let-values ([(in out)
                  (_make_fd_output_port s 'socket #f #f #t)])
      #| (values in (unix-socket-output-port out s)) |#
      (values in out))))

(define platform
  (let ([os (system-type 'os)]
        [machine (system-type 'machine)])
    (cond [(eq? os 'macosx) 'macosx]
          [(regexp-match #rx"Linux.*86" machine) 'linux86]
          [(regexp-match #rx"SunOS" machine) #f #;'solaris]
          [else #f])))

(define _socklen_t _uint)
(define _size_t _int)

(define AF_UNIX 1)
(define SOCK_STREAM
  (case platform
    ((linux86 macosx) 1)
    ((solaris) 2)
    (else #f)))

(define (make-socket)
  (unless (and AF_UNIX SOCK_STREAM)
    (raise-user-error
     'unix-socket-connect
     "unix-domain sockets not supported on this platform"))
  (_socket AF_UNIX SOCK_STREAM 0))

(define _sockaddr_un_path_part
  (case platform
    ((linux86 solaris)
     (make-cstruct-type (build-list 108 (lambda (i) _byte))))
    ((macosx)
     (make-cstruct-type (build-list 104 (lambda (i) _byte))))
    (else
     ;; kluge: so that later definitions go through.
     _int)))

(define-cstruct _sockaddr_un
  ([sun_family _short]
   [sun_path   _sockaddr_un_path_part]))

(define-cstruct _macosx_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _ubyte]
   [sun_path   _sockaddr_un_path_part]))

(define (ffi name type)
  (case platform
    ((linux86 solaris macosx)
     (get-ffi-obj name #f type (lambda () #f)))
    (else
     (lambda _ (error name "not supported")))))

(define _socket
  (ffi "socket" (_fun _int _int _int -> _int)))
(define _connect
  (ffi "connect"
       (case platform
         ((linux86 solaris)
          (_fun _int _sockaddr_un-pointer _int -> _int))
         ((macosx)
          (_fun _int _macosx_sockaddr_un-pointer _int -> _int)))))
(define _setsockopt
  (ffi "setsockopt" (_fun _int _int _int _pointer _socklen_t -> _int)))
(define _close
  (ffi "close" (_fun _int -> _int)))
(define _make_fd_output_port
  (ffi "scheme_make_fd_output_port"
       (_fun _int _scheme _bool _bool _bool -> _scheme)))

(define (make-unix-sockaddr path)
  (case platform
    ((linux86 solaris)
     (make-sockaddr_un AF_UNIX path))
    ((macosx)
     (make-macosx_sockaddr_un (bytes-length path) AF_UNIX path))))

(define (check-pathlike function path0)
  (unless (or (string? path0) (path? path0) (bytes? path0))
    (raise-type-error function
                      "path, string, or bytes"
                      path0))
  (let ([path
         (cond [(string? path0) (string->bytes/utf-8 path0)]
               [(path? path0) (path->bytes path0)]
               [(bytes? path0) path0])])
    (unless (< (bytes-length path) 100)
      (raise-type-error 'unix-socket-connect
                        "path (of less than 100 bytes)"
                        path0))
    path))


;; ----------------------------------------

#|

;; The credentials code is untested.

(define SO_PASSCRED 16)
(define SOL_SOCKET 1)

(define SCM_CREDENTIALS 2)

#|

struct msghdr {
    void         *msg_name;       /* optional address */
    socklen_t     msg_namelen;    /* size of address */
    struct iovec *msg_iov;        /* scatter/gather array */
    size_t        msg_iovlen;     /* # elements in msg_iov */
    void         *msg_control;    /* ancillary data, see below */
    socklen_t     msg_controllen; /* ancillary data buffer len */
    int           msg_flags;      /* flags on received message */
};

struct cmsghdr {
    socklen_t cmsg_len;     /* data byte count, including hdr */
    int       cmsg_level;   /* originating protocol */
    int       cmsg_type;    /* protocol-specific type */
/* followed by
    u_char    cmsg_data[]; */
};

struct ucred {
    pid_t pid;    /* process ID of the sending process */
    uid_t uid;    /* user ID of the sending process */
    gid_t gid;    /* group ID of the sending process */
};

struct iovec {
    void *iov_base;   /* Starting address */
    size_t iov_len;   /* Number of bytes */
};
|#

(define _pid_t _int)
(define _uid_t _uint)
(define _gid_t _uint)

(define _getpid (ffi "getpid" (_fun -> _pid_t)))
(define _geteuid (ffi "geteuid" (_fun -> _uid_t)))
(define _getegid (ffi "getegid" (_fun -> _gid_t)))

(define-cstruct _ucred
  ([pid _pid_t]
   [uid _uid_t]
   [gid _gid_t]))

(define-cstruct _cmsghdr/ucred
  ([cmsg_len   _socklen_t] ;; sizeof(_cmsghdr/ucred)
   [cmsg_level _int]
   [cmsg_type  _int]
   [creds      _ucred]))

(define-cstruct _iovec
  ([iov_base _pointer]
   [iov_len  _size_t]))

(define-cstruct _msghdr/ucred
  ([msg_name       _pointer]
   [msg_namelen    _socklen_t]
   [msg_iov        _iovec-pointer]
   [msg_iovlen     _size_t] ;; = 1
   [msg_control    _cmsghdr/ucred-pointer] 
   [msg_controllen _socklen_t] ;; = sizeof(_cmsghdr/ucred)
   [msg_flags      _int]))

(define (make-cred-cmsghdr)
  (make-cmsghdr/ucred (ctype-sizeof _cmsghdr/ucred)
                      SOL_SOCKET
                      SCM_CREDENTIALS
                      (make-ucred (_getpid)
                                  (_geteuid)
                                  (_getegid))))

(define (make-cred-msghdr bs)
  (make-msghdr/ucred #f
                     0
                     (make-iovec bs (bytes-length bs))
                     1
                     (make-cred-cmsghdr)
                     (ctype-sizeof _cmsghdr/ucred)
                     0))

(define _ssize_t _int)

(define _sendmsg
  (ffi "sendmsg" (_fun _int _msghdr/ucred-pointer _int -> _ssize_t)))

(define (unix-socket-send-credentials p [bs #"a"])
  (let ([s (unix-socket-output-port-fd p)]
        [fdout (unix-socket-output-port-out p)])
    ;; Must check fdout not closed, but avoid races!
    ;; Delay check until later so atomic section is minimal.
    (let* ([msg (make-cred-msghdr bs)]
           [result
            (call-as-atomic
             (lambda ()
               (when (port-closed? fdout)
                 (error 'unix-socket-send-credentials
                        "output port is closed"))
               (_sendmsg s msg 0)))])
      (when (negative? result)
        (_close s) ;; ???
        (error 'unix-socket-send-credentials
               "error sending credentials (errno = ~s)" (saved-errno)))
      (void))))

|#
