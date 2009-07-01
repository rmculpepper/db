;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require (for-syntax scheme/base)
         scheme/port)
(provide make-limited-input-port

         io:write
         io:write-int16
         io:write-int32
         io:write-null-terminated-bytes
         io:write-null-terminated-string
         io:write-byte
         io:write-byte/char
         io:write-bytes
         io:write-length+bytes
         io:write-length+string
         
         io:write-le-int16
         io:write-le-int24
         io:write-le-int32
         io:write-le-intN
         io:write-length-code
         io:write-length-coded-bytes
         io:write-length-coded-string
         
         io:read
         io:read-int16
         io:read-int32
         io:read-null-terminated-bytes
         io:read-null-terminated-string
         io:read-byte
         io:read-byte/char
         io:read-bytes-as-string
         io:read-bytes-as-bytes
         io:read-length+bytes
         io:read-length+string
         
         io:read-le-int16
         io:read-le-int24
         io:read-le-int32
         io:read-le-intN
         io:read-length-code
         io:read-length-coded-bytes
         io:read-length-coded-string

         ;; ???
         io:read-bytes-to-eof)

(require (rename-in scheme/base
                    [read-byte read-byte/timeout]
                    [read-bytes read-bytes/timeout]
                    [write-byte write-byte/timeout]
                    [write-bytes write-bytes/timeout]))

#|
(define (make-limited-input-port port limit [close-orig? #t])
  (let ([limit limit])
    (make-input-port
     (object-name port)
     (lambda (str)
       (let ([count (min limit (bytes-length str))])
         (if (zero? count)
             eof
             (let ([n (read-bytes-avail!* str port 0 count)])
               (define (reduce-limit! n)
                 (cond [(eq? n 0)
                        (wrap-evt port (lambda (x) 0))]
                       [(number? n)
                        (set! limit (- limit n))
                        n]
                       [(evt? n)
                        (wrap-evt n reduce-limit!)]
                       [else ;;(or (procedure? n) (eof-object? n))
                        (set! limit (sub1 limit))
                        n]))
               (reduce-limit! n)))))
     (lambda (str skip progress-evt)
       (let ([count (max 0 (min (- limit skip) (bytes-length str)))])
         (if (zero? count)
             eof
             (let ([n (peek-bytes-avail!* str skip progress-evt port 0 count)])
               (if (eq? n 0)
                   (wrap-evt port (lambda (x) 0))
                   n)))))
     (lambda ()
       (when close-orig?
         (close-input-port port)))
     (if (port-provides-progress-evts? port)
         (lambda () (port-progress-evt port))
         #f)
     (lambda (k progress-evt done)
       (port-commit-peeked k progress-evt done port)))))
|#

(define-syntax (io:write stx)
  (syntax-case stx ()
    [(io:write port #:int16 value)
     #'(io:write-int16 port value)]
    [(io:write port #:int32 value)
     #'(io:write-int32 port value)]
    [(io:write port #:string value)
     #'(io:write-null-terminated-string port value)]
    [(io:write port #:byte value)
     #'(io:write-byte port value)]
    [(io:write port #:byte/char value)
     #'(io:write-byte/char port value)]
    [(io:write port #:bytes value)
     #'(io:write-bytes port value)]
    [(io:write port #:length+bytes value)
     #'(io:write-length+bytes port value)]
    [(io:write port #:length+string value)
     #'(io:write-length+string port value)]
    [(io:write port #:bytes2 value)
     #'(io:write-bytes port value)]
    [(io:write port #:bytes4 value)
     #'(io:write-bytes port value)]
    [(io:write port bad-type . _)
     (raise-syntax-error 'io:write
                         "bad datatype keyword"
                         #'bad-type)]))

(define-syntax (io:read stx)
  (syntax-case stx ()
    [(io:read port #:int16)
     #'(io:read-int16 port)]
    [(io:read port #:int32)
     #'(io:read-int32 port)]
    [(io:read port #:string)
     #'(io:read-null-terminated-string port)]
    [(io:read port #:byte)
     #'(io:read-byte port)]
    [(io:read port #:byte/char)
     #'(io:read-byte/char port)]
    [(io:read port #:bytes length)
     #'(io:read-bytes port length)]
    [(io:read port #:length+bytes)
     #'(io:read-length+bytes port)]
    [(io:read port #:length+string)
     #'(io:read-length+string port)]
    [(io:read port #:bytes2)
     #'(io:read-bytes-as-bytes port 2)]
    [(io:read port #:bytes4)
     #'(io:read-bytes-as-bytes port 4)]
    [(io:read port bad-type . _)
     (raise-syntax-error 'io:read
                         "bad datatype keyword"
                         #'bad-type)]))

;; WRITING FUNCTIONS

;; Integer functions expect UNSIGNED values.  User is responsible for 
;; doing whatever necessary to deal with negative numbers.

;; NOTE: The write functions do not report errors such as providing a number
;; to write-intN that is longer than N bits.  The function should silently
;; take the N least significant bits, but no guarantees...

;; write-int16 : port integer -> (void)
;; Writes a 16-bit integer, network byte order
(define (io:write-int16 port val)
  (write-bytes/timeout (integer->integer-bytes val 2 #t #t) port))

;; write-int32 : port integer -> void
;; Writes a 32-bit integer, network byte order
(define (io:write-int32 port val)
  (write-bytes/timeout (integer->integer-bytes val 4 #t #t) port))

;; write-byte : port byte -> void
(define (io:write-byte port byte)
  (write-byte/timeout byte port))

;; write-byte/char : port char -> void
(define (io:write-byte/char port char)
  (write-byte/timeout (char->integer char) port))

;; write-bytes : port bytes -> void
(define (io:write-bytes port bytes)
  (write-bytes/timeout bytes port))

;; write-length+bytes : port bytes/#f -> void
;; #f indicates sql-null, represented as length -1
(define (io:write-length+bytes port bytes)
  (if bytes
      (begin (io:write-int32 port (bytes-length bytes))
             (write-bytes/timeout bytes port))
      (begin (io:write-int32 port -1))))

;; write-length+string : port string -> void
(define (io:write-length+string port string)
  (if string
      (begin (io:write-int32 port (string-utf-8-length string))
             (write-string string port))
      (begin (io:write-int32 port -1))))

;; write-null-terminated-bytes : port bytes -> void
(define (io:write-null-terminated-bytes port bytes)
  (write-bytes/timeout bytes port)
  (write-byte/timeout 0 port))

;; write-null-terminated-string : port string -> void
(define (io:write-null-terminated-string port string)
  (write-string string port)
  (write-byte/timeout 0 port))

;; write-le-int16
(define (io:write-le-int16 port n)
  (write-bytes/timeout (integer->integer-bytes n 2 #f #f) port))

(define (io:write-le-int24 port n)
  (write-bytes/timeout (subbytes (integer->integer-bytes n 4 #f #f) 0 3)
                       port))

(define (io:write-le-int32 port n)
  (write-bytes/timeout (integer->integer-bytes n 4 #f #f) port))

(define (io:write-le-intN port count n)
  (let loop ([count count] [n n])
    (when (positive? count)
      (io:write-byte port (bitwise-and #xFF n))
      (loop (sub1 count) (arithmetic-shift n -8)))))

(define (io:write-length-code port n)
  (cond [(<= n 250) (io:write-byte port n)]
        [(<= n #xFFFF)
         (io:write-byte port 252)
         (io:write-le-int16 port n)]
        [(<= n #xFFFFFF)
         (io:write-byte port 253)
         (io:write-le-int24 port n)]
        [(<= n #xFFFFFFFF)
         (io:write-byte port 253)
         (io:write-le-int32 port n)]
        [else
         (io:write-byte port 254)
         (io:write-le-intN port 8 n)]))

(define (io:write-length-coded-bytes port b)
  (io:write-length-code port (bytes-length b))
  (io:write-bytes port b))

(define (io:write-length-coded-string port s)
  (io:write-length-coded-bytes port (string->bytes/latin-1 s)))

;; READING

;; read-int16 : port -> integer
(define (io:read-int16 port)
  (integer-bytes->integer (read-bytes/timeout 2 port) #t #t))

;; read-int32 : port -> integer
(define (io:read-int32 port)
  (integer-bytes->integer (read-bytes/timeout 4 port) #t #t))

;; read-null-terminated-string : port -> string
(define (io:read-null-terminated-string port)
  (let [(strport (open-output-bytes))]
    (let loop ()
      (let ([next (read-byte/timeout port)])
        (cond [(zero? next)
               (get-output-string strport)]
              [else
               (write-byte next strport)
               (loop)])))))

;; read-null-terminated-bytes : port -> bytes
(define (io:read-null-terminated-bytes port)
  (let [(strport (open-output-bytes))]
    (let loop ()
      (let ([next (read-byte/timeout port)])
        (cond [(zero? next)
               (get-output-bytes strport)]
              [else
               (write-byte next strport)
               (loop)])))))

;; read-byte : port -> byte
(define (io:read-byte port)
  (read-byte/timeout port))

;; read-byte : port-> char
(define (io:read-byte/char port)
  (integer->char (read-byte/timeout port)))

;; read-bytes-as-bytes : port number -> bytes
(define (io:read-bytes-as-bytes port n)
  (read-bytes/timeout n port))

;; read-bytes-as-string : port -> string
(define (io:read-bytes-as-string port n)
  (bytes->string/utf-8 (read-bytes/timeout n port)))

;; read-length+bytes : port -> bytes | #f
;; As a special case, a "length" of -1 results in #f
;; Any other negative number will cause an error.
(define (io:read-length+bytes port)
  (let ([len (io:read-int32 port)])
    (if (= len -1)
        #f
        (io:read-bytes-as-bytes port len))))

;; read-length+string : port -> string | #f
;; As a special case, a "length" of -1 results in #f
;; Any other negative number will cause an error.
(define (io:read-length+string port)
  (let ([len (io:read-int32 port)])
    (if (= len -1)
        #f
        (io:read-bytes-as-string port len))))

(define (io:read-le-int16 port)
  (integer-bytes->integer (read-bytes/timeout 2 port) #f #f))

(define (io:read-le-int24 port)
  (io:read-le-intN port 3))

(define (io:read-le-int32 port)
  (integer-bytes->integer (read-bytes/timeout 4 port) #f #f))

(define (io:read-le-intN port count)
  (let ([b (read-bytes/timeout count port)])
    (unless (and (bytes? b) (= count (bytes-length b)))
      (error 'io:read-le-intN "unexpected eof; got ~s" b))
    (let loop ([pos 0])
      (if (< pos count)
          (+ (arithmetic-shift (loop (add1 pos)) -8)
             (bytes-ref b pos))
          0))))

(define (io:read-length-code port)
  (let ([first (read-byte/timeout port)])
    (cond [(<= first 250)
           first]
          [(= first 251)
           ;; Indicates NULL record
           #f]
          [(= first 252)
           (io:read-le-int16 port)]
          [(= first 253)
           (io:read-le-int32 port)]
          [(= first 254)
           (io:read-le-intN port 8)])))

(define (io:read-length-coded-bytes port)
  (let ([len (io:read-length-code port)])
    (and len (read-bytes/timeout len port))))

(define (io:read-length-coded-string port)
  (let ([b (io:read-length-coded-bytes port)])
    (and b (bytes->string/latin-1 b))))

;; FIXME: weird...
(define (io:read-bytes-to-eof port)
  (let loop ([acc null])
    (let ([next (read-bytes 1024 port)])
      (if (eof-object? next)
          (apply bytes-append (reverse acc))
          (loop (cons next acc))))))
