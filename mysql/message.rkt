;; Copyright 2007-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/match
         "../generic/sql-data.rkt"
         "../generic/io.rkt")
(provide (all-defined-out))

(define-struct packet () #:transparent)

(define-struct (handshake-packet packet)
  (protocol-version
   server-version
   thread-id
   scramble
   server-capabilities
   charset
   server-status)
  #:transparent)

(define-struct (client-authentication-packet packet)
  (client-flags
   max-packet-length
   charset
   user
   scramble
   database)
  #:transparent)

(define-struct (command-packet packet)
  (command
   argument)
  #:transparent)

(define-struct (command:change-user-packet packet)
  (user
   password
   database
   charset)
  #:transparent)

(define-struct (ok-packet packet)
  (affected-rows
   insert-id
   server-status
   warning-count
   message)
  #:transparent)

(define-struct (error-packet packet)
  (errno
   sqlstate
   message)
  #:transparent)

(define-struct (result-set-header-packet packet)
  (field-count
   extra)
  #:transparent)

(define-struct (field-packet packet)
  (catalog
   db
   table
   org-table
   name
   org-name
   charset
   length
   type
   flags
   decimals
   default)
  #:transparent)

(define-struct (eof-packet packet)
  (warning-count
   status)
  #:transparent)

(define-struct (row-data-packet packet)
  (data)
  #:transparent)

(define-struct (binary-row-data-packet packet)
  (data)
  #:transparent)

(define-struct (ok-prepared-statement-packet packet)
  (statement-handler-id
   result-count
   parameter-count)
  #:transparent)

(define-struct (parameter-packet packet)
  (type
   flags
   decimals
   length)
  #:transparent)

(define-struct (long-data-packet packet)
  (statement-handler-id
   parameter-number
   type
   data)
  #:transparent)

(define-struct (execute-packet packet)
  (statement-id
   flags
   iterations
   null-map
   first-execution?
   param-types
   params)
  #:transparent)

(define (write-packet out p number)
  (let ([o (open-output-bytes)])
    (write-packet* o p)
    (let ([b (get-output-bytes o)])
      #| (printf "writing packet #~s, length ~s\n" number (bytes-length b)) |#
      (io:write-le-int24 out (bytes-length b))
      (io:write-byte out number)
      (io:write-bytes out b))))

(define (write-packet* out p)
  (match p
    [(struct client-authentication-packet
             (client-flags max-length charset user scramble database))
     (io:write-le-int32 out (encode-server-flags client-flags))
     (io:write-le-int32 out max-length)
     (io:write-byte out (encode-charset charset))
     (io:write-bytes out (make-bytes 23 0))
     (io:write-null-terminated-string out user)
     (if scramble
         (io:write-length-coded-bytes out scramble)
         (io:write-byte out 0))
     (io:write-null-terminated-string out database)]
    [(struct command-packet (command arg))
     (io:write-byte out (encode-command command))
     (io:write-null-terminated-bytes out (string->bytes/utf-8 arg))]
    [(struct long-data-packet (statement-handler-id parameter-number type data))
     (io:write-le-int32 out statement-handler-id)
     (io:write-le-int16 out parameter-number)
     (io:write-le-int16 out type)
     (io:write-bytes out (string->bytes/utf-8 data))]
    [(struct execute-packet
             (statement-id flags iterations null-map first? param-types params))
     (io:write-byte out (encode-command 'statement-execute))
     (io:write-le-int32 out statement-id)
     (io:write-byte out (encode-execute-flags flags))
     (io:write-le-int32 out iterations)
     (io:write-le-intN out
                       (null-map-length null-map)
                       (null-map->integer null-map))
     (io:write-byte out (if first? 1 0))
     (when first?
       (for-each (lambda (pt) (io:write-le-int16 out (encode-type pt)))
                 param-types))
     (for-each (lambda (type param) (write-binary-datum out type param))
               param-types params)]))

(define (parse-packet in expect param-types)
  (let* ([len (io:read-le-int24 in)]
         [num (io:read-byte in)]
         #|
         [_ (fprintf (current-error-port) "** Received packet #~s, length ~s\n" num len)]
         |#
         [inp (subport in len)]
         [msg (parse-packet/1 inp expect len param-types)])
    (when (port-has-bytes? inp)
      (error 'parse-packet "bytes left over after parsing ~s; bytes were: ~s" 
             msg (io:read-bytes-to-eof inp)))
    (values num msg)))

(define (port-has-bytes? p)
  (not (eof-object? (peek-byte p))))

(define (parse-packet/1 in expect len param-types)
  (let ([first (peek-byte in)])
    (if (eq? first #xFF)
        (parse-error-packet in len)
        (parse-packet/2 in expect len param-types))))

(define (parse-packet/2 in expect len param-types)
  (case expect
    ((handshake)
     (parse-handshake-packet in len))
    ((auth)
     (unless (eq? (peek-byte in) #x00)
       (error 'parse-packet "expect auth ok packet"))
     (parse-ok-packet in len))
    ((result)
     (if (eq? (peek-byte in) #x00)
         (parse-ok-packet in len)
         (parse-result-set-header-packet in len)))
    ((field)
     (if (and (eq? (peek-byte in) #xFE) (< len 9))
         (parse-eof-packet in len)
         (parse-field-packet in len)))
    ((data)
     (if (and (eq? (peek-byte in) #xFE) (< len 9))
         (parse-eof-packet in len)
         (parse-row-data-packet in len)))
    ((binary-data)
     (if (and (eq? (peek-byte in) #xFE) (< len 9))
         (parse-eof-packet in len)
         (parse-binary-row-data-packet in len param-types)))
    ((prep-ok)
     (parse-ok-prepared-statement-packet in len))
    ((prep-params)
     (if (and (eq? (peek-byte in) #xFE) (< len 9))
         (parse-eof-packet in len)
         (parse-parameter-packet in len)))
    (else
     (error 'parse-packet "bad expected packet type: ~s" expect))))

;; Individual parsers

(define (parse-handshake-packet in len)
  (let* ([protocol-version (io:read-byte in)]
         [server-version (io:read-null-terminated-string in)]
         [thread-id (io:read-le-int32 in)]
         [scramble1 (io:read-bytes-as-bytes in 8)]
         [_ (io:read-byte in)]
         [server-capabilities (io:read-le-int16 in)]
         [charset (decode-charset (io:read-byte in))]
         [server-status (io:read-le-int16 in)]
         [_ (io:read-bytes-as-bytes in 13)]
         [scramble2 (io:read-bytes-as-bytes in 12)]
         [_ (io:read-byte in)] ;; always \0
         )
    (make-handshake-packet protocol-version
                           server-version
                           thread-id
                           (bytes-append scramble1 scramble2)
                           (decode-server-flags server-capabilities)
                           charset
                           server-status)))

(define (parse-client-authentication-packet in len)
  (let* ([flags (io:read-le-int32 in)]
         [max-length (io:read-le-int32 in)]
         [charset (io:read-byte in)]
         [_1 (io:read-bytes-as-bytes in 23)]
         [user (io:read-null-terminated-string in)]
         [scramble (io:read-length-coded-bytes in)]
         [_2 (io:read-byte in)]
         [db (io:read-null-terminated-string in)])
    (list (decode-server-flags flags)
          max-length
          charset
          _1
          user
          scramble
          _2
          db)))

(define (parse-old-client-authentication-packet in len)
  (let* ([flags (io:read-le-int16 in)]
         [max-length (io:read-le-int24 in)]
         [user (io:read-null-terminated-string in)]
         [scramble (io:read-bytes-as-bytes in 8)]
         [_2 (io:read-byte in)])
    (list (decode-server-flags flags)
          max-length
          user
          scramble
          _2)))

(define (parse-ok-packet in len)
  (let* ([_ (io:read-byte in)]
         [affected-rows (io:read-length-code in)]
         [insert-id (io:read-length-code in)]
         [server-status (io:read-le-int16 in)]
         [warning-count (io:read-le-int16 in)]
         [message (io:read-bytes-to-eof in)])
    (make-ok-packet affected-rows
                    insert-id
                    server-status
                    warning-count
                    (bytes->string/utf-8 message))))

(define (parse-error-packet in len)
  (let* ([_ (io:read-byte in)]
         [errno (io:read-le-int16 in)]
         [marker (peek-char in)]
         [sqlstate
          (and (eq? marker #\#)
               (begin (io:read-byte in)
                      (io:read-bytes-as-string in 5)))]
         [message (io:read-bytes-to-eof in)])
    (make-error-packet errno
                       sqlstate
                       (bytes->string/utf-8 message))))

(define (parse-result-set-header-packet in len)
  (let* ([field-count (io:read-length-code in)]
         [extra (and (port-has-bytes? in)
                     (io:read-length-code in))])
    (make-result-set-header-packet field-count extra)))

(define (parse-field-packet in len)
  (let* ([catalog (io:read-length-coded-string in)]
         [db (io:read-length-coded-string in)]
         [table (io:read-length-coded-string in)]
         [org-table (io:read-length-coded-string in)]
         [name (io:read-length-coded-string in)]
         [org-name (io:read-length-coded-string in)]
         [_ (io:read-byte in)]
         [charset (io:read-le-int16 in)]
         [len (io:read-le-int32 in)]
         [type (io:read-byte in)]
         [flags (io:read-le-int16 in)]
         [decimals (io:read-byte in)]
         [_ (io:read-bytes-as-bytes in 2)]
         [default (and (port-has-bytes? in)  (io:read-length-code in))])
    (make-field-packet catalog
                       db
                       table
                       org-table
                       name
                       org-name
                       charset
                       len
                       (decode-type type)
                       (decode-field-flags flags)
                       decimals
                       default)))

(define (parse-eof-packet in len)
  (let* ([_ (io:read-byte in)]
         [warnings (io:read-le-int16 in)]
         [status (io:read-le-int16 in)])
    (make-eof-packet warnings status)))

(define (parse-row-data-packet in len)
  (make-row-data-packet
   (let loop ()
     (if (at-eof? in)
         null
         (let* ([datum (io:read-length-coded-string in)])
           (cons (or datum sql-null)
                 (loop)))))))

(define (parse-ok-prepared-statement-packet in len)
  (let* ([ok (io:read-byte in)]
         [statement-handler-id (io:read-le-int32 in)]
         [columns (io:read-le-int16 in)]
         [params (io:read-le-int16 in)]
         [warnings (and (>= len 12) (io:read-le-int16 in))]
         [_ (io:read-bytes-to-eof in)])
    (unless (zero? ok)
      (error 'parse-ok-prepared-statement-packet
             "first byte was ~s" ok))
    (make-ok-prepared-statement-packet statement-handler-id
                                       columns
                                       params)))

(define (parse-parameter-packet in len)
  (let* ([type (io:read-le-int16 in)]
         [flags (io:read-le-int16 in)]
         [decimals (io:read-byte in)]
         [len (io:read-le-int32 in)])
    (make-parameter-packet (decode-type type)
                           (decode-field-flags flags)
                           decimals
                           len)))

(define (parse-binary-row-data-packet in0 len param-types)
  (define b (io:read-bytes-to-eof in0))
  (define in (open-input-bytes b))
  #|(printf "binary row: ~s or ~s\n" b (bytes->list b))|#
  (let* ([first (io:read-byte in)] ;; SKIP? seems to be always zero
         [param-count (length param-types)]
         [null-map-length (floor (/ (+ 9 param-count) 8))]
         [null-map-int (io:read-le-intN in null-map-length)]
         [null-map (cddr (integer->null-map null-map-int (+ 2 param-count)))]
         [fields
          (let loop ([null-map null-map] [param-types param-types])
            (cond [(pair? param-types)
                   (cons (if (not (car null-map))
                             (read-binary-datum in (car param-types))
                             sql-null)
                         (loop (cdr null-map) (cdr param-types)))]
                  [else null]))])
    #| (printf "param-types ~s; null-map-length ~s; null-map ~s\n"
               param-types null-map-length null-map) |#
    (make-binary-row-data-packet fields)))

(define (read-binary-datum in type)
  (case type
    ((tiny) (io:read-byte in))
    ((short) (io:read-le-int16 in))
    ((long) (io:read-le-int32 in))
    ((longlong) (io:read-le-intN in 8))
    ((blob var-string) (io:read-length-coded-string in))
    (else (error 'get-param "unimplemented: ~s" type))))

(define (write-binary-datum out type param)
  (case type
    ((tiny) (io:write-byte out param))
    ((short) (io:write-le-int16 out param))
    ((long) (io:write-le-int32 out param))
    ((longlong) (io:write-le-intN out param 8))
    ((var-string) (io:write-length-coded-string out param))
    (else (error 'send-param "unimplemented: ~s" type))))


;; ----

(define (fetch key table function)
  (let ([val (assq key table)])
    (if val
        (cdr val)
        (error function "not found: ~s" key))))

(define (encode-flags flags table function)
  (apply bitwise-ior
         (map (lambda (f) (fetch f table function))
              flags)))

(define (decode-flags n table function)
  (let loop ([table table])
    (cond [(null? table)
           null]
          [(positive? (bitwise-and (caar table) n))
           (cons (cdar table) (loop (cdr table)))]
          [else
           (loop (cdr table))])))

(define (invert-alist alist)
  (map (lambda (p) (cons (cdr p) (car p))) alist))

(define server-flags/decoding
  '((#x1     . long-password)
    (#x2     . found-rows)
    (#x4     . long-flag)
    (#x8     . connect-with-db)
    (#x10    . no-schema)
    (#x20    . compress)
    (#x40    . odbc)
    (#x80    . local-files)
    (#x100   . ignore-space)
    (#x200   . protocol-41)
    (#x400   . interactive)
    (#x800   . ssl)
    (#x1000  . ignore-sigpipe)
    (#x2000  . transactions)
    (#x4000  . protocol-41-OLD)
    (#x8000  . secure-connection)
    (#x10000 . multi-statements)
    (#x20000 . multi-results)))
(define server-flags/encoding
  (invert-alist server-flags/decoding))

(define commands/decoding
  '((#x00 . sleep)
    (#x01 . quit)
    (#x02 . init-db)
    (#x03 . query)
    (#x04 . field-list)
    (#x05 . create-db)      ;; deprecated
    (#x06 . drop-db)        ;; deprecated
    (#x07 . refresh)
    (#x08 . shutdown)
    (#x09 . statistics)
    (#x0A . process-info)
    (#x0B . connect)
    (#x0C . process-kill)
    (#x0D . debug)
    (#x0E . ping)
    (#x0F . time)
    (#x10 . delayed-insert)
    (#x11 . change-user)
    (#x16 . statement-prepare)
    (#x17 . statement-execute)
    (#x18 . statement-send-long-data)
    (#x19 . statement-close)
    (#x1A . statement-reset)
    (#x1B . set-option)
    (#x1C . statement-fetch)))
(define commands/encoding
  (invert-alist commands/decoding))

(define types/decoding
  '((#x00 . decimal)
    (#x01 . tiny)
    (#x02 . short)
    (#x03 . long)
    (#x04 . float)
    (#x05 . double)
    (#x06 . null)
    (#x07 . timestamp)
    (#x08 . longlong)
    (#x09 . int24)
    (#x0A . date)
    (#x0B . time)
    (#x0C . datetime)
    (#x0D . year)
    (#x0E . newdate)
    (#x0F . varchar)
    (#x10 . bit)
    (#xF6 . newdecimal)
    (#xF7 . enum)
    (#xF8 . set)
    (#xF9 . tiny-blob)
    (#xFA . medium-blob)
    (#xFB . long-blob)
    (#xFC . blob)
    (#xFD . var-string)
    (#xFE . string)
    (#xFF . geometry)))
(define types/encoding
  (invert-alist types/decoding))

(define field-flags/decoding
  '((#x001 . not-null)
    (#x002 . primary-key)
    (#x004 . unique-key)
    (#x008 . multiple-key)
    (#x010 . blob)
    (#x020 . unsigned)
    (#x040 . zero-fill)
    (#x080 . binary)
    (#x100 . enum)
    (#x200 . auto-increment)
    (#x400 . timestamp)
    (#x800 . set)))
(define field-flags/encoding
  (invert-alist field-flags/decoding))

(define execute-flags/decoding
  '((#x0 . cursor/no-cursor)
    (#x1 . cursor/read-only)
    (#x2 . cursor/for-update)
    (#x4 . cursor/scrollable)))
(define execute-flags/encoding
  (invert-alist execute-flags/decoding))


(define (encode-server-flags flags)
  (encode-flags flags server-flags/encoding 'encode-server-flags))
(define (decode-server-flags n)
  (decode-flags n server-flags/decoding 'decode-server-flags))

(define (encode-field-flags flags)
  (encode-flags flags field-flags/encoding 'encode-field-flags))
(define (decode-field-flags n)
  (decode-flags n field-flags/decoding 'decode-field-flags))

(define (encode-charset charset)
  (case charset
    ((utf8-general-ci) 33)
    (else (error 'encode-charset "unknown charset: ~e" charset))))
(define (decode-charset n)
  (case n
    ((33) 'utf8-general-ci)
    (else 'unknown)))

(define (encode-type type)
  (fetch type types/encoding 'encode-type))
(define (decode-type type)
  (fetch type types/decoding 'decode-type))

(define (encode-command command)
  (fetch command commands/encoding 'encode-command))

(define (encode-execute-flags flags)
  (encode-flags flags execute-flags/encoding 'encode-execute-flags))
(define (decode-execute-flags n)
  (decode-flags n execute-flags/decoding 'decode-execute-flags))

;; null-map-length : (list-of boolean) -> integer
(define (null-map-length null-map)
  (ceiling (/ (length null-map) 8)))

;; null-map->integer : (list-of boolean) -> integer
;; Least significant bit represents first boolean in list, etc
(define (null-map->integer null-map)
  (cond [(null? null-map)
         0]
        [(car null-map)
         (+ 1 (arithmetic-shift (null-map->integer (cdr null-map)) 1))]
        [else
         (arithmetic-shift (null-map->integer (cdr null-map)) 1)]))

;; integer->null-map : number number -> (list-of boolean)
(define (integer->null-map n len)
  (if (zero? len)
      null
      (cons (= 1 (bitwise-and n 1))
            (integer->null-map (arithmetic-shift n -1) (sub1 len)))))

(define (at-eof? in)
  (eof-object? (peek-byte in)))

(define (parse-field-info fp)
  (match fp
    [(struct field-packet
             (cat db tab otab name oname _ len type flags _ _))
     `((catalog . ,cat)
       (database . ,db)
       (table . ,tab)
       (original-table . ,otab)
       (name . ,name)
       (original-name . ,oname)
       (length . ,len)
       (type . ,type)
       (*type* . ,type)
       (flags . ,flags))]))
