;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         openssl/sha1
         "../generic/interfaces.rkt"
         "../generic/prepared.rkt"
         "../generic/sql-data.rkt"
         "../generic/exceptions.rkt"
         "message.rkt"
         "dbsystem.rkt")
(provide connection%)

;; Debugging
(define DEBUG-RESPONSES #f)
(define DEBUG-SENT-MESSAGES #f)

(define MAX-PACKET-LENGTH #x1000000)

;; ========================================

(define connection%
  (class* object% (connection<%>)

    (define inport #f)
    (define outport #f)

    (super-new)

    ;; with-disconnect-on-error
    (define-syntax with-disconnect-on-error
      (syntax-rules ()
        [(with-disconnect-on-error . body)
         (with-handlers ([exn:fail? (lambda (e) (disconnect* #f) (raise e))])
           . body)]))

    ;; ========================================

    ;; == Communication locking

    ;; Lock; all communication occurs within lock.
    (define wlock (make-semaphore 1))

    (define/private (lock who require-connected?)
      (semaphore-wait wlock)
      (when (and require-connected? (not outport))
        (semaphore-post wlock)
        (error who "not connected")))

    (define/private (unlock)
      (semaphore-post wlock))

    (define/private (call-with-lock who proc
                                    #:require-connected? [require-connected? #t])
      (lock who require-connected?)
      (with-handlers ([values (lambda (e) (unlock) (raise e))])
        (begin0 (proc) (unlock))))

    ;; == Communication
    ;; (Must be called with lock acquired.)

    (define next-msg-num 0)

    (define/private (fresh-exchange)
      (set! next-msg-num 0))

    ;; send-message : message -> void
    (define/private (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    ;; buffer-message : message -> void
    (define/private (buffer-message msg)
      (when DEBUG-SENT-MESSAGES
        (fprintf (current-error-port) "  >> ~s\n" msg))
      (with-disconnect-on-error
       (write-packet outport msg next-msg-num)
       (set! next-msg-num (add1 next-msg-num))))

    ;; flush-message-buffer : -> void
    (define/private (flush-message-buffer)
      (with-disconnect-on-error
       (flush-output outport)))

    ;; recv : symbol/#f [(list-of symbol)] -> message
    ;; Automatically handles asynchronous messages
    (define/private (recv fsym expectation [field-dvecs #f])
      (define r
        (with-disconnect-on-error
         (recv* fsym expectation field-dvecs)))
      (when DEBUG-RESPONSES
        (eprintf "  << ~s\n" r))
      (when (error-packet? r)
        (raise-backend-error fsym r))
      r)

    (define/private (recv* fsym expectation field-dvecs)
      (define (advance . ss)
        (unless (or (not expectation) 
                    (null? ss)
                    (memq expectation ss))
          (error fsym "internal error: unexpected packet")))
      (define (err packet)
        (error fsym "internal error: unexpected packet"))
      (let-values ([(msg-num next) (parse-packet inport expectation field-dvecs)])
        (set! next-msg-num (add1 msg-num))
        (match next
          [(? handshake-packet?)
           (advance 'handshake)]
          [(? ok-packet?)
           (advance)]
          [(? error-packet?)
           (advance)]
          [(struct result-set-header-packet (field-count _))
           (advance 'result)]
          [(? field-packet?)
           (advance 'field)]
          [(? row-data-packet?)
           (advance 'data)]
          [(? binary-row-data-packet?)
           (advance 'binary-data)]
          [(? ok-prepared-statement-packet? result)
           (advance 'prep-ok)]
          [(? parameter-packet? result)
           (advance 'prep-params)]
          [(? eof-packet?)
           (advance 'field 'data 'binary-data 'prep-params)]
          [else
           (err next)])
        next))

    ;; ========================================

    ;; Connection management

    ;; disconnect : -> (void)
    (define/public (disconnect)
      (disconnect* #t))

    (define/private (disconnect* lock-not-held?)
      ;; If we don't hold the lock, try to acquire it and disconnect politely.
      (define politely? lock-not-held?)
      (define (go)
        (when DEBUG-SENT-MESSAGES
          (eprintf "  ** Disconnecting\n"))
        (when outport
          (when politely?
            (fresh-exchange)
            (send-message (make-command-packet 'quit "")))
          (close-output-port outport)
          (set! outport #f))
        (when inport
          (close-input-port inport)
          (set! inport #f)))
      (cond [politely?
             (call-with-lock 'disconnect go #:require-connected? #f)]
            [else (go)]))

    ;; connected? : -> boolean
    (define/public (connected?)
      (let ([outport outport])
        (and outport (not (port-closed? outport)))))

    (define/public (get-dbsystem)
      dbsystem)

    ;; ========================================

    ;; == Connect

    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! inport in)
      (set! outport out))

    ;; start-connection-protocol : string string string/#f -> void
    (define/public (start-connection-protocol dbname username password)
      (with-disconnect-on-error
        (fresh-exchange)
        (let ([r (recv 'mysql-connect 'handshake)])
          (match r
            [(struct handshake-packet (pver sver tid scramble capabilities charset status))
             (check-required-flags capabilities)
             (send-message
              (make-client-authentication-packet
               (desired-capabilities capabilities)
               MAX-PACKET-LENGTH
               'utf8-general-ci ;; charset
               username
               (scramble-password scramble password)
               dbname))
             (expect-auth-confirmation)]
            [_
             (error 'mysql-connect
                    "internal error: unknown message during authentication")]))))

    (define/private (check-required-flags capabilities)
      (for-each (lambda (rf)
                  (unless (memq rf capabilities)
                    (error 'mysql-connect
                           "server does not support required capability: ~s"
                           rf)))
                REQUIRED-CAPABILITIES))

    (define/private (desired-capabilities capabilities)
      (cons 'interactive
            (filter (lambda (c) (memq c DESIRED-CAPABILITIES))
                    capabilities)))

    ;; expect-auth-confirmation : -> void
    (define/private (expect-auth-confirmation)
      (let ([r (recv 'connect 'auth)])
        (match r
          [(struct ok-packet (_ _ status warnings message))
           (after-connect)]
          [_
           (error 'mysql-connect
                  "internal error: unknown message after authentication")])))

    ;; Set connection to use utf8 encoding
    (define/private (after-connect)
      (query 'mysql-connect "set names 'utf8'"
             (lambda (fields ordered?) (values #f void void #f)))
      (void))


    ;; ========================================

    ;; == Query

    ;; name-counter : number
    (define name-counter 0)

    ;; query : symbol Statement Collector -> QueryResult
    (define/public (query fsym stmt collector)
      (let-values ([(stmt result)
                    (call-with-lock fsym
                      (lambda ()
                        (let ([stmt (check-statement fsym stmt)])
                          (values stmt (query1 fsym stmt)))))])
        ;; For some reason this is *really* slow:
        ;; (statement:after-exec stmt)
        (query1:process-result fsym collector result)))

    ;; query1 : symbol Statement Collector -> QueryResult
    (define/private (query1 fsym stmt)
      (fresh-exchange)
      (query1:enqueue stmt)
      (query1:collect fsym (not (string? stmt))))

    ;; check-statement : symbol any -> statement-binding
    (define/private (check-statement fsym stmt)
      (cond [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)])
               (send pst check-owner fsym this stmt)
               stmt)]
            [(and (string? stmt) (force-prepare-sql? fsym stmt))
             (let ([pst (prepare1 fsym stmt #t)])
               (send pst bind fsym null))]
            [else stmt]))

    ;; query1:enqueue : statement -> void
    (define/private (query1:enqueue stmt)
      (cond [(statement-binding? stmt)
             (let* ([pst (statement-binding-pst stmt)]
                    [id (send pst get-handle)]
                    [params (statement-binding-params stmt)]
                    [null-map (map not params)]
                    [param-typeids (send pst get-param-typeids)])
               (send-message
                (make-execute-packet id null 1 null-map 1 param-typeids params)))]
            [else ;; string
             (send-message (make-command-packet 'query stmt))]))

    ;; query1:collect : symbol bool -> QueryResult stream
    (define/private (query1:collect fsym binary?)
      (let ([r (recv fsym 'result)])
        (match r
          [(struct ok-packet (affected-rows insert-id status warnings message))
           (vector 'command `((affected-rows . ,affected-rows)
                              (insert-id . ,insert-id)
                              (status . ,status)
                              (message . ,message)))]
          [(struct result-set-header-packet (fields extra))
           (query1:expect-fields fsym null binary?)])))

    (define/private (query1:expect-fields fsym r-field-dvecs binary?)
      (let ([r (recv fsym 'field)])
        (match r
          [(? field-packet?)
           (query1:expect-fields fsym (cons (parse-field-dvec r) r-field-dvecs) binary?)]
          [(struct eof-packet (warning-count status))
           (let ([field-dvecs (reverse r-field-dvecs)])
             (vector 'recordset field-dvecs (query1:get-rows fsym field-dvecs binary?)))])))

    (define/private (query1:get-rows fsym field-dvecs binary?)
      ;; Note: binary? should always be #f, unless force-prepare-sql? misses something.
      (let ([r (recv fsym (if binary? 'binary-data 'data) field-dvecs)])
        (match r
          [(struct row-data-packet (data))
           (cons data (query1:get-rows fsym field-dvecs binary?))]
          [(struct binary-row-data-packet (data))
           (cons data (query1:get-rows fsym field-dvecs binary?))]
          [(struct eof-packet (warning-count status))
           null])))

    (define/private (query1:process-result fsym collector result)
      (match result
        [(vector 'recordset field-dvecs rows)
         (let-values ([(init combine finalize headers?)
                       (collector (length field-dvecs) #t)])
           (recordset (and headers? (map field-dvec->field-info field-dvecs))
                      (finalize
                       (for/fold ([acc init]) ([row (in-list rows)])
                         (combine acc row)))))]
        [(vector 'command command-info)
         (simple-result command-info)]))

    ;; == Prepare

    ;; prepare : symbol string boolean -> PreparedStatement
    (define/public (prepare fsym stmt close-on-exec?)
      (call-with-lock fsym
        (lambda ()
          (prepare1 fsym stmt close-on-exec?))))

    (define/private (prepare1 fsym stmt close-on-exec?)
      (fresh-exchange)
      (send-message (make-command-packet 'statement-prepare stmt))
      (let ([r (recv fsym 'prep-ok)])
        (match r
          [(struct ok-prepared-statement-packet (id fields params))
           (let ([param-dvecs
                  (if (zero? params) null (prepare1:get-field-descriptions fsym))]
                 [field-dvecs
                  (if (zero? fields) null (prepare1:get-field-descriptions fsym))])
             (new prepared-statement%
                  (handle id)
                  (close-on-exec? close-on-exec?)
                  (param-typeids (map field-dvec->typeid param-dvecs))
                  (result-dvecs field-dvecs)
                  (owner this)))])))

    (define/private (prepare1:get-field-descriptions fsym)
      (let ([r (recv fsym 'field)])
        (match r
          [(struct eof-packet (warning-count status))
           null]
          [(? field-packet?)
           (cons (parse-field-dvec r) (prepare1:get-field-descriptions fsym))])))

    (define/public (free-statement pst)
      (call-with-lock 'free-statement
        #:require-connected? #f
        (lambda ()
          (let ([id (send pst get-handle)])
            (when (and id outport) ;; outport = connected?
              (send pst set-handle #f)
              (fresh-exchange)
              (send-message (make-command:statement-packet 'statement-close id)))))))))


;; ========================================

;; scramble-password : bytes string -> bytes
(define (scramble-password scramble password)
  (and scramble password
       (let* ([password (string->bytes/latin-1 password)]
              [stage1 (sha1-bytes (open-input-bytes password))]
              [stage2 (sha1-bytes (open-input-bytes stage1))]
              [stage3 (sha1-bytes (open-input-bytes (bytes-append scramble stage2)))]
              [reply (bytes-xor stage1 stage3)])
         reply)))

;; bytes-xor : bytes bytes -> bytes
;; Assumes args are same length
(define (bytes-xor a b)
  (let ([c (make-bytes (bytes-length a))])
    (let loop ([i 0])
      (when (< i (bytes-length c))
        (bytes-set! c i
                    (bitwise-xor (bytes-ref a i) (bytes-ref b i)))
        (loop (add1 i))))
    c))

(define REQUIRED-CAPABILITIES
  '(long-flag
    connect-with-db
    protocol-41
    secure-connection))

(define DESIRED-CAPABILITIES
  '(long-password
    long-flag
    transactions
    protocol-41
    secure-connection
    connect-with-db))

;; raise-backend-error : symbol ErrorPacket -> raises exn
(define (raise-backend-error who r)
  (define code (error-packet-sqlstate r))
  (define message (error-packet-message r))
  (define props (list (cons 'errno (error-packet-errno r))
                      (cons 'code code)
                      (cons 'message message)))
  (raise-sql-error who code message props))

;; ========================================

#|
MySQL allows only certain kinds of statements to be prepared; the rest
must go through the old execution path. See here:
  http://dev.mysql.com/doc/refman/5.0/en/c-api-prepared-statements.html
According to that page, the following statements may be prepared:

  CALL, CREATE TABLE, DELETE, DO, INSERT, REPLACE, SELECT, SET, UPDATE,
  and most SHOW statements

On the other hand, we want to force all recordset-returning statements
through the prepared-statement path to use the binary data
protocol. That would seem to be the following:

  CALL (?) and SELECT

The following bit of heinously offensive code determines the kind of
SQL statement is contained in a string.

----

3 kinds of comments in mysql SQL:
  - "#" to end of line
  - "-- " to end of line
  - "/*" to next "*/" (not nested), except some weird conditional-inclusion stuff

I'll ignore the third kind.
|#

(define (force-prepare-sql? fsym stmt)
  (let ([kw (get-sql-keyword stmt)])
    (cond [(not kw)
           ;; better to have unpreparable stmt rejected than
           ;; to have SELECT return unconvered types
           #t]
          [(string-ci=? kw "select") #t]
          [(string-ci=? kw "call") #t]
          [else #f])))

(define sql-statement-rx
  #rx"^(?:(?:#[^\n\r]*[\n\r])|(?:-- [^\n\r]*[\n\r])|[ \t\n\r])*([A-Za-z]+)")

(define (get-sql-keyword stmt)
  (let ([m (regexp-match sql-statement-rx stmt)])
    (and m (cadr m))))
