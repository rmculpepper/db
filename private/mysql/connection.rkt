;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         openssl/sha1
         "../generic/interfaces.rkt"
         "../generic/connection.rkt"
         "../generic/sql-data.rkt"
         "../generic/query.rkt"
         "message.rkt"
         "exceptions.rkt"
         "dbsystem.rkt")
(provide connection%)

;; Debugging
(define DEBUG-RESPONSES #f)
(define DEBUG-SENT-MESSAGES #f)

(define MAX-PACKET-LENGTH #x1000000)

(define prepared-statement%
  (class prepared-statement-base%
    (init-private id) ;; int
    (define/public (get-id) id)
    (super-new)))

(define mysql-base<%>
  (interface ()
    recv
    send-message
    buffer-message
    flush-message-buffer
    fresh-exchange
    disconnect
    disconnect*
    connected?
    after-connect))

(define mysql-backend-link%
  (class object%
    (init-field inport
                outport)
    (define next-msg-num 0)

    (define/public (encode msg)
      (write-packet outport msg next-msg-num)
      (set! next-msg-num (add1 next-msg-num)))

    (define/public (fresh-exchange)
      (set! next-msg-num 0))

    (define/public (flush)
      (flush-output outport))

    (define/public (close)
      (close-output-port outport)
      (close-input-port inport))

    (define/public get-message
      (case-lambda 
        [(expectation) (get-message* expectation #f)]
        [(expectation types) (get-message* expectation types)]))

    (define/private (get-message* expectation result-infos)
      (define (advance . ss)
        (unless (or (not expectation) 
                    (null? ss)
                    (memq expectation ss))
          (error 'get-message "unexpected packet (wanted ~s)"
                 expectation)))
      (define (err packet)
        (error 'get-message "unexpected packet: ~s" packet))
      (let-values ([(msg-num next) (parse-packet inport expectation result-infos)])
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

    (define/public (alive?)
      (not (port-closed? outport)))

    (super-new)))

(define disconnected-backend-link%
  (class object%
    (define/public (close)
      (void))
    (define/public (encode . args)
      (illegal))
    (define/public (flush)
      (illegal))
    (define/public (fresh-exchange)
      (illegal))
    (define/public (get-message . s)
      (illegal))

    (define/public (alive?) #f)

    (define/private (illegal)
      (error 'backend-link "not connected"))
    (super-new)))

(define disconnected-backend-link
  (new disconnected-backend-link%))

;; mysql-base%
(define mysql-base%
  (class* object% (mysql-base<%> connection:admin<%>)
    (init-field [backend-link disconnected-backend-link])
    (super-new)

    ;; with-disconnect-on-error
    ;; Specialized to use direct method call rather than 'send'
    (define-syntax with-disconnect-on-error
      (syntax-rules ()
        [(with-disconnect-on-error expr)
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (disconnect* #f)
                            (raise e))])
           expr)]))

    ;; Communication Methods

    ;; recv : symbol/#f [(list-of symbol)] -> message
    ;; Automatically handles asynchronous messages
    (define/public (recv behalf . args)
      (define r
        (with-disconnect-on-error
            (send/apply backend-link get-message args)))
      (when DEBUG-RESPONSES
        (fprintf (current-error-port) "  << ~s\n" r))
      (when (error-packet? r)
        (raise-backend-error behalf r))
      r)

    ;; send-message : message -> void
    (define/public (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    ;; fresh-exchange : -> void
    (define/public (fresh-exchange)
      (send backend-link fresh-exchange))

    ;; buffer-message : message -> void
    (define/public (buffer-message msg)
      (when DEBUG-SENT-MESSAGES
        (fprintf (current-error-port) "  >> ~s\n" msg))
      (with-disconnect-on-error
          (send backend-link encode msg)))

    ;; flush-message-buffer : -> void
    (define/public (flush-message-buffer)
      (with-disconnect-on-error
          (send backend-link flush)))

    ;; Connection management

    ;; disconnect : -> (void)
    (define/public (disconnect)
      (disconnect* #t))

    (define/public (disconnect* politely?)
      (when (connected?)
        (when politely?
          (fresh-exchange)
          (send-message (make-command-packet 'quit "")))
        (send backend-link close)
        (set! backend-link disconnected-backend-link)))

    ;; connected? : -> boolean
    (define/public (connected?)
      (send backend-link alive?))

    ;; System

    (define/public (get-dbsystem)
      dbsystem)

    ;; Initialization

    (define/public (after-connect)
      (void))
    ))

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

;; connector-mixin%
(define connector-mixin
  (mixin (mysql-base<%>) (connector<%>)
    (inherit-field backend-link)
    (inherit recv
             send-message
             fresh-exchange
             disconnect
             disconnect*
             after-connect)
    (super-new)

    ;; with-disconnect-on-error
    ;; Specialized to use direct method call rather than 'send'
    (define-syntax with-disconnect-on-error
      (syntax-rules ()
        [(with-disconnect-on-error . body)
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (disconnect* #f)
                            (raise e))])
           . body)]))

    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! backend-link
            (new mysql-backend-link% (inport in) (outport out))))

    ;; start-connection-protocol : string string string/#f -> void
    (define/public (start-connection-protocol dbname username password)
      (with-disconnect-on-error
          (fresh-exchange)
        (let ([r (recv 'connect 'handshake)])
          (match r
            [(struct handshake-packet
                     (pver sver tid scramble capabilities charset status))
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
             (error 'connect
                    "authentication failed (backend sent unexpected message)")]))))

    (define/private (check-required-flags capabilities)
      (for-each (lambda (rf)
                  (unless (memq rf capabilities)
                    (error 'connect
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
           (error 'connect
                  (string-append "connection failed after authentication "
                                 "(backend sent unexpected message)"))])))

    ))

;; query-mixin
;; Handles the mechanics of connection creations, queries, etc.
;; Provides functionality, not usability. See connection% for friendly 
;; interface.
(define query-mixin
  (mixin (mysql-base<%> connection:admin<%>) ()
    (inherit recv
             send-message
             fresh-exchange
             get-dbsystem)
    (super-new)

    ;; name-counter : number
    (define name-counter 0)

    ;; NOTE: Always prepare statement so that query goes through the binary
    ;; data path. See code in methods below marked TEXT for disabled code and
    ;; comments about the disabled text data path. See also get-result-handlers.

    ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
    ;; The single point of control for the query engine
    (define/public (query* fsym stmts collector)
      (let ([collector collector])
        ;; TEXT: (compose-collector-with-conversions (get-dbsystem) collector)
        (let ([stmts
               (for/list ([stmt (in-list stmts)])
                 (check-statement fsym stmt))])
          (for/list ([stmt (in-list stmts)])
            (query1 fsym stmt collector)))))

    ;; query1 : symbol Statement Collector -> QueryResult
    (define/private (query1 fsym stmt collector)
      (fresh-exchange)
      (query1:enqueue stmt)
      (query1:collect stmt collector))

    ;; check-statement : symbol any -> statement-binding
    (define/private (check-statement fsym stmt)
      (cond [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)])
               (send pst check-owner fsym this stmt)
               stmt)]
            [(string? stmt)
             ;; TEXT: leave string alone to re-enable text data path
             (let ([pst (prepare1 fsym stmt)])
               (send pst bind fsym null))]))

    ;; query1:enqueue : Statement -> void
    (define/private (query1:enqueue stmt)
      (cond [(string? stmt)
             ;; TEXT: This clause is currently unreachable.
             (send-message (make-command-packet 'query stmt))]
            [(statement-binding? stmt)
             (let* ([pst (statement-binding-pst stmt)]
                    [id (send pst get-id)]
                    [params (statement-binding-params stmt)]
                    [null-map (map not params)]
                    [param-types (send pst get-param-types)])
               (send-message
                (make-execute-packet id null 1 null-map 1 param-types params)))]))

    ;; query1:collect : Statement Collector -> QueryResult stream
    (define/private (query1:collect stmt collector)
      (cond
        [(string? stmt)
         (query1:result #f collector)]
        [(statement-binding? stmt)
         (let* ([pst (statement-binding-pst stmt)]
                [result-infos (send pst get-result-infos)])
           (query1:result result-infos collector))]))

    (define/private (query1:result result-infos collector)
      (let ([r (recv 'query* 'result)])
        (match r
          [(struct ok-packet (affected-rows insert-id status warnings message))
           (simple-result `((affected-rows . ,affected-rows)
                            (insert-id . ,insert-id)
                            (status . ,status)
                            (message . ,message)))]
          [(struct result-set-header-packet (fields extra))
           (query1:expect-fields result-infos null collector)])))

    (define/private (query1:expect-fields result-infos fieldinfos collector)
      (let ([r (recv 'query* 'field)])
        (match r
          [(? field-packet?)
           (query1:expect-fields result-infos
                                 (cons (parse-field-info r) fieldinfos)
                                 collector)]
          [(struct eof-packet (warning-count status))
           (let-values ([(init combine finalize info)
                         (collector (reverse fieldinfos) result-infos)])
             (query1:data-loop result-infos init combine finalize info))])))

    (define/private (query1:get-data result-infos)
      (let ([r (recv 'query* (if result-infos 'binary-data 'data) result-infos)])
        (match r
          [(struct row-data-packet (data))
           (cons data (query1:get-data result-infos))]
          [(struct binary-row-data-packet (data))
           (cons data (query1:get-data result-infos))]
          [(struct eof-packet (warning-count status))
           null])))

    (define/private (query1:data-loop result-infos init combine finalize info)
      (define data (query1:get-data result-infos))
      (let loop ([init init] [data data])
        (if (pair? data)
            (loop (combine init (list->vector (car data))) (cdr data))
            (recordset info (finalize init)))))

    ;; prepare* : symbol (list-of string) -> (list-of PreparedStatement)
    (define/public (prepare* fsym stmts)
      (map (lambda (stmt) (prepare1 fsym stmt)) stmts))

    (define/private (prepare1 fsym stmt)
      (fresh-exchange)
      (send-message (make-command-packet 'statement-prepare stmt))
      (let ([r (recv fsym 'prep-ok)])
        (match r
          [(struct ok-prepared-statement-packet (id fields params))
           (let ([paraminfos
                  (if (zero? params) null (prepare1:get-params fsym))]
                 [fieldinfos
                  (if (zero? fields) null (prepare1:get-fields fsym))])
             (new prepared-statement%
                  (id id)
                  (param-infos paraminfos)
                  (result-infos fieldinfos)
                  (owner this)))])))

    (define/private (prepare1:get-params fsym)
      (let ([r (recv fsym 'field)])
        (match r
          [(struct eof-packet (warning-count status))
           null]
          [(? field-packet?)
           (cons (parse-field-info r) (prepare1:get-params))])))

    (define/private (prepare1:get-fields fsym)
      (let ([r (recv fsym 'field)])
        (match r
          [(struct eof-packet (warning-count status))
           null]
          [(? field-packet?)
           (cons (parse-field-info r) (prepare1:get-fields fsym))])))
    ))

;; connection%
(define connection% 
  (class* (query-mixin
           (connector-mixin
            mysql-base%))
      (connection<%>)
    (super-new)
    (inherit query*)

    ;; Set connection to use utf8 encoding
    (define/override (after-connect)
      (super after-connect)
      (query* 'after-connect (list "set names 'utf8'")
              (lambda (fields binary?)
                (values #f void void #f)))
      (void))))
