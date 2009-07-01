;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; Implementation of connections, which communicate with a backend through
;; structured messages.

#lang scheme/base
(require scheme/class
         scheme/match
         (only-in (planet "digest.ss" ("soegaard" "digest.plt" 1 2))
                  sha1-bytes)
         "../generic/interfaces.ss"
         "../generic/sql-data.ss"
         "../generic/query.ss"
         "message.ss"
         "dbsystem.ss"
         "types.ss")
(provide connection%)

;; Debugging
(define DEBUG-RESPONSES #t)
(define DEBUG-SENT-MESSAGES #t)

(define MAX-PACKET-LENGTH #x1000000)

;; A PreparedStatement is:
;;   (make-MySQLPreparedStatement int int
;;                                int (list-of FieldInfo)
;;                                int (list-of FieldInfo)
;;                                (weak-box connection))
(define-struct (MySQLPreparedStatement PreparedStatement)
  (id params paraminfos fields fieldinfos wcx))

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

    (define/private (get-message* expectation param-types)
      (define (advance . ss)
        (unless (or (not expectation) 
                    (null? ss)
                    (memq expectation ss))
          (error 'get-message "unexpected packet (wanted ~s)"
                 expectation)))
      (define (err packet)
        (error 'get-message "unexpected packet: ~s" packet))
      (let-values ([(msg-num next) (parse-packet inport expectation param-types)])
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

    (define/public (alive?) #t)

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
  (class* object% (connection:admin<%> mysql-base<%>)
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
        (error 'backend "error: ~s" r)
        #| (raise-mysql-backend-error behalf r) |# )
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
      (when politely?
        (fresh-exchange)
        (send-message (make-command-packet 'quit "")))
      (send backend-link close)
      (set! backend-link disconnected-backend-link))

    ;; connected? : -> boolean
    (define/public (connected?)
      (send backend-link alive?))

    ;; System

    (define/public (get-system)
      dbsystem)

    ;; Initialization

    (define/public (after-connect)
      (void))
    ))

;; scramble-password : bytes string -> bytes
(define (scramble-password scramble password)
  (and scramble password
       (let* ([password (string->bytes/latin-1 password)]
              [stage1 (sha1-bytes password)]
              [stage2 (sha1-bytes stage1)]
              [stage3 (sha1-bytes (bytes-append scramble stage2))]
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

;; primitive-query-mixin
;; Handles the mechanics of connection creations, queries, etc.
;; Provides functionality, not usability. See connection% for friendly 
;; interface.
(define primitive-query-mixin
  (mixin (mysql-base<%> primitive-query<%>) (primitive-query/prepare<%>)
    (inherit recv
             send-message
             fresh-exchange
             datum->external-representation)
    (super-new)

    ;; name-counter : number
    (define name-counter 0)

    ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
    ;; The single point of control for the query engine
    (define/override (query*/no-conversion fsym stmts collector)
      (for ([stmt stmts])
        (check-statement fsym stmt))
      (for/list ([stmt stmts])
        (query1 fsym stmt collector)))

    ;; query1 : symbol Statement Collector -> QueryResult
    (define/private (query1 fsym stmt collector)
      (fresh-exchange)
      (query1:enqueue stmt)
      (query1:collect stmt collector))

    ;; check-statement : symbol any -> void
    (define/private (check-statement fsym stmt)
      (unless (or (string? stmt) (StatementBinding? stmt))
        (raise-type-error fsym "string or StatementBinding" stmt))
      (when (StatementBinding? stmt)
        (check-prepared-statement fsym (StatementBinding-pst stmt))))

    ;; check-prepared-statement : symbol any -> void
    (define/private (check-prepared-statement fsym pst)
      (unless (MySQLPreparedStatement? pst)
        (raise-type-error fsym
                          "StatementBinding containing prepared statement"))
      (unless (eq? this (weak-box-value (MySQLPreparedStatement-wcx pst)))
        (raise-mismatch-error 
         fsym
         "prepared statement owned by another connection"
         pst)))

    ;; query1:enqueue : Statement -> void
    (define/private (query1:enqueue stmt)
      (cond
        [(string? stmt)
         (send-message (make-command-packet 'query stmt))]
        [(StatementBinding? stmt)
         (let* ([pst (StatementBinding-pst stmt)]
                [id (MySQLPreparedStatement-id pst)]
                [params (StatementBinding-params stmt)]
                [null-map (map not params)]
                [param-types
                 (map get-type (MySQLPreparedStatement-paraminfos pst))])
           #|
           (define (send-param pos data)
             (when data
               (send-message (make-long-data-packet id pos 0 data))))
           |#
           #|
           (let loop ([pos 0] [params params])
             (when (pair? params)
               (send-param pos (car params))
               (loop (add1 pos) (cdr params))))
           |#
           (send-message
            (make-execute-packet id null 1 null-map 1 param-types params)))]))

    ;; query1:collect : Statement Collector -> QueryResult stream
    (define/private (query1:collect stmt collector)
      (cond
        [(string? stmt)
         (query1:result #f collector)]
        [(StatementBinding? stmt)
         (let* ([pst (StatementBinding-pst stmt)]
                [fieldinfos (MySQLPreparedStatement-fieldinfos pst)]
                [types (map get-type fieldinfos)])
           (query1:result types collector))]))

    (define/private (query1:result binary? collector)
      (let ([r (recv 'query* 'result)])
        (match r
          [(struct ok-packet (affected-rows insert-id status warnings message))
           (make-SimpleResult message)]
          [(struct result-set-header-packet (fields extra))
           (query1:expect-fields binary? null collector)])))

    (define/private (query1:expect-fields binary? fieldinfos collector)
      (let ([r (recv 'query* 'field)])
        (match r
          [(? field-packet?)
           (query1:expect-fields binary? 
                                 (cons (parse-field-info r) fieldinfos)
                                 collector)]
          [(struct eof-packet (warning-count status))
           (let-values ([(init combine finalize info)
                         (collector (reverse fieldinfos) binary?)])
             (query1:data-loop binary? init combine finalize info))])))

    (define/private (query1:get-data binary?)
      (let ([r (recv 'query* (if binary? 'binary-data 'data) binary?)])
        (match r
          [(struct row-data-packet (data))
           (cons data (query1:get-data binary?))]
          [(struct binary-row-data-packet (data))
           (cons data (query1:get-data binary?))]
          [(struct eof-packet (warning-count status))
           null])))

    (define/private (query1:data-loop binary? init combine finalize info)
      (define data (query1:get-data binary?))
      (let loop ([init init] [data data])
        (if (pair? data)
            (loop (apply combine init (car data)) (cdr data))
            (make-Recordset info (finalize init)))))

    ;; prepare-multiple : (list-of string) -> (list-of PreparedStatement)
    (define/public (prepare-multiple stmts)
      (for-each (lambda (stmt)
                  (unless (string? stmt)
                    (raise-type-error 'prepare* "string" stmt)))
                stmts)
      (map (lambda (stmt) (prepare1 stmt)) stmts))

    (define/private (prepare1 stmt)
      (fresh-exchange)
      (send-message (make-command-packet 'statement-prepare stmt))
      (let ([r (recv 'prepare* 'prep-ok)])
        (match r
          [(struct ok-prepared-statement-packet (id fields params))
           (let ([paraminfos
                  (if (zero? params) null (prepare1:get-params))]
                 [fieldinfos
                  (if (zero? fields) null (prepare1:get-fields))])
             (make-MySQLPreparedStatement fields
                                          id
                                          params paraminfos
                                          fields fieldinfos
                                          (make-weak-box this)))])))

    (define/private (prepare1:get-params)
      (let ([r (recv 'prepare* 'field)])
        (match r
          [(struct eof-packet (warning-count status))
           null]
          [(? field-packet?)
           (cons (parse-field-info r) (prepare1:get-params))])))

    (define/private (prepare1:get-fields)
      (let ([r (recv 'prepare* 'field)])
        (match r
          [(struct eof-packet (warning-count status))
           null]
          [(? field-packet?)
           (cons (parse-field-info r) (prepare1:get-fields))])))

    ;; bind-prepared-statement : PreparedStatement (list-of value)
    ;;                        -> StatementBinding
    (define/public (bind-prepared-statement pst params)
      #|
      (printf "wcx = ~s\n"
              (MySQLPreparedStatement-wcx pst))
      (printf "owner = this? : ~s\n"
              (eq? this (weak-box-value (MySQLPreparedStatement-wcx pst))))
      |#
      (unless (MySQLPreparedStatement? pst)
        (raise-type-error 'bind-prepared-statement "prepared statement" pst))
      (unless (eq? this (weak-box-value (MySQLPreparedStatement-wcx pst)))
        (raise-mismatch-error 'bind-prepared-statement
                              "prepared statement is owned by another connection"
                              pst))
      (unless (list? params)
        (raise-type-error 'bind-prepared-statement "list" params))
      (match pst
        [(struct MySQLPreparedStatement
                 (results id paramc paraminfos fields fieldinfos wcx))
         (check-params params paraminfos)
         (let* ([param-types (map get-type paraminfos)]
                [params
                 (map (lambda (p t)
                        (if (sql-null? p)
                            #f
                            (datum->external-representation t p)))
                      params 
                      param-types)])
           (make-StatementBinding pst params))]))

    (define/private (check-params params paraminfos)
      (define len (length params))
      (define tlen (length paraminfos))
      (when (not (= len tlen))
        (raise-user-error
         'bind-prepared-statement
         "prepared statement requires ~s parameters, given ~s" tlen len)))

    ))

;; connection%
(define connection% 
  (class (prepare-query-mixin
          (query-mixin
           (primitive-query-mixin
            (primitive-query-base-mixin
             (connector-mixin
              mysql-base%)))))
    (super-new)))
