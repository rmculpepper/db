;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         racket/vector
         file/md5
         openssl/mzssl
         "../generic/interfaces.rkt"
         "../generic/connection.rkt"
         "../generic/sql-data.rkt"
         "../generic/query.rkt"
         "msg.rkt"
         "exceptions.rkt"
         "dbsystem.rkt")
(provide connection%)

;; Debugging
(define DEBUG-RESPONSES #f)
(define DEBUG-SENT-MESSAGES #f)

;; ========================================

(define prepared-statement%
  (class prepared-statement-base%
    (init-private name)
    (define/public (get-name) name)
    (super-new)))

;; ========================================

;; base<%>
;; Manages communication
(define base<%>
  (interface ()
    recv-message           ;; -> message
    send-message           ;; message -> void
    buffer-message         ;; mesage -> void
    flush-message-buffer   ;; -> void
    check-ready-for-query  ;; symbol boolean -> void

    lock                   ;; symbol -> void
    unlock                 ;; -> void
    call-with-lock         ;; symbol (-> any) -> any

    disconnect*            ;; [boolean] -> void
    connected?             ;; -> boolean
    ))

;; ========================================

(define connection-base%
  (class* object% (connection<%> connector<%>)
    (init-private notice-handler
                  notification-handler
                  allow-cleartext-password?)
    (define inport #f)
    (define outport #f)
    (define process-id #f)
    (define secret-key #f)

    (super-new)

    ;; with-disconnect-on-error
    (define-syntax-rule (with-disconnect-on-error . body)
      (with-handlers ([exn:fail? (lambda (e) (disconnect* #f) (raise e))])
        . body))

    ;; ========================================

    ;; == Communication locking

    ;; Lock; all communication occurs within lock.
    (define wlock (make-semaphore 1))

    ;; Delay async handler calls until unlock.
    (define delayed-handler-calls null)

    (define/public (lock who)
      (semaphore-wait wlock)
      (unless outport
        (semaphore-post wlock)
        (error who "not connected")))

    (define/public (unlock)
      (let ([handler-calls delayed-handler-calls])
        (set! delayed-handler-calls null)
        (semaphore-post wlock)
        (for-each (lambda (p) (p)) handler-calls)))

    (define/public (call-with-lock who proc)
      (lock who)
      (with-handlers ([values (lambda (e) (unlock) (raise e))])
        (begin0 (proc) (unlock))))

    ;; == Communication
    ;; (Must be called with lock acquired.)

    ;; raw-recv : -> message
    (define/private (raw-recv)
      (with-disconnect-on-error
       (let ([r (parse-server-message inport)])
         (when DEBUG-RESPONSES
           (fprintf (current-error-port) "  << ~s\n" r))
         r)))

    ;; recv-message : symbol -> message
    (define/public (recv-message fsym)
      (let ([r (raw-recv)])
        (cond [(ErrorResponse? r)
               (check-ready-for-query fsym #t) ;; FIXME: eat msgs until ReadyForQuery?
               (raise-backend-error fsym r)]
              [(or (NoticeResponse? r)
                   (NotificationResponse? r)
                   (ParameterStatus? r))
               (handle-async-message fsym r)
               (recv-message fsym)]
              [else r])))

    ;; send-message : message -> void
    (define/public (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    ;; buffer-message : message -> void
    (define/public (buffer-message msg)
      (when DEBUG-SENT-MESSAGES
        (fprintf (current-error-port) "  >> ~s\n" msg))
      (with-disconnect-on-error
       (write-message msg outport)))

    ;; flush-message-buffer : -> void
    (define/public (flush-message-buffer)
      (with-disconnect-on-error
       (flush-output outport)))

    ;; check-ready-for-query : symbol -> void
    (define/public (check-ready-for-query fsym or-eof?)
      (let ([r (recv-message fsym)])
        (cond [(ReadyForQuery? r) (void)]
              [(and or-eof? (eof-object? r)) (void)]
              [else
               (error fsym "internal error: backend sent unexpected message")])))

    ;; == Asynchronous messages

    ;; handle-async-message : message -> void
    (define/private (handle-async-message fsym msg)
      (match msg
        [(struct NoticeResponse (properties))
         (set! delayed-handler-calls
               (cons (lambda ()
                       (notice-handler (cdr (assq 'code properties))
                                       (cdr (assq 'message properties))))
                     delayed-handler-calls))]
        [(struct NotificationResponse (pid condition info))
         (set! delayed-handler-calls
               (cons (lambda ()
                       (notification-handler condition))
                     delayed-handler-calls))]
        [(struct ParameterStatus (name value))
         (cond [(equal? name "client_encoding")
                (unless (equal? value "UTF8")
                  (disconnect* #f)
                  (error fsym
                         (string-append
                          "backend attempted to change the client character encoding "
                          "from UTF8 to ~a, disconnecting")
                         value))]
               [else (void)])]))

    ;; == Connection management

    ;; disconnect : [boolean] -> (void)
    (define/public (disconnect)
      (disconnect* #t))

    ;; disconnect* : boolean -> void
    (define/public (disconnect* no-lock-held?)
      ;; If we don't hold the lock, try to acquire it and disconnect politely.
      (define politely? no-lock-held?)
      (when (connected?)
        (when politely?
          (lock 'disconnect)
          (send-message (make-Terminate)))
        (when DEBUG-SENT-MESSAGES
          (fprintf (current-error-port) "  ** Disconnecting\n"))
        (when inport
          (close-input-port inport)
          (set! inport #f))
        (when outport
          (close-output-port outport)
          (set! outport #f))
        (when politely?
          (unlock))))

    ;; connected? : -> boolean
    (define/public (connected?)
      (let ([outport outport])
        (and outport (not (port-closed? outport)))))

    ;; == System

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
       (call-with-lock 'postgresql-connect
        (lambda ()
          (send-message
           (make-StartupMessage
            (list (cons "user" username)
                  (cons "database" dbname)
                  (cons "client_encoding" "UTF8")
                  (cons "DateStyle" "ISO, MDY"))))
          (connect:expect-auth username password)))))

    ;; connect:expect-auth : string/#f -> ConnectionResult
    (define/private (connect:expect-auth username password)
      (let ([r (recv-message 'postgresql-connect)])
        (match r
          [(struct AuthenticationOk ())
           (connect:expect-ready-for-query)]
          [(struct AuthenticationCleartextPassword ())
           (unless (string? password)
             (error 'postgresql-connect "password needed but not supplied"))
           (unless allow-cleartext-password?
             (error 'postgresql-connect (nosupport "cleartext password")))
           (send-message (make-PasswordMessage password))
           (connect:expect-auth username password)]
          [(struct AuthenticationCryptPassword (salt))
           (unless #f ;; crypt() support removed
             (error 'postgresql-connect (nosupport "crypt()-encrypted password")))
           (unless (string? password)
             (error 'postgresql-connect "password needed but not supplied"))
           (send-message (make-PasswordMessage (crypt-password password salt)))
           (connect:expect-auth username password)]
          [(struct AuthenticationMD5Password (salt))
           (unless (string? password)
             (error 'postgresql-connect "password needed but not supplied"))
           (send-message (make-PasswordMessage (md5-password username password salt)))
           (connect:expect-auth username password)]
          [(struct AuthenticationKerberosV5 ())
           (error 'postgresql-connect (nosupport "KerberosV5 authentication"))]
          [(struct AuthenticationSCMCredential ())
           (error 'postgresql-connect (nosupport "SCM authentication"))]
          ;; ErrorResponse handled by recv-message
          [_
           (error 'postgresql-connect
                  "internal error: unknown message during authentication")])))

    ;; connect:expect-ready-for-query : -> void
    (define/private (connect:expect-ready-for-query)
      (let ([r (recv-message 'postgresql-connect)])
        (match r
          [(struct ReadyForQuery (status))
           (void)]
          [(struct BackendKeyData (pid secret))
           (set! process-id pid)
           (set! secret-key secret)
           (connect:expect-ready-for-query)]
          [_
           (error 'postgresql-connect
                  "internal error: unknown message after authentication")])))

    ;; ============================================================

    ;; == Query

    ;; query* : symbol (list-of Statement) Collector
    ;;       -> (list-of QueryResult)
    (define/public (query* fsym stmts collector)
      (for ([stmt (in-list stmts)])
        (check-statement fsym stmt))
      (let ([results
             (call-with-lock fsym
               (lambda ()
                 (for ([stmt (in-list stmts)])
                   (query1:enqueue stmt))
                 (send-message (make-Sync))
                 (begin0 (for/list ([stmt (in-list stmts)])
                           (query1:collect fsym stmt))
                   (check-ready-for-query fsym #f))))])
        (map (lambda (result) (query1:process-result fsym collector result))
             results)))

    ;; query1:enqueue : Statement -> void
    (define/private (query1:enqueue stmt)
      (if (string? stmt)
          (begin (buffer-message (make-Parse "" stmt null))
                 (buffer-message (make-Bind "" "" null null null)))
          (let* ([pst (statement-binding-pst stmt)]
                 [pst-name (send pst get-name)]
                 [params (statement-binding-params stmt)])
            (buffer-message (make-Bind "" pst-name null params null))))
      (buffer-message (make-Describe 'portal ""))
      (buffer-message (make-Execute "" 0))
      (buffer-message (make-Close 'portal "")))

    ;; query1:collect : symbol Statement bool bool -> QueryResult stream
    (define/private (query1:collect fsym stmt)
      (if (string? stmt)
          (query1:expect-parse-complete fsym)
          (query1:expect-bind-complete fsym)))

    (define/private (query1:expect-parse-complete fsym)
      (let ([r (recv-message fsym)])
        (match r
          [(struct ParseComplete ())
           (query1:expect-bind-complete fsym)]
          [_ (query1:error-recovery fsym r)])))

    (define/private (query1:expect-bind-complete fsym)
      (let ([r (recv-message fsym)])
        (match r
          [(struct BindComplete ())
           (query1:expect-portal-description fsym)]
          [_ (query1:error-recovery fsym r)])))

    (define/private (query1:expect-portal-description fsym)
      (let ([r (recv-message fsym)])
        (match r
          [(struct RowDescription (field-dvecs))
           (query1:data-loop fsym field-dvecs null)]
          [(struct NoData ())
           (query1:expect-completion fsym)]
          [_ (query1:error-recovery fsym r)])))

    (define/private (query1:data-loop fsym field-dvecs rows)
      (let ([r (recv-message fsym)])
        (match r
          [(struct DataRow (value))
           (query1:data-loop fsym field-dvecs (cons (list->vector value) rows))]
          [(struct CommandComplete (command))
           (query1:finalize fsym (vector 'recordset field-dvecs rows))]
          [_ (query1:error-recovery fsym r)])))

    (define/private (query1:expect-completion fsym)
      (let ([r (recv-message fsym)])
        (match r
          [(struct CommandComplete (command))
           (query1:finalize fsym (vector 'command command))]
          [(struct EmptyQueryResponse ())
           (query1:finalize fsym (lambda () (simple-result '())))]
          [_ (query1:error-recovery fsym r)])))

    (define/private (query1:finalize fsym result)
      (let ([r (recv-message fsym)])
        (match r
          [(struct CloseComplete ())
           result]
          [_ (query1:error-recovery fsym r)])))

    (define/private (query1:error-recovery fsym r)
      (match r
        [(struct CopyInResponse (format column-formats))
         (error fsym (nosupport "COPY IN statements"))]
        [(struct CopyOutResponse (format column-formats))
         (error fsym (nosupport "COPY OUT statements"))]
        [_ (error fsym "internal error: unexpected message")]))

    (define/private (query1:process-result fsym collector result)
      (match result
        [(vector 'recordset field-dvecs rows)
         (let-values ([(init combine finalize headers?)
                       (collector (length field-dvecs) #f)])
           (let* ([type-reader-v
                   (list->vector (query1:get-type-readers fsym field-dvecs))]
                  [row-length (length field-dvecs)]
                  [convert-row
                   (lambda (row)
                     (vector-map! (lambda (value type-reader)
                                    (cond [(sql-null? value) sql-null]
                                          [type-reader (type-reader value)]
                                          [else value]))
                                  row
                                  type-reader-v))])
             (recordset (and headers?
                             (map field-dvec->field-info field-dvecs))
                        (finalize
                         (for/fold ([accum init]) ([row (in-list rows)])
                           (combine accum (convert-row row)))))))]
        [(vector 'command command-string)
         (simple-result `((command . ,command-string)))]))

    (define/private (query1:get-type-readers fsym field-dvecs)
      (map (lambda (dvec)
             (let* ([typeid (field-dvec->typeid dvec)]
                    [type (or (typeid->type typeid)
                              (error fsym "unsupported type: (typeid ~a)" typeid))])
               (or (type->type-reader type)
                   (error fsym "unsupported type: ~a" type))))
           field-dvecs))


    ;; == Prepare

    ;; prepare* : symbol (list-of string) -> (list-of PreparedStatement)
    (define/public (prepare* fsym stmts)
      (call-with-lock fsym
        (lambda ()
          ;; name generation within exchange: synchronized
          (let ([names (map (lambda (_) (generate-name)) stmts)])
            (for ([name (in-list names)]
                  [stmt (in-list stmts)])
              (prepare1:enqueue name stmt))
            (send-message (make-Sync))
            (let ([results
                   (for/list ([name (in-list names)]
                              [stmt (in-list stmts)])
                     (prepare1:collect fsym name stmt))])
              (check-ready-for-query fsym #f)
              results)))))

    ;; prepare1:enqueue : string string -> void
    (define/private (prepare1:enqueue name stmt)
      (buffer-message (make-Parse name stmt null))
      (buffer-message (make-Describe 'statement name)))

    ;; prepare1:collect : stream string string -> PreparedStatement stream
    (define/private (prepare1:collect fsym name stmt)
      (let ([r (recv-message fsym)])
        (match r
          [(struct ParseComplete ())
           (prepare1:describe-params fsym name stmt)]
          [else (prepare1:error fsym r stmt)])))

    (define/private (prepare1:describe-params fsym name stmt)
      (let ([r (recv-message fsym)])
        (match r
          [(struct ParameterDescription (param-types))
           (prepare1:describe-result fsym name stmt param-types)]
          [else (prepare1:error fsym r stmt)])))

    (define/private (prepare1:describe-result fsym name stmt param-types)
      (let ([r (recv-message fsym)])
        (match r
          [(struct RowDescription (field-dvecs))
           (prepare1:finish fsym name stmt param-types (map field-dvec->field-info field-dvecs))]
          [(struct NoData ())
           (prepare1:finish fsym name stmt param-types #f)]
          [else (prepare1:error fsym r stmt)])))

    (define/private (prepare1:error fsym r stmt)
      (error fsym "internal error: unexpected message processing ~s" stmt))

    (define/private (prepare1:finish fsym name stmt param-types result-fields)
      (new prepared-statement%
           (name name)
           (param-infos (map (lambda (t) `((typeid . ,t))) param-types))
           (result-infos result-fields)
           (owner this)))

    ;; check-statement : symbol any -> void
    (define/private (check-statement fsym stmt)
      (unless (or (string? stmt) (statement-binding? stmt))
        (raise-type-error fsym "string or statement-binding" stmt))
      (when (statement-binding? stmt)
        (let ([pst (statement-binding-pst stmt)])
          (send pst check-owner fsym this stmt))))

    ;; name-counter : nat
    (define name-counter 0)

    ;; generate-name : -> string
    (define/private (generate-name)
      (let ([n name-counter])
        (set! name-counter (add1 name-counter))
        (format "λmz_~a_~a" process-id n)))))


;; ========================================

;; ssl-connector-mixin
;; Adds SSL connection support.
(define ssl-connector-mixin
  (mixin (connector<%>) ()
    (super-new)

    ;; attach-to-ports : input-port output-port -> void
    (define/override (attach-to-ports in out [ssl 'no] [ssl-encrypt #f])
      (with-handlers ([(lambda _ #t)
                       (lambda (e)
                         (close-input-port in)
                         (close-output-port out)
                         (raise e))])
        (case ssl
          ((yes optional)
           ;; Try negotiating SSL connection
           (write-message (make-SSLRequest) out)
           (flush-output out)
           (let ([response (peek-byte in)])
             (case (integer->char response)
               ((#\S)
                (void (read-byte in))
                (let-values ([(sin sout)
                              (ports->ssl-ports in out
                                                #:mode 'connect
                                                #:encrypt ssl-encrypt 
                                                #:close-original? #t)])
                  (super attach-to-ports sin sout)))
               ((#\N)
                ;; Backend gracefully declined
                (void (read-byte in))
                (unless (eq? ssl 'optional)
                  (error 'postgresql-connect "backend refused SSL connection"))
                (super attach-to-ports in out))
               ((#\E)
                (let ([r (parse-server-message in)])
                  (raise-backend-error 'postgresql-connect r)))
               (else
                (error 'postgresql-connect
                       "backend returned invalid response to SSL request")))))
          ((no)
           (super attach-to-ports in out)))))))

;; ========================================

;; nosupport : string -> string
(define (nosupport str)
  (string-append "not supported: " str))

;; md5-password : string string bytes -> string
;; Compute the MD5 hash of a password in the form expected by the PostgreSQL 
;; backend.
(define (md5-password user password salt)
  (bytes->string/latin-1
   (md5-password/bytes (string->bytes/latin-1 user)
                       (string->bytes/latin-1 password)
                       salt)))
(define (md5-password/bytes user password salt)
  (let* ([s (md5 (bytes-append password user))]
         [t (md5 (bytes-append s salt))])
    (bytes-append #"md5" t)))

(define (crypt-password password salt)
  (error 'crypt-password "not implemented"))

;; ========================================

;; connection%
(define connection%
  (class (ssl-connector-mixin connection-base%)
    (super-new)))
