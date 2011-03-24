;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; Implementation of connections, which communicate with a backend through
;; structured messages.

#lang racket/base
(require mzlib/etc
         racket/class
         racket/match
         file/md5
         openssl/mzssl
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "../generic/query.rkt"
         "msg.rkt"
         "exceptions.rkt"
         "types.rkt"
         "dbsystem.rkt")
(provide pure-connection%
         connection%)

;; Debugging
(define DEBUG-RESPONSES #f)
(define DEBUG-SENT-MESSAGES #f)

;; prepared-statement%
(define prepared-statement%
  (class* object% (prepared-statement<%>)
    (init-private name
                  param-types
                  result-count)
    (init ([-owner owner]))

    (define owner (make-weak-box -owner))
    (define type-writers
      (send -owner get-type-writers param-types))

    (define/public (get-name) name)
    (define/public (get-result-count) result-count)

    (define/public (check-owner c)
      (eq? c (weak-box-value owner)))

    (define/public (bind params)
      (check-param-count params param-types)
      (let* ([params
              (map (lambda (tw p)
                     (if (sql-null? p)
                         sql-null
                         (tw p)))
                   type-writers
                   params)])
        (statement-binding this params)))

    (define/private (check-param-count params param-types)
      (define len (length params))
      (define tlen (length param-types))
      (when (not (= len tlen))
        (raise-user-error
         'bind-prepared-statement
         "prepared statement requires ~s parameters, given ~s" tlen len)))

    (super-new)))


;; base<%>
;; Manages communication
(define base<%>
  (interface ()
    ;; recv-message : -> message
    recv-message

    ;; send-message : message -> void
    send-message

    ;; buffer-message : message -> void
    buffer-message

    ;; flush-message-buffer : -> void
    flush-message-buffer

    ;; check-ready-for-query : symbol -> void
    check-ready-for-query

    ;; lock : symbol -> void
    lock

    ;; unlock : -> void
    unlock

    ;; call-with-lock : symbol (-> any) -> any
    call-with-lock

    ;; disconnect : -> void
    ;; disconnect : boolean -> void
    disconnect

    ;; connected? : -> boolean
    connected?
    ))

;; postgres-base<%>
;; Hooks for extending postgres message behavior
(define postgres-base<%>
  (interface (base<%>)
    ;; handle-parameter-status : string string -> void
    handle-parameter-status

    ;; handle-notice : string string string (listof (cons string string)) => void
    handle-notice

    ;; handle-notification : string -> void
    handle-notification))

;; postgres-connector<%>
;; Hooks for extending postgres connection setup
(define postgres-connector<%>
  (interface (connector<%>)
    ;; compute-cleartext-password : string/#f -> bytes
    compute-cleartext-password

    ;; compute-crypt-password : string/#f bytes -> bytes
    compute-crypt-password

    ;; compute-md5-password : string string bytes -> bytes
    compute-md5-password

    ;; handle-kerberos5-authentication : -> void
    handle-kerberos5-authentication

    ;; handle-scm-credential-authentication : -> void
    handle-scm-credential-authentication))

;; ----

;; base%
(define base%
  (class* object% (connection:admin<%> postgres-base<%>)
    (init-field [inport #f]
                [outport #f]
                [process-id #f]
                [secret-key #f])
    (define wlock (make-semaphore 1))
    (super-new)

    ;; with-disconnect-on-error
    ;; Specialized to use direct method call rather than 'send'
    (define-syntax-rule (with-disconnect-on-error expr)
      (with-handlers ([exn:fail? (lambda (e) (disconnect #f) (raise e))])
        expr))

    ;; == Communication locking

    (define/public (lock who)
      (semaphore-wait wlock)
      (unless outport
        (semaphore-post wlock)
        (error who "not connected")))

    (define/public (unlock)
      (semaphore-post wlock))

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
    (define/public (recv-message behalf)
      (let ([r (raw-recv)])
        (cond [(ErrorResponse? r)
               (check-ready-for-query behalf) ;; FIXME: eat msgs until ReadyForQuery?
               (raise-backend-error behalf r)]
              [(or (NoticeResponse? r)
                   (NotificationResponse? r)
                   (ParameterStatus? r))
               (handle-async-message r)
               (recv-message behalf)]
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
    (define/public (check-ready-for-query fsym)
      (let ([r (recv-message fsym)])
        (unless (ReadyForQuery? r)
          (error fsym "internal error: backend sent unexpected message: ~e" r))))

    ;; == Asynchronous message hooks

    ;; handle-async-message : message -> void
    (define/private (handle-async-message msg)
      (match msg
        [(struct NoticeResponse (properties))
         (handle-notice properties)]
        [(struct NotificationResponse (pid condition info))
         (handle-notification condition info)]
        [(struct ParameterStatus (name value))
         (handle-parameter-status name value)]))

    ;; handle-parameter-status : string string -> void
    (define/public (handle-parameter-status name value)
      (when (equal? name "client_encoding")
        (unless (equal? value "UTF8")
          (disconnect* #f)
          (error 'connection
                 "client encoding must be UTF8, changed to: ~e"
                 value)))
      (void))

    ;; handle-notice : (listof (cons symbol string)) -> void
    (define/public (handle-notice properties)
      (fprintf (current-error-port)
               "notice: ~a (SQL code ~a)\n" 
               (cdr (assq 'message properties))
               (cdr (assq 'code properties))))

    ;; handle-notification :  string string -> void
    (define/public (handle-notification condition info)
      (fprintf (current-error-port)
               "notification ~a: ~a\n"
               condition
               info))

    ;; == Connection management

    ;; disconnect : [boolean] -> (void)
    (define/public disconnect
      (case-lambda
        [() (disconnect* #t)]
        [(politely?) (disconnect* politely?)]))

    ;; disconnect* : boolean -> void
    ;; If politely? = #t, lock is not already held.
    ;; If politely? = #f, lock is already held.
    (define/public (disconnect* politely?)
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
        (unlock)))

    ;; connected? : -> boolean
    (define/public (connected?)
      (and outport #t))

    ;; == System

    (define/public (get-dbsystem)
      dbsystem)
    ))

;; connector-mixin%
(define connector-mixin
  (mixin (base<%>) (connector<%>)
    (init-field [allow-cleartext-password? #f])

    (inherit-field inport
                   outport
                   process-id
                   secret-key)
    (inherit recv-message
             lock
             unlock
             call-with-lock
             buffer-message
             send-message
             disconnect)
    (super-new)

    ;; with-disconnect-on-error
    ;; Specialized to use direct method call rather than 'send'
    (define-syntax-rule (with-disconnect-on-error . body)
      (with-handlers ([exn:fail? (lambda (e) (disconnect #f) (raise e))])
        . body))

    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! inport in)
      (set! outport out))

    ;; start-connection-protocol : string string string/#f -> void
    (define/public (start-connection-protocol dbname username password)
      (with-disconnect-on-error
       (call-with-lock 'connect
        (lambda ()
          (send-message
           (make-StartupMessage
            (list (cons "user" username)
                  (cons "database" dbname)
                  (cons "client_encoding" "UTF8")
                  (cons "DateStyle" "ISO, MDY"))))
          (expect-auth username password)))))

    ;; expect-auth : string/#f -> ConnectionResult
    (define/private (expect-auth username password)
      (let ([r (recv-message 'connect)])
        (match r
          [(struct AuthenticationOk ())
           (expect-ready-for-query)]
          [(struct AuthenticationCleartextPassword ())
           (handle-cleartext-password-authentication password)
           (expect-auth username password)]
          [(struct AuthenticationCryptPassword (salt))
           (handle-crypt-password-authentication password salt)
           (expect-auth username password)]
          [(struct AuthenticationMD5Password (salt))
           (handle-md5-password-authentication username password salt)
           (expect-auth username password)]
          [(struct AuthenticationKerberosV5 ())
           (handle-kerberos5-authentication)
           (expect-auth username password)]
          [(struct AuthenticationSCMCredential ())
           (handle-scm-credential-authentication)
           (expect-auth username password)]
          [_
           (error 'connect
                  "authentication failed (backend sent unexpected message)")])))

    ;; expect-ready-for-query : -> void
    (define/private (expect-ready-for-query)
      (let ([r (recv-message 'connect)])
        (match r
          [(struct ReadyForQuery (status))
           (void)]
          [(struct BackendKeyData (pid secret))
           (set! process-id pid)
           (set! secret-key secret)
           (expect-ready-for-query)]
          [_
           (error 'connect
                  (string-append "connection failed after authentication "
                                 "(backend sent unexpected message: ~e)")
                  r)])))

    ;; Authentication hooks
    ;; The authentication hooks serve two purposes:
    ;;   - to handle unsupported mechanisms (eg kerberos)
    ;;   - to prevent undesirable authentication methods
    ;;     (such as sending passwords in cleartext)
    ;; An authentication hook should take any necessary action (eg send one or more
    ;; messages to the protocol) and then return to continue the authentication 
    ;; process, or raise an error to abort the connection.

    ;; handle-cleartext-password-authentication : string -> void
    (define/private (handle-cleartext-password-authentication password)
      (unless (string? password)
        (raise-user-error 'connect "password needed but not supplied"))
      (send-message (make-PasswordMessage (compute-cleartext-password password))))

    ;; compute-cleartext-password : string -> string
    (define/public (compute-cleartext-password password)
      (unless allow-cleartext-password?
        (raise-user-error 'connect (nosupport "cleartext password")))
      password)

    ;; handle-crypt-password-authentication : string bytes -> void
    (define/private (handle-crypt-password-authentication password salt)
      (send-message (make-PasswordMessage (compute-crypt-password password salt))))

    ;; compute-crypt-password : string bytes -> void
    (define/public (compute-crypt-password password salt)
      (raise-user-error 'connect (nosupport "crypt()-encrypted password")))

    ;; handle-md5-password-authentication : string string bytes -> void
    (define/private (handle-md5-password-authentication user password salt)
      (send-message (make-PasswordMessage (compute-md5-password user password salt))))

    ;; compute-md5-password : strin string bytes -> bytes
    (define/public (compute-md5-password user password salt)
      (unless (string? password)
        (raise-user-error 'connect "password needed but not supplied"))
      (md5password user password salt))

    ;; handle-kerberos5-authentication : -> void
    (define/public (handle-kerberos5-authentication)
      (raise-user-error 'connect (nosupport "KerberosV5 authentication")))

    ;; handle-scm-credential-authentication : -> void
    (define/public (handle-scm-credential-authentication)
      (raise-user-error 'connect (nosupport "SCM authentication")))

    ))

;; ssl-connector-mixin
;; Adds SSL connection support.
(define ssl-connector-mixin
  (mixin (connector<%> base<%>) (ssl-connector<%>)
    (field [ssl 'no]
           [ssl-encrypt 'sslv2-or-v3])
    (super-new)

    ;; set-ssl-options : YesNoOptional/#f SSLMode/#f -> void
    (define/public (set-ssl-options -ssl -ssl-encrypt)
      (unless (memq -ssl '(yes no optional))
        (raise-user-error 'set-ssl-options
                          "bad ssl option: expected 'yes, 'no, or 'optional, got: ~e"
                          -ssl))
      (when -ssl (set! ssl -ssl))
      (when -ssl-encrypt (set! ssl-encrypt -ssl-encrypt)))

    ;; attach-to-ports : input-port output-port -> void
    (define/override (attach-to-ports in out)
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
                  (raise-user-error 'connect "backend does not support SSL"))
                (super attach-to-ports in out))
               ((#\E)
                (let ([r (parse-server-message in)])
                  (raise-backend-error 'connect r)))
               (else
                (error 'connect "backend returned invalid response to SSL request")))))
          ((no)
           (super attach-to-ports in out)))))))

;; nosupport : string -> string
(define (nosupport str)
  (string-append "not supported: " str))

;; md5password : string string bytes -> string
;; Compute the MD5 hash of a password in the form expected by the PostgreSQL 
;; backend.
(define (md5password user password salt)
  (bytes->string/latin-1
   (md5password/bytes (string->bytes/latin-1 user)
                      (string->bytes/latin-1 password)
                      salt)))
(define (md5password/bytes user password salt)
  (let* [(s (md5 (bytes-append password user)))
         (t (md5 (bytes-append s salt)))]
    (bytes-append #"md5" t)))

;; ============================================================

;; query-mixin
;; Handles the mechanics of connection creations, queries, etc.
;; Provides functionality, not usability. See connection% for friendly 
;; interface.
(define query-mixin
  (mixin (base<%> primitive-query<%>) ()
    (inherit-field process-id
                   secret-key)
    (inherit recv-message
             send-message
             buffer-message
             flush-message-buffer
             check-ready-for-query
             lock
             unlock
             call-with-lock)
    (super-new)

    ;; name-counter : number
    (define name-counter 0)

    (define/private (call-with-unlock proc)
      (with-handlers ([values (lambda (e) (unlock) (raise e))])
        (begin0 (proc)
          (unlock))))

    ;; query*/no-conversion : symbol (list-of Statement) Collector
    ;;                     -> (list-of QueryResult)
    ;; The single point of control for the query engine
    (define/override (query*/no-conversion fsym stmts collector)
      (for ([stmt (in-list stmts)])
        (check-statement fsym stmt))
      (let ([thunks
             (call-with-lock fsym
               (lambda ()
                 (for ([stmt (in-list stmts)])
                   (query1:enqueue stmt))
                 (send-message (make-Sync))
                 (let ([thunks
                        (for/list ([stmt (in-list stmts)])
                          (query1:collect fsym stmt collector))])
                   (check-ready-for-query fsym)
                   thunks)))])
        (map (lambda (p) (p)) thunks)))

    ;; query1:enqueue : Statement -> void
    (define/private (query1:enqueue stmt)
      (if (string? stmt)
          (begin (buffer-message (make-Parse "" stmt null))
                 (buffer-message (make-Bind "" "" null null null)))
          (let* ([pst (statement-binding-pst stmt)]
                 [pst-name (send pst get-name)]
                 [params (statement-binding-params stmt)])
            (buffer-message (make-Bind "" pst-name null params null))))
      (buffer-message (begin-lifted (make-Describe 'portal "")))
      (buffer-message (begin-lifted (make-Execute "" 0)))
      (buffer-message (begin-lifted (make-Close 'portal ""))))

    ;; query1:collect : symbol Statement Collector -> QueryResult stream
    (define/private (query1:collect fsym stmt collector)
      (if (string? stmt)
          (query1:expect-parse-complete fsym collector)
          (query1:expect-bind-complete fsym collector)))
    (define/private (query1:expect-parse-complete fsym collector)
      (let ([r (recv-message fsym)])
        (match r
          [(struct ParseComplete ())
           (query1:expect-bind-complete fsym collector)]
          [_ (query1:error-recovery fsym r)])))
    (define/private (query1:expect-bind-complete fsym collector)
      (let ([r (recv-message fsym)])
        (match r
          [(struct BindComplete ())
           (query1:expect-portal-description fsym collector)]
          [_ (query1:error-recovery fsym r)])))
    (define/private (query1:expect-portal-description fsym collector)
      (let ([r (recv-message fsym)])
        (match r
          [(struct RowDescription (rows))
           (query1:data-loop fsym collector (map parse-field-info rows) null)]
          [(struct NoData ())
           (query1:expect-completion fsym)]
          [_ (query1:error-recovery fsym r)])))
    (define/private (query1:data-loop fsym collector field-infos rows)
      (let ([r (recv-message fsym)])
        (match r
          [(struct DataRow (value))
           (query1:data-loop fsym collector field-infos (cons value rows))]
          [(struct CommandComplete (command))
           (query1:finalize fsym
                            (lambda ()
                              (let-values ([(init combine finalize info)
                                            (collector field-infos #f)])
                                (recordset info
                                           (finalize
                                            (for/fold ([accum init])
                                                ([value (in-list (reverse rows))])
                                              (combine accum (list->vector value))))))))]
          [_ (query1:error-recovery fsym r)])))
    (define/private (query1:expect-completion fsym)
      (let ([r (recv-message fsym)])
        (match r
          [(struct CommandComplete (command))
           (query1:finalize fsym
                            (lambda ()
                              (simple-result
                               `((command . ,(string->command command))))))]
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
         (raise-user-error fsym "COPY IN statements not supported")]
        [(struct CopyOutResponse (format column-formats))
         (raise-user-error fsym "COPY OUT statements not supported")]
        [_ (error fsym "internal error: unexpected message")]))

    ;; prepare-multiple : (list-of string) -> (list-of PreparedStatement)
    (define/public (prepare-multiple stmts)
      (for ([stmt (in-list stmts)])
        (unless (string? stmt)
          (raise-type-error 'prepare* "string" stmt)))
      (call-with-lock 'prepare-multiple
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
                     (prepare1:collect 'prepare-multiple name stmt))])
              (check-ready-for-query 'prepare-multiple)
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
          [(struct RowDescription (field-records))
           (prepare1:finish fsym name stmt param-types (length field-records))]
          [(struct NoData ())
           (prepare1:finish fsym name stmt param-types #f)]
          [else (prepare1:error fsym r stmt)])))
    (define/private (prepare1:error fsym r stmt)
      (error fsym "internal error: unexpected message processing ~s: ~s" stmt r))
    (define/private (prepare1:finish fsym name stmt param-types result-fields)
      (new prepared-statement%
           (name name)
           (param-types param-types)
           (result-count result-fields)
           (owner this)))

    ;; check-statement : symbol any -> void
    (define/private (check-statement fsym stmt)
      (unless (or (string? stmt) (statement-binding? stmt))
        (raise-type-error fsym "string or statement-binding" stmt))
      (when (statement-binding? stmt)
        (let ([pst (statement-binding-pst stmt)])
          (unless (and (is-a? pst prepared-statement%)
                       (send pst check-owner this))
            (raise-mismatch-error 
             fsym
             "prepared statement owned by another connection" stmt)))))

    ;; generate-name : -> string
    (define/private (generate-name)
      (let ([n name-counter])
        (set! name-counter (add1 name-counter))
        (format "λmz_~a_~a" process-id n)))))

;; pure-connection%
(define pure-connection% 
  (class* (query-mixin
           (primitive-query-mixin
            (connector-mixin
             base%)))
      (connection<%>)
    (super-new)))

;; connection%
(define connection%
  (class (ssl-connector-mixin pure-connection%)
    (super-new)))
