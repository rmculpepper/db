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
    (init name)
    (init param-types)
    (init result-count)
    (init owner)

    (define -name name)
    (define -param-types param-types)
    (define -result-count result-count)
    (define -owner (make-weak-box owner))

    (define/public-final (get-name) -name)
    (define/public-final (get-result-count) -result-count)

    (define/public-final (check-owner c)
      (eq? c (weak-box-value -owner)))

    (define/public-final (bind params)
      (unless (list? params)
        (raise-type-error 'bind-prepared-statement "list" params))
      (check-params params -param-types)
      (let* ([owner (weak-box-value -owner)]
             ;; If the owner if #f, we can't use it to call datum->external-representation.
             ;; But that's okay, just put 'invalid as params;
             ;; query methods check ownership before looking at params.
             [params
              (cond [owner
                     (map (lambda (t p)
                            (if (sql-null? p)
                                sql-null
                                (send owner datum->external-representation t p)))
                          -param-types
                          params)]
                    [else
                     'invalid])])
        (make-StatementBinding this params)))

    (define/private (check-params params param-types)
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
    ;; recv-one-message : -> message
    recv-one-message

    ;; recv-messages : -> (listof message)
    recv-messages

    ;; split-messages : (listof message) -> (values message (listof message))
    split-messages

    ;; send-message : message -> void
    send-message

    ;; buffer-message : message -> void
    buffer-message

    ;; flush-message-buffer : -> void
    flush-message-buffer

    ;; new-exchange : -> stream
    new-exchange

    ;; end-exchange : -> void
    end-exchange

    ;; after-connect : -> void
    after-connect

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

;; backend-link<%>
(define backend-link<%>
  (interface ()
    ;; new-exchange : -> void
    new-exchange

    ;; end-exchange : -> void
    end-exchange

    ;; close : -> void
    close

    ;; get-next-message : -> msg
    get-next-message

    ;; encode : msg -> void
    encode

    ;; flush : -> void
    flush

    ;; alive? : -> boolean
    alive?
    ))

;; backend-link%
(define backend-link%
  (class* object% (backend-link<%>)
    (init-field inport
                outport)

    (define wlock (make-semaphore 1))

    (define/public (new-exchange)
      (semaphore-wait wlock))

    (define/public (end-exchange)
      (semaphore-post wlock))

    (define/public (get-next-message)
      (parse-server-message inport))

    (define/public (encode msg)
      (write-message msg outport))

    (define/public (close)
      (close-output-port outport)
      (close-input-port inport))

    (define/public (flush)
      (flush-output outport))

    (define/public (alive?) #t)

    (super-new)))

(define disconnected-backend-link
  (let ()
    (define disconnected-backend-link%
      (class* object% (backend-link<%>)
        (define/public (new-exchange . args)
          (illegal))
        (define/public (end-exchange . args)
          (illegal))
        (define/public (get-next-message)
          (illegal))
        (define/public (close)
          (void))
        (define/public (encode . args)
          (illegal))
        (define/public (flush)
          (illegal))
        (define/public (alive?) #f)
        (define/private (illegal)
          (error 'backend-link "not connected"))
        (super-new)))
    (new disconnected-backend-link%)))


;; ----

;; base%
(define base%
  (class* object% (connection:admin<%> postgres-base<%>)
    ;; protocol : backend-link<%>
    (init-field [protocol disconnected-backend-link]
                [process-id #f]
                [secret-key #f])
    (super-new)

    ;; with-disconnect-on-error
    ;; Specialized to use direct method call rather than 'send'
    (define-syntax with-disconnect-on-error
      (syntax-rules ()
        [(with-disconnect-on-error expr)
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (disconnect #f)
                            (raise e))])
           expr)]))

    ;; Communication Methods

    ;; new-exchange : -> stream
    (define/public (new-exchange)
      (send protocol new-exchange))

    ;; end-exchange : -> void
    (define/public (end-exchange)
      (send protocol end-exchange))

    ;; raw-recv : -> message
    ;; Must be called within exchange.
    (define/private (raw-recv)
      (with-disconnect-on-error
       (let ([r (send protocol get-next-message)])
         (when DEBUG-RESPONSES
           (fprintf (current-error-port) "  << ~s\n" r))
         r)))

    ;; recv-one-message : symbol -> message
    (define/public (recv-one-message behalf)
      (let ([r (raw-recv)])
        (or (auto-handle-message r behalf)
            (recv-one-message behalf))))

    ;; recv-messages : -> (listof message)
    (define/public (recv-messages)
      (let ([r (raw-recv)])
        (if (ReadyForQuery? r)
            (list r)
            (cons r (recv-messages)))))

    ;; split-messages : symbol (listof msg) -> (values msg (listof msg))
    (define/public (split-messages behalf msgs)
      (if (auto-handle-message (car msgs) behalf)
          (values (car msgs) (cdr msgs))
          (split-messages behalf (cdr msgs))))

    ;; auto-handle-message : message behalf -> message/#f
    ;; Returns #f if message handled asynchronously
    (define/private (auto-handle-message r behalf)
      (cond [(ErrorResponse? r)
             (raise-backend-error behalf r)]
            [(or (NoticeResponse? r)
                 (NotificationResponse? r)
                 (ParameterStatus? r))
             (handle-async-message r)
             #f]
            [else r]))

    ;; Asynchronous message hooks

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

    ;; Sending

    ;; send-message : message -> void
    (define/public (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    ;; buffer-message : message -> void
    (define/public (buffer-message msg)
      (when DEBUG-SENT-MESSAGES
        (fprintf (current-error-port) "  >> ~s\n" msg))
      (with-disconnect-on-error
       (send protocol encode msg)))

    ;; flush-message-buffer : -> void
    (define/public (flush-message-buffer)
      (with-disconnect-on-error
       (send protocol flush)))

    ;; Connection management

    ;; disconnect : [boolean] -> (void)
    (define/public disconnect
      (case-lambda
        [() (disconnect* #t)]
        [(politely?) (disconnect* politely?)]))

    (define/public (disconnect* politely?)
      (when (and politely? (send protocol alive?))
        (new-exchange)
        (send-message (make-Terminate))
        (end-exchange))
      (when DEBUG-SENT-MESSAGES
        (fprintf (current-error-port) "  ** Disconnecting\n"))
      (send protocol close)
      (set! protocol disconnected-backend-link))

    ;; connected? : -> boolean
    (define/public (connected?)
      (send protocol alive?))

    ;; System

    (define/public (connection-dbsystem)
      dbsystem)

    ;; Initialization

    (define/public (after-connect)
      (void))
    ))

;; connector-mixin%
(define connector-mixin
  (mixin (base<%>) (connector<%>)
    (init-field [allow-cleartext-password? #f])

    (inherit-field protocol
                   process-id
                   secret-key)
    (inherit recv-one-message
             new-exchange
             end-exchange
             buffer-message
             send-message
             disconnect
             after-connect)
    (super-new)

    ;; with-disconnect-on-error
    ;; Specialized to use direct method call rather than 'send'
    (define-syntax with-disconnect-on-error
      (syntax-rules ()
        [(with-disconnect-on-error . b)
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (disconnect #f)
                            (raise e))])
           . b)]))

    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! protocol
            (new backend-link% (inport in) (outport out))))

    ;; start-connection-protocol : string string string/#f -> void
    (define/public (start-connection-protocol dbname username password)
      (with-disconnect-on-error
       (new-exchange)
       (send-message
        (make-StartupMessage
         (list (cons "user" username)
               (cons "database" dbname)
               (cons "client_encoding" "UTF8")
               (cons "DateStyle" "ISO, MDY"))))
       (expect-auth username password)))

    ;; expect-auth : string/#f -> ConnectionResult
    (define/private (expect-auth username password)
      (let ([r (recv-one-message 'connect)])
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
      (let ([r (recv-one-message 'connect)])
        (match r
          [(struct ReadyForQuery (status))
           (end-exchange)
           (after-connect)]
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

;; primitive-query-mixin
;; Handles the mechanics of connection creations, queries, etc.
;; Provides functionality, not usability. See connection% for friendly 
;; interface.
(define primitive-query-mixin
  (mixin (base<%> primitive-query<%>) (primitive-query/prepare<%>)
    (inherit-field protocol
                   process-id
                   secret-key)
    (inherit recv-messages
             split-messages
             send-message
             buffer-message
             flush-message-buffer
             new-exchange
             end-exchange
             datum->external-representation)
    (super-new)

    ;; name-counter : number
    (define name-counter 0)

    (define-syntax with-final-end-exchange
      (syntax-rules ()
        [(with-final-end-exchange . b)
         (dynamic-wind
          void
          (lambda () . b)
          (lambda () (end-exchange)))]))

    ;; query*/no-conversion : symbol (list-of Statement) Collector
    ;;                     -> (list-of QueryResult)
    ;; The single point of control for the query engine
    (define/override (query*/no-conversion fsym stmts collector)
      (for-each (lambda (stmt) (check-statement fsym stmt)) stmts)
      (new-exchange)
      (let ([mg (with-final-end-exchange
                 (for-each (lambda (stmt) (query1:enqueue stmt)) stmts)
                 (send-message (make-Sync))
                 (recv-messages))])
        (let loop ([stmts stmts] [mg mg])
          (if (null? stmts)
              (begin (check-ready-for-query mg)
                     null)
              (let-values ([(result mg) (query1:collect mg (car stmts) collector)])
                (cons result (loop (cdr stmts) mg)))))))

    ;; check-ready-for-query : (listof msg) -> void
    (define/private (check-ready-for-query mg)
      (let-values ([(r mg) (split-messages 'query* mg)])
        (unless (ReadyForQuery? r)
          (error 'query* "backend sent unexpected message after query results"))))

    ;; check-statement : symbol any -> void
    (define/private (check-statement fsym stmt)
      (unless (or (string? stmt) (StatementBinding? stmt))
        (raise-type-error fsym "string or StatementBinding" stmt))
      (when (StatementBinding? stmt)
        (let ([pst (StatementBinding-pst stmt)])
          (unless (is-a? pst prepared-statement%)
            (raise-type-error 
             fsym
             "StatementBinding containing prepared statement" stmt))
          (unless (send pst check-owner this)
            (raise-mismatch-error 
             fsym
             "prepared statement owned by another connection" stmt)))))

    ;; query1:enqueue : Statement -> void
    (define/private (query1:enqueue stmt)
      (if (string? stmt)
          (begin (buffer-message (make-Parse "" stmt null))
                 (buffer-message (make-Bind "" "" null null null)))
          (let* ([pst (StatementBinding-pst stmt)]
                 [pst-name (send pst get-name)]
                 [params (StatementBinding-params stmt)])
            (buffer-message (make-Bind "" pst-name null params null))))
      (buffer-message (begin-lifted (make-Describe 'portal "")))
      (buffer-message (begin-lifted (make-Execute "" 0)))
      (buffer-message (begin-lifted (make-Close 'portal ""))))

    ;; query1:collect : stream Statement Collector -> QueryResult stream
    (define/private (query1:collect mg stmt collector)
      (if (string? stmt)
          (query1:expect-parse-complete mg collector)
          (query1:expect-bind-complete mg collector)))
    (define/private (query1:expect-parse-complete mg collector)
      (let-values ([(r mg) (split-messages 'query* mg)])
        (match r
          [(struct ParseComplete ())
           (query1:expect-bind-complete mg collector)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:expect-bind-complete mg collector)
      (let-values ([(r mg) (split-messages 'query* mg)])
        (match r
          [(struct BindComplete ())
           (query1:expect-portal-description mg collector)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:expect-portal-description mg collector)
      (let-values ([(r mg) (split-messages 'query* mg)])
        (match r
          [(struct RowDescription (rows))
           (let-values ([(init combine finalize info)
                         (collector (map parse-field-info rows) #f)])
             (query1:data-loop mg init combine finalize info))]
          [(struct NoData ())
           (query1:expect-completion mg)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:data-loop mg init combine finalize info)
      (let-values ([(r mg) (split-messages 'query* mg)])
        (match r
          [(struct DataRow (value))
           (query1:data-loop mg 
                             (apply combine init value)
                             combine
                             finalize
                             info)]
          [(struct CommandComplete (command))
           (query1:finalize mg (make-Recordset info (finalize init)))]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:expect-completion mg)
      (let-values ([(r mg) (split-messages 'query* mg)])
        (match r
          [(struct CommandComplete (command))
           (query1:finalize mg (make-SimpleResult command))]
          [(struct EmptyQueryResponse ())
           (query1:finalize mg (make-SimpleResult #f))]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:finalize mg result)
      (let-values ([(r mg) (split-messages 'query* mg)])
        (match r
          [(struct CloseComplete ())
           (values result mg)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:error-recovery r mg)
      (match r
        [(struct CopyInResponse (format column-formats))
         (raise-user-error 'query*
                           "COPY IN statements not supported")]
        [(struct CopyOutResponse (format column-formats))
         (raise-user-error 'query*
                           "COPY OUT statements not supported")]
        [_ (error 'query "unexpected message")]))

    ;; generate-name : -> string
    (define/private (generate-name)
      (let ([n name-counter])
        (set! name-counter (add1 name-counter))
        (format "λmz_~a_~a" process-id n)))

    ;; prepare-multiple : (list-of string) -> (list-of PreparedStatement)
    (define/public (prepare-multiple stmts)
      (for-each (lambda (stmt)
                  (unless (string? stmt)
                    (raise-type-error 'prepare* "string" stmt)))
                stmts)
      (new-exchange)
      (let* (;; name generation within exchange: synchronized
             [names (map (lambda (_) (generate-name)) stmts)]
             [mg (with-final-end-exchange
                  (for-each (lambda (name stmt) (prepare1:enqueue name stmt))
                            names
                            stmts)
                  (send-message (make-Sync))
                  (recv-messages))])
        (let loop ([names names] [stmts stmts] [mg mg])
          (if (null? stmts)
              (begin (check-ready-for-query mg)
                     null)
              (let-values ([(result mg) (prepare1:collect mg (car names) (car stmts))])
                (cons result (loop (cdr names) (cdr stmts) mg)))))))

    ;; prepare1:enqueue : string string -> void
    (define/private (prepare1:enqueue name stmt)
      (buffer-message (make-Parse name stmt null))
      (buffer-message (make-Describe 'statement name)))

    ;; prepare1:collect : stream string string -> PreparedStatement stream
    (define/private (prepare1:collect mg name stmt)
      (let-values ([(r mg) (split-messages 'prepare* mg)])
        (match r
          [(struct ParseComplete ())
           (prepare1:describe-params mg name stmt)]
          [else (prepare1:error mg r stmt)])))
    (define/private (prepare1:describe-params mg name stmt)
      (let-values ([(r mg) (split-messages 'prepare* mg)])
        (match r
          [(struct ParameterDescription (param-types))
           (prepare1:describe-result mg name stmt param-types)]
          [else (prepare1:error mg r stmt)])))
    (define/private (prepare1:describe-result mg name stmt param-types)
      (let-values ([(r mg) (split-messages 'prepare* mg)])
        (match r
          [(struct RowDescription (field-records))
           (prepare1:finish mg name stmt param-types (length field-records))]
          [(struct NoData ())
           (prepare1:finish mg name stmt param-types #f)]
          [else (prepare1:error mg r stmt)])))
    (define/private (prepare1:error mg r stmt)
      (error 'prepare* "unexpected message processing ~s: ~s" stmt r))
    (define/private (prepare1:finish mg name stmt param-types result-fields)
      (values
       (new prepared-statement%
            (name name)
            (param-types param-types)
            (result-count result-fields)
            (owner this))
       mg))
    ))

;; pure-connection%
(define pure-connection% 
  (class (prepare-query-mixin
          (query-mixin
           (primitive-query-mixin
            (primitive-query-base-mixin
             (connector-mixin
              base%)))))
    (super-new)))

;; connection%
(define connection%
  (class (ssl-connector-mixin pure-connection%)
    (super-new)))
