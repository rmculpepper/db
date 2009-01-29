;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; Implementation of connections, which communicate with a backend through
;; structured messages.

#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/match
         file/md5
         openssl/mzssl
         "../generic/stream.ss"
         "../generic/interfaces.ss"
         "../generic/sql-data.ss"
         "../generic/query.ss"
         "msg.ss"
         "exceptions.ss"
         "types.ss")
(provide pure-connection%
         connection%)

;; Debugging
(define DEBUG-RESPONSES #f)
(define DEBUG-SENT-MESSAGES #f)

;; A PreparedStatement is:
;;   (make-PostgresPreparedStatement number
;;                                   string
;;                                   (list-of number)
;;                                   (weak-box connection<%>))
(define-struct (PostgresPreparedStatement PreparedStatement)
  (name param-types wcx))

;; base<%>
;; Manages communication
(define base<%>
  (interface ()
    ;; recv : stream -> message stream
    recv
    
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

;; backend-link%
(define backend-link%
  (class* object% (backend-link<%>/sub)
    (init-field inport
                outport)
    
    (define stream #f)
    (define wlock (make-semaphore 1))
    (define rlock (make-semaphore 1))
    
    (define/public (new-exchange . args)
      (semaphore-wait wlock)
      (stream:force-to-end stream)
      (let ([new-mg
             (stream:new (lambda ()
                           (semaphore-wait rlock))
                         (lambda ()
                           (semaphore-post rlock))
                         (mk-get-next-message args)
                         (mk-end-message? args))])
        (set! stream new-mg)
        new-mg))
    
    (define/public (mk-get-next-message args)
      (error 'server-link% "must override mk-get-next-message"))
    
    (define/public (mk-end-message? args)
      (error 'server-link% "must override mk-end-message?"))
    
    (define/public (end-exchange)
      (semaphore-post wlock))
    
    (define/public (close)
      (close-output-port outport)
      (close-input-port inport))
    
    (define/public (encode msg)
      (error 'server-link% "must override encode"))
    
    (define/public (flush)
      (flush-output outport))
    
    (define/public (alive?) #t)
    
    (super-new)))

(define disconnected-backend-link%
  (class* object% (backend-link<%>)
    (define/public (new-exchange . args)
      (illegal))
    (define/public (end-exchange . args)
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

(define disconnected-backend-link
  (new disconnected-backend-link%))

;; base%
(define base%
  (class* object% (base<%>)
    ;; protocol : protocol3
    (init-field [protocol disconnected-backend-link])
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
    
    ;; recv : symbol/#f stream -> message stream
    ;; Automatically handles asynchronous messages
    (define/public (recv behalf mg)
      (let-values ([(r mg)
                    (with-disconnect-on-error
                        (stream:current+next mg))])
        (when DEBUG-RESPONSES
          (fprintf (current-error-port) "  << ~s\n" r))
        (values r mg)))
    
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
    
    ;; send-message : message -> void
    (define/public (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))
    
    ;; Connection management
    
    ;; disconnect : [boolean] -> (void)
    (define/public disconnect
      (case-lambda
        [() (disconnect* #t)]
        [(politely?) (disconnect* politely?)]))
    
    (define/public (disconnect* politely?)
      (when DEBUG-SENT-MESSAGES
        (fprintf (current-error-port) "  ** Disconnecting\n"))
      (send protocol close)
      (set! protocol disconnected-backend-link))
    
    ;; connected? : -> boolean
    (define/public (connected?)
      (send protocol alive?))
    
    ;; Initialization
    
    (define/public (after-connect)
      (void))
    ))

;; postgres-backend-link%
(define postgres-backend-link%
  (class backend-link%
    (inherit-field inport
                   outport)
    
    (define/override (mk-get-next-message _)
      (lambda () (parse-server-message inport)))
    
    (define/override (mk-end-message? _)
      ReadyForQuery?)
    
    (define/override (encode msg)
      (write-message msg outport))
    
    (super-new)))

;; postgres-base%
(define postgres-base%
  (class base%
    (inherit-field protocol)
    (init-field [process-id #f]
                [secret-key #f])
    (inherit new-exchange
             end-exchange
             send-message)
    
    (super-new)
    
    (define/override (recv behalf mg)
      (let-values ([(r mg) (super recv behalf mg)])
        (match r
          [(? ErrorResponse?)
           (raise-backend-error behalf r)]
          [(struct NoticeResponse (properties))
           (handle-notice properties)
           (recv behalf mg)]
          [(struct NotificationResponse (process-id condition info))
           (handle-notification process-id condition info)
           (recv behalf mg)]
          [(struct ParameterStatus (name value))
           (handle-parameter-status name value)
           (recv behalf mg)]
          [else
           (values r mg)])))
    
    (define/override (disconnect* politely?)
      (when (and politely? (send protocol alive?))
        (let ([_ (new-exchange)])
          (send-message (make-Terminate))
          (end-exchange)))
      (super disconnect* politely?))
    
    ;; Asynchronous message hooks
    
    ;; handle-parameter-status : string string -> void
    (define/public (handle-parameter-status name value)
      (when (equal? name "client_encoding")
        (unless (equal? value "UTF8")
          (disconnect* #t)
          (error 'connection "client encoding must be UTF8, changed to: ~e" value)))
      (void))
    
    ;; handle-notice : (listof (cons symbol string)) -> void
    (define/public (handle-notice properties)
      (fprintf (current-error-port)
               "notice: ~a (SQL code ~a)\n" 
               (cdr (assq 'message properties))
               (cdr (assq 'code properties))))
    
    ;; handle-notification : number string string -> void
    (define/public (handle-notification process-id condition info)
      (fprintf (current-error-port)
               "notification ~a: ~a\n"
               condition
               info))
    ))

;; connector-mixin%
(define connector-mixin
  (mixin (base<%>) (connector<%>)
    (init-field [allow-cleartext-password? #f])
    
    (inherit-field protocol
                   process-id
                   secret-key)
    (inherit recv
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
        [(with-disconnect-on-error expr)
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (disconnect #f)
                            (raise e))])
           expr)]))
    
    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! protocol
            (new postgres-backend-link% (inport in) (outport out))))
    
    ;; start-connection-protocol : string string string/#f -> void
    (define/public (start-connection-protocol dbname username password)
      (with-disconnect-on-error
          (let [(mg (new-exchange))]
            (send-message
             (make-StartupMessage
              (list (cons "user" username)
                    (cons "database" dbname)
                    (cons "client_encoding" "UTF8")
                    (cons "DateStyle" "ISO, MDY"))))
            (expect-auth mg username password))))
    
    ;; expect-auth : stream string/#f -> ConnectionResult
    (define/private (expect-auth mg username password)
      (let-values [((r mg) (recv 'connect mg))]
        (match r
          [(struct AuthenticationOk ())
           (expect-ready-for-query mg)]
          [(struct AuthenticationCleartextPassword ())
           (handle-cleartext-password-authentication password)
           (expect-auth mg username password)]
          [(struct AuthenticationCryptPassword (salt))
           (handle-crypt-password-authentication password salt)
           (expect-auth mg username password)]
          [(struct AuthenticationMD5Password (salt))
           (handle-md5-password-authentication username password salt)
           (expect-auth mg username password)]
          [(struct AuthenticationKerberosV5 ())
           (handle-kerberos5-authentication)
           (expect-auth mg username password)]
          [(struct AuthenticationSCMCredential ())
           (handle-scm-credential-authentication)
           (expect-auth mg username password)]
          [_
           (error 'connect
                  "authentication failed (backend sent unexpected message)")])))
    
    ;; expect-ready-for-query : stream -> void
    (define/private (expect-ready-for-query mg)
      (let-values [((r mg) (recv 'connect mg))]
        (match r
          [(struct ReadyForQuery (status))
           (end-exchange)
           (after-connect)]
          [(struct BackendKeyData (pid secret))
           (set! process-id pid)
           (set! secret-key secret)
           (expect-ready-for-query mg)]
          [_
           (error 'connect
                  (string-append "connection failed after authentication "
                                 "(backend sent unexpected message)"))])))
    
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
  (string-append "spgsql does not support " str))

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
  (mixin (base<%>) (primitive-query<%> primitive-query/prepare<%>)
    (inherit-field protocol
                   process-id
                   secret-key)
    (inherit recv
             send-message
             buffer-message
             flush-message-buffer
             new-exchange
             end-exchange)
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
    
    ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
    ;; The single point of control for the query engine
    (define/public (query* fsym stmts collector)
      (for-each (lambda (stmt) (check-statement fsym stmt)) stmts)
      (let ([mg (new-exchange)])
        (with-final-end-exchange
            (for-each (lambda (stmt) (query1:enqueue stmt)) stmts)
          (send-message (make-Sync)))
        (let loop ([stmts stmts] [mg mg])
          (if (null? stmts)
              (begin (check-ready-for-query mg)
                     null)
              (let-values ([(result mg) (query1:collect mg (car stmts) collector)])
                (cons result (loop (cdr stmts) mg)))))))
    
    ;; check-ready-for-query : stream -> void
    (define/private (check-ready-for-query mg)
      (let-values ([(r mg) (recv 'query* mg)])
        (unless (ReadyForQuery? r)
          (error 'query* "backend sent unexpected message after query results"))))
    
    ;; check-statement : symbol any -> void
    (define/private (check-statement fsym stmt)
      (unless (or (string? stmt) (StatementBinding? stmt))
        (raise-type-error fsym "string or StatementBinding" stmt))
      (when (StatementBinding? stmt)
        (let ([pst (StatementBinding-pst stmt)])
          (unless (PostgresPreparedStatement? pst)
            (raise-type-error 
             fsym
             "StatementBinding containing prepared statement" stmt))
          (unless (eq? this (weak-box-value (PostgresPreparedStatement-wcx pst)))
            (raise-mismatch-error 
             fsym
             "prepared statement owned by another connection" stmt)))))
    
    ;; query1:enqueue : Statement -> void
    (define/private (query1:enqueue stmt)
      (if (string? stmt)
          (begin (buffer-message (make-Parse "" stmt null))
                 (buffer-message (make-Bind "" "" null null null)))
          (let* ([pst (StatementBinding-pst stmt)]
                 [pst-name (PostgresPreparedStatement-name pst)]
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
      (let-values ([(r mg) (recv 'query* mg)])
        (match r
          [(struct ParseComplete ())
           (query1:expect-bind-complete mg collector)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:expect-bind-complete mg collector)
      (let-values ([(r mg) (recv 'query* mg)])
        (match r
          [(struct BindComplete ())
           (query1:expect-portal-description mg collector)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:expect-portal-description mg collector)
      (let-values ([(r mg) (recv 'query* mg)])
        (match r
          [(struct RowDescription (rows))
           (let-values ([(init combine finalize info)
                         (collector (map parse-field-info rows) #f)])
             (query1:data-loop mg init combine finalize info))]
          [(struct NoData ())
           (query1:expect-completion mg)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:data-loop mg init combine finalize info)
      (let-values ([(r mg) (recv 'query* mg)])
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
      (let-values ([(r mg) (recv 'query* mg)])
        (match r
          [(struct CommandComplete (command))
           (query1:finalize mg (make-SimpleResult command))]
          [(struct EmptyQueryResponse ())
           (query1:finalize mg (make-SimpleResult #f))]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:finalize mg result)
      (let-values ([(r mg) (recv 'query* mg)])
        (match r
          [(struct CloseComplete ())
           (values result mg)]
          [_ (query1:error-recovery r mg)])))
    (define/private (query1:error-recovery r mg)
      (match r
        [(struct CopyInResponse (format column-formats))
         (raise-user-error 'query*
                           "COPY IN statements not allowed in this query mode")]
        [(struct CopyOutResponse (format column-formats))
         (raise-user-error 'query*
                           "COPY OUT statements not allowed in this query mode")]
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
      (let* ([mg (new-exchange)]
             ;; name generation within exchange: synchronized
             [names (map (lambda (_) (generate-name)) stmts)])
        (with-final-end-exchange
            (for-each (lambda (name stmt) (prepare1:enqueue name stmt))
                      names
                      stmts)
          (send-message (make-Sync)))
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
      (let-values ([(r mg) (recv 'prepare* mg)])
        (match r
          [(struct ParseComplete ())
           (prepare1:describe-params mg name stmt)]
          [else (prepare1:error mg r stmt)])))
    (define/private (prepare1:describe-params mg name stmt)
      (let-values ([(r mg) (recv 'prepare* mg)])
        (match r
          [(struct ParameterDescription (param-types))
           (prepare1:describe-result mg name stmt param-types)]
          [else (prepare1:error mg r stmt)])))
    (define/private (prepare1:describe-result mg name stmt param-types)
      (let-values ([(r mg) (recv 'prepare* mg)])
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
       (make-PostgresPreparedStatement result-fields name param-types (make-weak-box this))
       mg))
    
    ;; bind-prepared-statement : PreparedStatement (list-of value) -> StatementBinding
    (define/public (bind-prepared-statement pst params)
      (unless (PostgresPreparedStatement? pst)
        (raise-type-error 'bind-prepared-statement "prepared statement" pst))
      (unless (eq? this (weak-box-value (PostgresPreparedStatement-wcx pst)))
        (raise-mismatch-error 'bind-prepared-statement
                              "prepared statement is owned by another connection"
                              pst))
      (unless (list? params)
        (raise-type-error 'bind-prepared-statement "list" params))
      (match pst
        [(struct PostgresPreparedStatement (result? pst-name param-types pst-wcx))
         (check-params params param-types)
         (let ([params
                (map (lambda (p t)
                       (if (sql-null? p)
                           sql-null
                           (datum->external-representation t p)))
                     params 
                     param-types)])
           (make-StatementBinding pst params))]))
    
    (define/private (check-params params param-types)
      (define len (length params))
      (define tlen (length param-types))
      (when (not (= len tlen))
        (raise-user-error
         'bind-prepared-statement
         "prepared statement requires ~s parameters, given ~s" tlen len)))
    
    ;; datum->external-representation : number datum -> string
    (define/public (datum->external-representation typeoid datum)
      (unless (string? datum)
        (raise-user-error 'datum->external-representation
                          "cannot convert datum: ~s" datum))
      datum)
    ))

;; conversion-mixin
;; Adds automatic conversions from SQL external representations to Scheme data
(define postgres-conversion-mixin
  (mixin (primitive-query/conversion<%>) ()
    (inherit query*/no-conversion)
    
    ;; typeid->type : number -> symbol
    (define/override (typeid->type typeid)
      (std:typeoid->type typeid))
    
    ;; get-type-reader : symbol -> (string -> datum)
    (define/override (get-type-reader type)
      (or (type->type-reader type)
          (super get-type-reader type)))
    
    ;; get-type-writer : symbol -> (datum -> string) or #f
    (define/override (get-type-writer type)
      (or (type->type-writer type)
          (super get-type-writer type)))
    
    (super-new)))

;; db-based-conversion-mixin
;; This mixin includes code to fetch the typeoid mapping from the
;; database when the connection is made. Use this mixin if the
;; typeoid assignment for the supported basic types changes.
;; I expect the typeoid assignment to be pretty stable, though.

(define TYPE-QUERY 
  "select oid, typname from pg_type where typtype = 'b' and typelem = 0")

(define db-based-conversion-mixin
  (mixin (base<%> primitive-query<%> primitive-query/conversion<%>) ()
    (inherit query*/no-conversion)
    (field [typeoid-mapping (lambda (s) #f)])
    
    ;; typeid->type : num -> symbol
    (define/override (typeid->type typeid)
      (hash-ref typeoid-mapping typeid #f))
    
    ;; create-typeoid-mapping : -> void
    (define/private (create-typeoid-mappings)
      (let ([ht (make-hasheq)]
            [qrs (query*/no-conversion 'create-typeoid-mapping
                                       (list TYPE-QUERY)
                                       vectorlist-collector)])
        (for-each
         (lambda (v)
           (let ([oid (string->number (vector-ref v 0))]
                 [type (string->symbol (vector-ref v 1))])
             (hash-set! ht oid type)))
         (Recordset-data (car qrs)))
        (set! typeoid-mapping ht)))
    
    ;; after-connect : -> void
    (define/override (after-connect)
      (super after-connect)
      (create-typeoid-mappings))
    
    (super-new)))

;; pure-connection%
(define pure-connection% 
  (class (prepare-query-mixin
          (query-mixin
           (postgres-conversion-mixin
            (conversion-mixin
             (primitive-query-mixin
              (connector-mixin
               postgres-base%))))))
    (super-new)))

;; connection%
(define connection%
  (class (ssl-connector-mixin pure-connection%)
    (super-new)))
