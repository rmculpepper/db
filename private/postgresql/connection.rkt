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
         "../generic/sql-data.rkt"
         "../generic/prepared.rkt"
         "message.rkt"
         "dbsystem.rkt")
(provide connection%
         password-hash)

;; Debugging
(define DEBUG-RESPONSES #f)
(define DEBUG-SENT-MESSAGES #f)

;; ========================================

(define connection-base%
  (class* object% (connection<%> connector<%>)
    (init-private notice-handler
                  notification-handler
                  allow-cleartext-password?)
    (define inport #f)
    (define outport #f)
    (define process-id #f)
    (define tx-status #f) ;; #f, #t, 'invalid

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

    (define/private (lock who require-connected?)
      (semaphore-wait wlock)
      (when (and require-connected? (not outport))
        (semaphore-post wlock)
        (error/not-connected who)))

    (define/private (unlock)
      (let ([handler-calls (reverse delayed-handler-calls)])
        (set! delayed-handler-calls null)
        (semaphore-post wlock)
        (for-each call-with-continuation-barrier handler-calls)))

    (define/private (call-with-lock who proc
                                   #:require-connected? [require-connected? #t])
      (lock who require-connected?)
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
    (define/private (recv-message fsym)
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
    (define/private (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    ;; buffer-message : message -> void
    (define/private (buffer-message msg)
      (when DEBUG-SENT-MESSAGES
        (fprintf (current-error-port) "  >> ~s\n" msg))
      (with-disconnect-on-error
       (write-message msg outport)))

    ;; flush-message-buffer : -> void
    (define/private (flush-message-buffer)
      (with-disconnect-on-error
       (flush-output outport)))

    ;; check-ready-for-query : symbol -> void
    (define/private (check-ready-for-query fsym or-eof?)
      (let ([r (recv-message fsym)])
        (cond [(ReadyForQuery? r)
               ;; Update transaction status
               (case (ReadyForQuery-transaction-status r)
                 ((idle) (set! tx-status #f))
                 ((transaction) (set! tx-status #t))
                 ((failed) (set! tx-status 'invalid)))]
              [(and or-eof? (eof-object? r)) (void)]
              [else (error/comm fsym)])))

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
                  (uerror fsym
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
    (define/private (disconnect* no-lock-held?)
      (define (go politely?)
        (when DEBUG-SENT-MESSAGES
          (fprintf (current-error-port) "  ** Disconnecting\n"))
        (let ([outport* outport]
              [inport* inport])
          (when outport*
            (when politely?
              (send-message (make-Terminate)))
            (close-output-port outport*)
            (set! outport #f))
          (when inport*
            (close-input-port inport*)
            (set! inport #f))))
      ;; If we don't hold the lock, try to acquire it and disconnect politely.
      ;; Except, if already disconnected, no need to acquire lock.
      (cond [(and no-lock-held? (connected?))
             (call-with-lock 'disconnect (lambda () (go #t))
                             #:require-connected? #f)]
            [else (go #f)]))

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
             (error/need-password 'postgresql-connect))
           (unless allow-cleartext-password?
             (uerror 'postgresql-connect (nosupport "cleartext password")))
           (send-message (make-PasswordMessage password))
           (connect:expect-auth username password)]
          [(struct AuthenticationCryptPassword (salt))
           (uerror 'postgresql-connect (nosupport "crypt()-encrypted password"))]
          [(struct AuthenticationMD5Password (salt))
           (unless password
             (error/need-password 'postgresql-connect))
           (send-message (make-PasswordMessage (md5-password username password salt)))
           (connect:expect-auth username password)]
          [(struct AuthenticationKerberosV5 ())
           (uerror 'postgresql-connect (nosupport "KerberosV5 authentication"))]
          [(struct AuthenticationSCMCredential ())
           (uerror 'postgresql-connect (nosupport "SCM authentication"))]
          ;; ErrorResponse handled by recv-message
          [_ (error/comm 'postgresql-connect "during authentication")])))

    ;; connect:expect-ready-for-query : -> void
    (define/private (connect:expect-ready-for-query)
      (let ([r (recv-message 'postgresql-connect)])
        (match r
          [(struct ReadyForQuery (status))
           (void)]
          [(struct BackendKeyData (pid secret))
           (set! process-id pid)
           (connect:expect-ready-for-query)]
          [_
           (error/comm 'postgresql-connect "after authentication")])))

    ;; ============================================================

    ;; == Query

    ;; query : symbol Statement Collector -> QueryResult
    (define/public (query fsym stmt0 collector)
      (let-values ([(stmt result)
                    (call-with-lock fsym
                      (lambda ()
                        (check-valid-tx-status fsym tx-status)
                        (query1 fsym stmt0)))])
        (statement:after-exec stmt)
        (query1:process-result fsym collector result)))

    (define/private (query1 fsym stmt)
      (let ([stmt (check-statement fsym stmt)])
        (query1:enqueue stmt)
        (send-message (make-Sync))
        (begin0 (values stmt (query1:collect fsym stmt))
          (check-ready-for-query fsym #f))))

    ;; check-statement : symbol statement -> statement-binding
    ;; Always prepare, so we can have type information to choose result formats.
    (define/private (check-statement fsym stmt)
      (cond [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)])
               (send pst check-owner fsym this stmt))
             stmt]
            [(string? stmt)
             (let ([pst (prepare1 fsym stmt #t)])
               (send pst bind fsym null))]))

    ;; query1:enqueue : Statement -> void
    (define/private (query1:enqueue stmt)
      (let* ([pst (statement-binding-pst stmt)]
             [pst-name (send pst get-handle)]
             [params (statement-binding-params stmt)])
        (buffer-message (make-Bind "" pst-name
                                   (map typeid->format (send pst get-param-typeids))
                                   params
                                   (map typeid->format (send pst get-result-typeids)))))
      (buffer-message (make-Describe 'portal ""))
      (buffer-message (make-Execute "" 0))
      (buffer-message (make-Close 'portal "")))

    (define/private (query1:collect fsym stmt)
      (when (string? stmt)
        (match (recv-message fsym)
          [(struct ParseComplete ()) (void)]
          [other-r (query1:error fsym other-r)]))
      (match (recv-message fsym)
        [(struct BindComplete ()) (void)]
        [other-r (query1:error fsym other-r)])
      (match (recv-message fsym)
        [(struct RowDescription (field-dvecs))
         (let* ([rows (query1:data-loop fsym)])
           (query1:expect-close-complete fsym)
           (vector 'recordset field-dvecs rows))]
        [(struct NoData ())
         (let* ([command (query1:expect-completion fsym)])
           (query1:expect-close-complete fsym)
           (vector 'command command))]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:data-loop fsym)
      (match (recv-message fsym)
        [(struct DataRow (row))
         (cons row (query1:data-loop fsym))]
        [(struct CommandComplete (command)) null]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:expect-completion fsym)
      (match (recv-message fsym)
        [(struct CommandComplete (command)) `((command . ,command))]
        [(struct EmptyQueryResponse ()) '()]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:expect-close-complete fsym)
      (match (recv-message fsym)
        [(struct CloseComplete ()) (void)]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:error fsym r)
      (match r
        [(struct CopyInResponse (format column-formats))
         (uerror fsym (nosupport "COPY IN statements"))]
        [(struct CopyOutResponse (format column-formats))
         (uerror fsym (nosupport "COPY OUT statements"))]
        [_ (error/comm fsym)]))

    (define/private (query1:process-result fsym collector result)
      (match result
        [(vector 'recordset field-dvecs rows)
         (let-values ([(init combine finalize headers?)
                       (collector (length field-dvecs) #t)])
           (let* ([type-reader-v
                   (list->vector (query1:get-type-readers fsym field-dvecs))]
                  [row-length (length field-dvecs)]
                  [convert-row
                   (lambda (row)
                     (vector-map! (lambda (value type-reader)
                                    (cond [(sql-null? value) sql-null]
                                          [else (type-reader value)]))
                                  row
                                  type-reader-v))])
             (recordset (and headers?
                             (map field-dvec->field-info field-dvecs))
                        (finalize
                         (for/fold ([accum init]) ([row (in-list rows)])
                           (combine accum (convert-row row)))))))]
        [(vector 'command command)
         (simple-result command)]))

    (define/private (query1:get-type-readers fsym field-dvecs)
      (map (lambda (dvec)
             (let ([typeid (field-dvec->typeid dvec)])
               (typeid->type-reader fsym typeid)))
           field-dvecs))


    ;; == Prepare

    (define/public (prepare fsym stmt close-on-exec?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym tx-status)
          (prepare1 fsym stmt close-on-exec?))))

    (define/private (prepare1 fsym stmt close-on-exec?)
      ;; name generation within exchange: synchronized
      (let ([name (generate-name)])
        (prepare1:enqueue name stmt)
        (send-message (make-Sync))
        (begin0 (prepare1:collect fsym name close-on-exec?)
          (check-ready-for-query fsym #f))))

    (define/private (prepare1:enqueue name stmt)
      (buffer-message (make-Parse name stmt null))
      (buffer-message (make-Describe 'statement name)))

    (define/private (prepare1:collect fsym name close-on-exec?)
      (match (recv-message fsym)
        [(struct ParseComplete ()) (void)]
        [other-r (prepare1:error fsym other-r)])
      (let* ([param-typeids (prepare1:describe-params fsym)]
             [field-dvecs (prepare1:describe-result fsym)])
        (new prepared-statement%
             (handle name)
             (close-on-exec? close-on-exec?)
             (param-typeids param-typeids)
             (result-dvecs field-dvecs)
             (owner this))))

    (define/private (prepare1:describe-params fsym)
      (match (recv-message fsym)
        [(struct ParameterDescription (param-typeids)) param-typeids]
        [other-r (prepare1:error fsym other-r)]))

    (define/private (prepare1:describe-result fsym)
      (match (recv-message fsym)
        [(struct RowDescription (field-dvecs)) field-dvecs]
        [(struct NoData ()) null]
        [other-r (prepare1:error fsym other-r)]))

    (define/private (prepare1:error fsym r)
      (error/comm fsym "during prepare"))

    ;; name-counter : nat
    (define name-counter 0)

    ;; generate-name : -> string
    (define/private (generate-name)
      (let ([n name-counter])
        (set! name-counter (add1 name-counter))
        (format "λmz_~a_~a" process-id n)))

    ;; free-statement : prepared-statement -> void
    (define/public (free-statement pst)
      (call-with-lock 'free-statement
        #:require-connected? #f
        (lambda ()
          (let ([name (send pst get-handle)])
            (when (and name outport) ;; outport = connected?
              (send pst set-handle #f)
              (buffer-message (make-Close 'statement name))
              (buffer-message (make-Sync))
              (let ([r (recv-message 'free-statement)])
                (cond [(CloseComplete? r) (void)]
                      [else (error/comm 'free-statement)])
                (check-ready-for-query 'free-statement #t)))))))

    ;; == Transactions

    (define/public (transaction-status fsym)
      (call-with-lock fsym (lambda () tx-status)))

    (define/public (start-transaction fsym isolation)
      (let ([stmt
             (call-with-lock fsym
               (lambda ()
                 (when tx-status
                   (error/already-in-tx fsym))
                 (let* ([isolation-level (isolation-symbol->string isolation)]
                        ;; 'read-only  => "READ ONLY"
                        ;; 'read-write => "READ WRITE"
                        [stmt
                         (if isolation-level
                             (string-append "BEGIN WORK ISOLATION LEVE " isolation-level)
                             "BEGIN WORK")])
                   (let-values ([(stmt result) (query1 fsym stmt)])
                     stmt))))])
        (statement:after-exec stmt)
        (void)))

    (define/public (end-transaction fsym mode)
      (let ([stmt
             (call-with-lock fsym
               (lambda ()
                 (unless (eq? mode 'rollback)
                   ;; otherwise, COMMIT statement would cause silent ROLLBACK !!!
                   (check-valid-tx-status fsym tx-status))
                 (let ([stmt (case mode
                               ((commit) "COMMIT WORK")
                               ((rollback) "ROLLBACK WORK"))])
                   (let-values ([(stmt result) (query1 fsym stmt)])
                     stmt))))])
        (statement:after-exec stmt)
        (void)))))

;; ========================================

;; ssl-connector-mixin
;; Adds SSL connection support.
(define ssl-connector-mixin
  (mixin (connector<%>) ()
    (super-new)

    ;; attach-to-ports : input-port output-port -> void
    (define/override (attach-to-ports in out [ssl 'no] [ssl-context #f])
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
                                                #:context ssl-context
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
                (error/comm 'postgresql-connect "after SSL request")))))
          ((no)
           (super attach-to-ports in out)))))))

;; ========================================

;; nosupport : string -> string
(define (nosupport str)
  (string-append "not supported: " str))

;; ========================================

;; md5-password : string (U string (list 'hash string)) bytes -> string
;; Compute the MD5 hash of a password in the form expected by the PostgreSQL 
;; backend.
(define (md5-password user password salt)
  (let ([hash
         (cond [(pair? password) (string->bytes/latin-1 (cadr password))]
               [(string? password) (password-hash user password)])])
    (bytes->string/latin-1
     (bytes-append #"md5" (md5 (bytes-append hash salt))))))

;; password-hash : string string -> bytes
(define (password-hash user password)
  (let ([user (string->bytes/latin-1 user)]
        [password (string->bytes/latin-1 password)])
    (md5 (bytes-append password user))))

;; ========================================

;; raise-backend-error : symbol ErrorResponse -> raises exn
(define (raise-backend-error who r)
  (define props (ErrorResponse-properties r))
  (define code (cdr (assq 'code props)))
  (define message (cdr (assq 'message props)))
  (raise-sql-error who code message props))

;; ========================================

;; connection%
(define connection%
  (class (ssl-connector-mixin connection-base%)
    (super-new)))
