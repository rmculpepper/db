;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base)
         racket/match
         "../generic/sql-data.rkt"
         "../generic/io.rkt")
(provide (all-defined-out))

(define-syntax-rule (with-length-in p c . body)
  ;; header char read & discarded elsewhere
  (let ([len (io:read p #:int32)])
    (let ([p (subport p (- len 4))])
      . body)))

(define-syntax-rule (with-length-out p c . body)
  (let ([bs (let ([p (open-output-bytes)])
              (let () (begin . body) (void))
              (get-output-bytes p))])
    (when c (io:write p #:byte/char c))
    (io:write-int32 p (+ 4 (bytes-length bs)))
    (write-bytes bs p)))

;; ========================================
;; The strange structures

(define-struct AuthenticationOk ())
(define-struct AuthenticationKerberosV5 ())
(define-struct AuthenticationCleartextPassword ())
(define-struct AuthenticationCryptPassword (salt))
(define-struct AuthenticationMD5Password (salt))
(define-struct AuthenticationSCMCredential ())
(define (parse:Authentication p)
  (with-length-in p #\R
    (let* ([tag (io:read-int32 p)])
      (case tag
        ((0) (make-AuthenticationOk))
        ((2) (make-AuthenticationKerberosV5))
        ((3) (make-AuthenticationCleartextPassword))
        ((4) (let ([salt (io:read-bytes-as-bytes p 2)])
               (make-AuthenticationCleartextPassword salt)))
        ((5) (let ([salt (io:read-bytes-as-bytes p 4)])
               (make-AuthenticationMD5Password salt)))
        ((6) (make-AuthenticationSCMCredential))
        (else (error 'parse:Authentication
                     "unknown authentication method requested (~s)" tag))))))

(define-struct StartupMessage (parameters))
(define (write:StartupMessage p v)
  (with-length-out p #f
    (io:write-int32 p 196608)
    (for-each (lambda (param)
                (io:write p #:string (car param))
                (io:write p #:string (cdr param)))
              (StartupMessage-parameters v))
    (io:write-byte p 0)))

(define-struct SSLRequest ())
(define (write:SSLRequest p v)
  (io:write-int32 p 8)
  (io:write-int32 p 80877103))

(define-struct CancelRequest (process-id secret-key))
(define (write:CancelRequest p v)
  (io:write-int32 p 16)
  (io:write-int32 p 80877102)
  (io:write-int32 p (CancelRequest-process-id v))
  (io:write-int32 p (CancelRequest-secret-key v)))

(define-struct ErrorResponse (properties))
(define (parse:ErrorResponse p)
  (with-length-in p #\E
    (let* ([fields (parse-field-list p)])
      (make-ErrorResponse fields))))

(define-struct NoticeResponse (properties))
(define (parse:NoticeResponse p)
  (with-length-in p #\N
    (let* ([fields (parse-field-list p)])
      (make-NoticeResponse fields))))

(define (parse-field-list p)
  (let loop ()
    (let ([next (peek-byte p)])
      (cond [(zero? next)
             (begin (read-byte p) null)]
            [else
             (let* ([tag (integer->char (io:read-byte p))]
                    [value (io:read-null-terminated-string p)])
               (cons (cons (char->message-tag tag) value)
                     (loop)))]))))

;; ========================================
;; The normal structures

(define-struct BackendKeyData (process-id secret-key))
(define (parse:BackendKeyData p)
  (with-length-in p #\K
    (let* ([process-id (io:read-int32 p)]
           [secret-key (io:read-int32 p)])
      (make-BackendKeyData process-id secret-key))))

(define-struct Bind (portal statement param-formats values result-formats))
(define (write:Bind p v)
  (match v
    [(struct Bind (portal statement param-formats values result-formats))
     (with-length-out p #\B
       (io:write p #:string portal)
       (io:write p #:string statement)
       (io:write p #:int16 (length param-formats))
       (for ([param-format (in-list param-formats)])
         (io:write p #:int16 param-format))
       (io:write p #:int16 (length values))
       (for ([value (in-list values)])
         (io:write p #:length+string (string/sql-null->string/f value)))
       (io:write p #:int16 (length result-formats))
       (for ([result-format (in-list result-formats)])
         (io:write p #:int16 result-format)))]))

(define-struct BindComplete ())
(define (parse:BindComplete p)
  (with-length-in p #\2
    (make-BindComplete)))

(define-struct Close (type name))
(define (write:Close p v)
  (match v
    [(struct Close (type name))
     (with-length-out p #\C
       (io:write p #:byte/char (statement/portal->char type))
       (io:write p #:string name))]))

(define-struct CloseComplete ())
(define (parse:CloseComplete p)
  (with-length-in p #\3
    (make-CloseComplete)))

(define-struct CommandComplete (command))
(define (parse:CommandComplete p)
  (with-length-in p #\C
    (let* ([command (io:read p #:string)])
      (make-CommandComplete command))))

(define-struct CopyInResponse (format column-formats))
(define (parse:CopyInResponse p)
  (with-length-in p #\G
    (let* ([format (io:read p #:byte)]
           [column-formats
            (for/list ([i (in-range (io:read p #:int16))])
              (io:read p #:int16))])
      (make-CopyInResponse format column-formats))))

(define-struct CopyOutResponse (format column-formats))
(define (parse:CopyOutResponse p)
  (with-length-in p #\H
    (let* ([format (io:read p #:byte)]
           [column-formats
            (for/list ([i (in-range (io:read p #:int16))])
              (io:read p #:int16))])
      (make-CopyOutResponse format column-formats))))

(define-struct DataRow (values))
(define (parse:DataRow p)
  (with-length-in p #\D
    (let* ([values
            (for/list ([i (in-range (io:read p #:int16))])
              (string/f->string/sql-null (io:read p #:length+string)))])
      (make-DataRow values))))

(define-struct Describe (type name))
(define (write:Describe p v)
  (match v
    [(struct Describe (type name))
     (with-length-out p #\D
       (io:write p #:byte/char (statement/portal->char type))
       (io:write p #:string name))]))

(define-struct EmptyQueryResponse ())
(define (parse:EmptyQueryResponse p)
  (with-length-in p #\I
    (make-EmptyQueryResponse)))

(define-struct Execute (portal row-limit))
(define (write:Execute p v)
  (match v
    [(struct Execute (portal row-limit))
     (with-length-out p #\E
       (io:write p #:string portal)
       (io:write p #:int32 row-limit))]))

(define-struct Flush ())
(define (write:Flush p v)
  (with-length-out p #\H))

(define-struct NoData ())
(define (parse:NoData p)
  (with-length-in p #\n
    (make-NoData)))

(define-struct NotificationResponse (process-id condition info))
(define (parse:NotificationResponse p)
  (with-length-in p #\A
    (let* ([process-id (io:read p #:int32)]
           [condition (io:read p #:int32)]
           [info (io:read p #:int32)])
      (make-NotificationResponse process-id condition info))))

(define-struct ParameterDescription (type-oids))
(define (parse:ParameterDescription p)
  (with-length-in p #\t
    (let* ([type-oids
            (for/list ([i (in-range (io:read p #:int16))])
              (io:read p #:int32))])
      (make-ParameterDescription type-oids))))

(define-struct ParameterStatus (name value))
(define (parse:ParameterStatus p)
  (with-length-in p #\S
    (let* ([name (io:read p #:string)]
           [value (io:read p #:string)])
      (make-ParameterStatus name value))))

(define-struct Parse (name query type-oids))
(define (write:Parse p v)
  (match v
    [(struct Parse (name query type-oids))
     (with-length-out p #\P
       (io:write p #:string name)
       (io:write p #:string query)
       (io:write p #:int16 (length type-oids))
       (for ([type-oid (in-list type-oids)])
         (io:write p #:int32 type-oid)))]))

(define-struct ParseComplete ())
(define (parse:ParseComplete p)
  (with-length-in p #\1
    (make-ParseComplete)))

(define-struct PasswordMessage (password))
(define (write:PasswordMessage p v)
  (match v
    [(struct PasswordMessage (password))
     (with-length-out p #\p
       (io:write p #:string password))]))

(define-struct PortalSuspended ())
(define (parse:PortalSuspended p)
  (with-length-in p #\p
    (make-PortalSuspended)))

(define-struct Query (query))
(define (write:Query p v)
  (match v
    [(struct Query (query))
     (with-length-out p #\Q
       (io:write p #:string query))]))

(define-struct ReadyForQuery (transaction-status))
(define (parse:ReadyForQuery p)
  (with-length-in p #\Z
    (let* ([transaction-status
            (char->transaction-status (io:read p #:byte/char))])
      (make-ReadyForQuery transaction-status))))

(define-struct RowDescription (fields))
(define (parse:RowDescription p)
  (with-length-in p #\T
    (let* ([fields
            (for/list ([i (in-range (io:read p #:int16))])
              (let* ([name (io:read p #:string)]
                     [table-oid (io:read p #:int32)]
                     [column-attid (io:read p #:int16)]
                     [type-oid (io:read p #:int32)]
                     [type-size (io:read p #:int16)]
                     [type-mod (io:read p #:int32)]
                     [format-code (io:read p #:int16)])
                (vector name table-oid column-attid type-oid type-size type-mod format-code)))])
      (make-RowDescription fields))))

(define-struct Sync ())
(define (write:Sync p v)
  (with-length-out p #\S))

(define-struct Terminate ())
(define (write:Terminate p v)
  (with-length-out p #\X))

;; ========================================

(define (write-message msg port)
  (define-syntax (gen-cond stx)
    (syntax-case stx ()
      [(gen-cond type ...)
       (with-syntax ([((pred write) ...)
                      (for/list ([type (in-list (syntax->list #'(type ...)))])
                        (list (datum->syntax type
                                (string->symbol (format "~a?" (syntax-e type))))
                              (datum->syntax type
                                (string->symbol (format "write:~a" (syntax-e type))))))])
         #'(cond [(pred msg) (write port msg)] ...
                 [else
                  (error 'write-message "internal error: unknown message type: ~e" msg)]))]))
  (gen-cond Sync
            Parse
            Describe
            Bind
            Execute
            Flush
            Query
            Close
            Terminate
            StartupMessage
            PasswordMessage
            SSLRequest
            CancelRequest))

(define (parse-server-message p)
  (let ([c (io:read p #:byte/char)])
    (case c
      ((#\R) (parse:Authentication p))
      ((#\E) (parse:ErrorResponse p))
      ((#\N) (parse:NoticeResponse p))
      ((#\K) (parse:BackendKeyData p))
      ((#\2) (parse:BindComplete p))
      ((#\3) (parse:CloseComplete p))
      ((#\C) (parse:CommandComplete p))
      ((#\G) (parse:CopyInResponse p))
      ((#\H) (parse:CopyOutResponse p))
      ((#\D) (parse:DataRow p))
      ((#\I) (parse:EmptyQueryResponse p))
      ((#\n) (parse:NoData p))
      ((#\A) (parse:NotificationResponse p))
      ((#\t) (parse:ParameterDescription p))
      ((#\S) (parse:ParameterStatus p))
      ((#\1) (parse:ParseComplete p))
      ((#\p) (parse:PortalSuspended p))
      ((#\Z) (parse:ReadyForQuery p))
      ((#\T) (parse:RowDescription p))
      (else (error 'parse-server-message
                   "internal error: unknown message header byte: ~e" c)))))

;; ========================================

;; == Helpers

(define (string/f->string/sql-null b)
  (if b b sql-null))

(define (string/sql-null->string/f b)
  (if (sql-null? b) #f b))

(define (char->message-tag c)
  (case c
    [(#\S) 'severity]
    [(#\C) 'code]
    [(#\M) 'message]
    [(#\D) 'detail]
    [(#\H) 'hint]
    [(#\P) 'position]
    [(#\p) 'internal-position]
    [(#\q) 'internal-query]
    [(#\W) 'where]
    [(#\F) 'file]
    [(#\L) 'line]
    [(#\R) 'routine]))

(define (char->statement/portal c)
  (case c
    [(#\S) 'statement]
    [(#\P) 'portal]))
(define (statement/portal->char sp)
  (case sp
    [(statement) #\S]
    [(portal) #\P]))

(define (char->transaction-status c)
  (case c
    [(#\I) 'idle]
    [(#\T) 'transaction]
    [(#\E) 'failed]))
(define (transaction-status->char ts)
  (case ts
    [(idle) #\I]
    [(transaction) #\T]
    [(failed) #\E]))

(define (string->command s)
  (cond [(regexp-match #rx"^SELECT *$" s)
         => (lambda (m) (list 'select))]
        [(regexp-match #rx"^INSERT ([0-9]*) ([0-9]*) *$" s)
         => (lambda (m)
              (list 'insert 
                    (string->number (cadr m))
                    (string->number (caddr m))))]
        [(regexp-match #rx"^DELETE ([0-9]* *$)" s)
         => (lambda (m)
              (list 'delete (string->number (cadr m))))]
        [(regexp-match #rx"^UPDATE ([0-9]*) *$" s)
         => (lambda (m)
              (list 'update (string->number (cadr m))))]
        [(regexp-match #rx"^MOVE ([0-9]*) *$" s)
         => (lambda (m)
              (list 'move (string->number (cadr m))))]
        [(regexp-match #rx"^FETCH ([0-9]*) *$" s)
         => (lambda (m)
              (list 'fetch (string->number (cadr m))))]
        [(regexp-match #rx"^(CREATE|ALTER|DROP) ([A-Z]*) *$" s)
         => (lambda (m)
              (list (string->symbol (string-downcase (cadr m)))
                    (string->symbol (string-downcase (caddr m)))))]
        [else s]))

(define (command->string s)
  (if (list? s)
      (apply string-append
             (case (car s)
               [(insert) "INSERT"]
               [(delete) "DELETE"]
               [(update) "UPDATE"]
               [(move) "MOVE"]
               [(fetch) "FETCH"]
               [else s])
             (map (lambda (n) (format " ~a" n))
                  (cdr s)))
      s))

;; parse-field-dvec : list (from RowDescription) -> field-dvec
;; layout is #(name table-oid col-oid typeid typelen typemod text/binary)
(define (parse-field-dvec info)
  (list->vector info))

(define (field-dvec->typeid dvec)
  (vector-ref dvec 3))

(define (field-dvec->field-info dvec)
  (match dvec
    [(vector name table column type-oid type-size type-mod format-code)
     `((name . ,name)
       (typeid . ,type-oid)
       (type-size . ,type-size)
       (type-mod . ,type-mod))]))
