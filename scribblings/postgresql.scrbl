#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          (for-label scheme/base)
          (for-label "../generic/main.ss")
          (for-label "../postgresql/main.ss"))

@(define the-eval (make-base-eval))
@(interaction-eval #:eval the-eval
                   (require scheme/class
                            "../generic/main.ss"
                            "../postgresql/main.ss"
                            "../postgresql/connection.ss"))
@(define-syntax-rule (examples/results [example result] ...)
   (examples #:eval the-eval (eval:alts example result) ...))
@(define-syntax-rule (my-interaction [example result] ...)
   (interaction #:eval the-eval (eval:alts example result) ...))

@title{Connecting to a PostgreSQL server}

@defmodule["../postgresql/main.ss"]

Use the following procedure to create a connection:

@defproc[(connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 5432]
                  [#:socket socket (or/c path? string? bytes? false/c) #f]
                  [#:password password (or/c string? false/c) #f]
                  [#:allow-cleartext-password? allow-cleartext-password?
                   boolean? #f]
                  [#:ssl ssl (symbols 'yes 'optional 'no) 'no])
         (and/c (is-a/c connection<%>)
                (is-a/c connection:query<%>)
                (is-a/c connection:query/prepare<%>))]{

  Creates a connection to a PostgreSQL server. The @scheme[connect]
  procedure recognizes the keyword arguments listed above. Only the
  @scheme[user] and @scheme[database] arguments are mandatory. 

  By default, the connection is made via TCP to @scheme["localhost"]
  at port @scheme[5432]. To make a different TCP connection, specify
  one or both of the @scheme[server] and @scheme[port] keyword
  arguments.

  To connect via a local socket, specify the socket path as the
  @scheme[socket] argument. You must not supply the @scheme[socket]
  argument if you have also supplied either of the TCP arguments. See
  @secref{connecting-to-server} for notes on finding the socket path.
  Sockets are only available under Linux (x86) and Mac OS X.

  If the server requests password authentication, the
  @scheme[password] argument must be present; otherwise
  @scheme[connect] raises an exception. If the server does not request
  password authentication, the @scheme[password] argument is ignored
  and may be omitted.

  The @scheme[connect] procedure normally only sends password hashes
  (using the @tt{md5} authentication method). If the server requests a
  password sent in the clear (un-hashed), @scheme[connect] aborts the
  connection unless a non-false value is supplied for the optional
  @scheme[allow-cleartext-password?] argument.

  If the @scheme[ssl] argument is either @scheme['yes] or
  @scheme['optional], @scheme[connect] attempts to negotiate an SSL
  connection. If the server refuses SSL, @scheme[connect] raises an
  error if @scheme[ssl] was set to @scheme['yes] or continues with a
  normal connection if @scheme[ssl] was set to @scheme['optional]. SSL
  may only be used with TCP, not with local sockets.

  If the connection cannot be made, @scheme[connect] raises an exception.

  @(examples/results
    [(connect #:server "db.mysite.tla"
              #:port 5432
              #:database "webappdb"
              #:user "webapp"
              #:password "ultra5ecret")
     (new connection%)]
    [(connect #:user "me"
              #:database "me"
              #:password "icecream")
     (new connection%)]
    [(connect @code:comment{Typical socket path on some PostgreSQL configurations}
              #:socket "/var/run/postgresql/.s.PGSQL.5432"
              #:user "me"
              #:database "me")
     (new connection%)])
}
