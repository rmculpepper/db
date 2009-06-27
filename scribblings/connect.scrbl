#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          (planet cce/scheme:3)
          (for-label scheme/base)
          (for-label "../generic/main.ss")
          (for-label "../main.ss"))

@title{Connecting to a server}
@declare-exporting/this-package[]

@section{Connecting to a PostgreSQL server}

Use the following procedure to create a connection:

@defproc[(postgresql:connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 5432]
                  [#:socket socket (or/c path? string? bytes? false/c) #f]
                  [#:password password (or/c string? false/c) #f]
                  [#:allow-cleartext-password? allow-cleartext-password?
                   boolean? #f]
                  [#:ssl ssl (symbols 'yes 'optional 'no) 'no])
         (and/c (is-a/c connection:admin<%>)
                (is-a/c connection:query<%>)
                (is-a/c connection:query/prepare<%>))]{

  Creates a connection to a PostgreSQL server. The
  @scheme[postgresql:connect] procedure recognizes the keyword
  arguments listed above. Only the @scheme[user] and @scheme[database]
  arguments are mandatory.

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
  @scheme[password] argument must be present; otherwise an exception
  is raised. If the server does not request password authentication,
  the @scheme[password] argument is ignored and may be omitted.

  A connection normally only sends password hashes (using the @tt{md5}
  authentication method). If the server requests a password sent in
  the clear (un-hashed), the connection is aborted unless a non-false
  value is supplied for the optional
  @scheme[allow-cleartext-password?] argument.

  If the @scheme[ssl] argument is either @scheme['yes] or
  @scheme['optional], the connection attempts to negotiate an SSL
  connection. If the server refuses SSL, the connection raises an
  error if @scheme[ssl] was set to @scheme['yes] or continues with a
  normal connection if @scheme[ssl] was set to @scheme['optional]. SSL
  may only be used with TCP connections, not with local sockets.

  If the connection cannot be made, an exception is raised.

  @(schemeinput
    (postgresql:connect #:server "db.mysite.com"
             #:port 5432
             #:database "webappdb"
             #:user "webapp"
             #:password "ultra5ecret"))
  @(schemeinput
    (postgresql:connect #:user "me"
             #:database "me"
             #:password "icecream"))
  @(schemeinput
    (postgresql:connect @code:comment{Typical socket path on some PostgreSQL configurations}
             #:socket "/var/run/postgresql/.s.PGSQL.5432"
             #:user "me"
             #:database "me"))
}


@section{Connecting to a MySQL server}

Use the following procedure to create a connection:

@defproc[(mysql:connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 3306]
                  [#:socket socket (or/c path? string? bytes? false/c) #f]
                  [#:password password (or/c string? false/c) #f]
                  [#:ssl ssl (symbols 'yes 'optional 'no) 'no])
         (and/c (is-a/c connection:admin<%>)
                (is-a/c connection:query<%>)
                (is-a/c connection:query/prepare<%>))]{

  Creates a connection to a MySQL server. The meaning of the keyword
  arguments is similar to those of the @scheme[postgresql:connect]
  procedure.

  The default port for MySQL databases is 3306.

  @(schemeinput
    (mysql:connect #:server "db.mysite.com"
              #:port 3306
              #:database "webappdb"
              #:user "webapp"
              #:password "ultra5ecret"))
  @(schemeinput
    (mysql:connect #:user "me"
              #:database "me"
              #:password "icecream"))
  @(schemeinput
    (mysql:connect @code:comment{Typical socket path on some MySQL configurations}
              #:socket "/var/run/mysqld/mysqld.sock"
              #:user "me"
              #:database "me"))

}
