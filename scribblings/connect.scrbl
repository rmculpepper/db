#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt")

@title{Connections}

@(my-declare-exporting)

This section describes functions for creating connections as well as
administrative functions for managing connections.

@section{Creating connections}

Connections are made using the following functions.

@defproc[(postgresql-connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 5432]
                  [#:socket socket (or/c path? string? false/c) #f]
                  [#:password password (or/c string? false/c) #f]
                  [#:allow-cleartext-password? allow-cleartext-password?
                   boolean? #f]
                  [#:ssl ssl (symbols 'yes 'optional 'no) 'no])
         connection?]{

  Creates a connection to a PostgreSQL server. The
  @racket[postgresql-connect] function recognizes the keyword
  arguments listed above. Only the @racket[user] and @racket[database]
  arguments are mandatory.

  By default, the connection is made via TCP to @racket["localhost"]
  at port @racket[5432]. To make a different TCP connection, specify
  one or both of the @racket[server] and @racket[port] keyword
  arguments.

  To connect via a local socket, specify the socket path as the
  @racket[socket] argument. You must not supply the @racket[socket]
  argument if you have also supplied either of the TCP arguments. See
  also @secref{connecting-to-server} for notes the socket path, and
  see @racket[postgresql-guess-socket-path] for a way of automatically
  determining the socket path.  Sockets are only available under Linux
  (x86) and Mac OS X.

  If the server requests password authentication, the
  @racket[password] argument must be present; otherwise an exception
  is raised. If the server does not request password authentication,
  the @racket[password] argument is ignored and may be omitted.  A
  connection normally only sends password hashes (using the @tt{md5}
  authentication method). If the server requests a password sent as
  cleartext (un-hashed), the connection is aborted unless a non-false
  value was supplied for the optional
  @racket[allow-cleartext-password?] argument.

  If the @racket[ssl] argument is either @racket['yes] or
  @racket['optional], the connection attempts to negotiate an SSL
  connection. If the server refuses SSL, the connection raises an
  error if @racket[ssl] was set to @racket['yes] or continues with an
  unencrypted connection if @racket[ssl] was set to
  @racket['optional]. SSL may only be used with TCP connections, not
  with local sockets.

  If the connection cannot be made, an exception is raised.

  @(examples/results
    [(postgresql-connect #:server "db.mysite.com"
                         #:port 5432
                         #:database "webappdb"
                         #:user "webapp"
                         #:password "ultra5ecret")
     (new connection%)]
    [(postgresql-connect #:user "me"
                         #:database "me"
                         #:password "icecream")
     (new connection%)]
    [(postgresql-connect @code:comment{Typical socket path}
                         #:socket "/var/run/postgresql/.s.PGSQL.5432"
                         #:user "me"
                         #:database "me")
     (new connection%)]
    [(postgresql-connect #:socket (postgresql-guess-socket-path)
                         #:user "me"
                         #:database "me")
     (new connection%)])
}

@defproc[(postgresql-guess-socket-path)
         (or/c path? string?)]{

  Attempts to guess the path for the socket based on conventional
  locations. This function returns the first such conventional path
  that exists in the filesystem. It does not check that the path is a
  socket file, nor that the path is connected to a PostgreSQL server.

  If the socket file cannot be found, an error is raised.
}

@defproc[(mysql-connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 3306]
                  [#:socket socket (or/c path? string? false/c) #f]
                  [#:password password (or/c string? false/c) #f])
         connection?]{

  Creates a connection to a MySQL server. The meaning of the keyword
  arguments is similar to those of the @racket[postgresql-connect]
  function.

  The default port for MySQL databases is 3306.

  @(examples/results
    [(mysql-connect #:server "db.mysite.com"
                    #:port 3306
                    #:database "webappdb"
                    #:user "webapp"
                    #:password "ultra5ecret")
     (new connection%)]
    [(mysql-connect #:user "me"
                    #:database "me"
                    #:password "icecream")
     (new connection%)]
    [(mysql-connect @code:comment{Typical socket path}
                    #:socket "/var/run/mysqld/mysqld.sock"
                    #:user "me"
                    #:database "me")
     (new connection%)]
    [(mysql-connect #:socket (mysql-guess-socket-path)
                    #:user "me"
                    #:database "me")
     (new connection%)])
}

@defproc[(mysql-guess-socket-path)
         (or/c path? string?)]{

  Attempts to guess the path for the socket based on conventional
  locations. This function returns the first such conventional path
  that exists in the filesystem. It does not check that the path is a
  socket file, nor that the path is connected to a MySQL server.

  If the socket file cannot be found, an error is raised.
}

@defproc[(sqlite3-connect
                [#:database database (or/c path? string? 'memory 'temporary)]
                [#:mode mode (or/c 'read-only 'read/write 'read/write/create) 'read/write])
         connection?]{

  Opens the SQLite database at the file named by @racket[database], if
  @racket[database] is a string or path. If @racket[database] is
  @racket['temporary], a private disk-based database is created. If
  @racket[database] is @racket['memory], a private memory-based
  database is created.

  If @racket[mode] is @racket['read-only], the database is opened in
  read-only mode. If @racket[mode] is @racket['read/write] (the
  default), the database is opened for reading and writing (if
  filesystem permissions permit). The @racket['read/write/create] mode
  is like @racket['read/write], except that if the given file does not
  exist, it is created as a new database.

  @(examples/results
    [(sqlite3-connect #:database "/path/to/my.db")
     (new connection%)]
    [(sqlite3-connect #:database "relpath/to/my.db"
                      #:mode 'read/write/create)
     (new connection%)])
}


@section{Mangaging connections}

@defproc[(connection? [x any/c])
         boolean?]{

Returns @racket[#t] if @racket[x] is a connection, @racket[#f] otherwise.
}

@defproc[(disconnect [connection connection?])
         void?]{
Closes the connection.
}

@defproc[(connected? [connection connection?])
         boolean?]{

Returns @racket[#t] if @racket[connection] is connected, @racket[#f]
otherwise.
}

@defproc[(connection-dbsystem [connection connection?])
         dbsystem?]{

Gets an object encapsulating information about the database system of
@racket[connection].
}

@defproc[(dbsystem? [x any/c])
         boolean?]{

Predicate for objects representing database systems.
}

@defproc[(dbsystem-name [sys dbsystem?])
         symbol?]{

Returns a symbol that identifies the database system. Currently one of the
following:
@itemize[
@item[@racket['postgresql]]
@item[@racket['mysql]]
@item[@racket['sqlite3]]
]
}
