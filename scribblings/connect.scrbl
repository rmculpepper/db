#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt"
          (for-label openssl))

@title[#:tag "connect"]{Connections}

@(my-declare-exporting)

This section describes functions for creating connections as well as
administrative functions for managing connections.

@section[#:tag "creating-connections"]{Creating connections}

Connections are made using the following functions.

@defproc[(postgresql-connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 5432]
                  [#:socket socket (or/c path-string? false/c) #f]
                  [#:password password (or/c string? false/c) #f]
                  [#:allow-cleartext-password? allow-cleartext-password?
                   boolean? #f]
                  [#:ssl ssl (or/c 'yes 'optional 'no) 'no]
                  [#:ssl-context ssl-context ssl-client-context?
                   (ssl-make-client-context 'sslv3)]
                  [#:notice-handler notice-handler
                   (or/c 'output 'error output-port?
                         (-> string? string? any))
                   void]
                  [#:notification-handler notification-handler
                   (or/c 'output 'error output-port?
                         (-> string? any))
                   void])
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
  cleartext (un-hashed), the connection is aborted unless
  @racket[allow-cleartext-password?] is true.

  If the @racket[ssl] argument is either @racket['yes] or
  @racket['optional], the connection attempts to negotiate an SSL
  connection. If the server refuses SSL, the connection raises an
  exception if @racket[ssl] was set to @racket['yes] or continues with
  an unencrypted connection if @racket[ssl] was set to
  @racket['optional]. By default, SSL provides encryption but does not
  verify the identity of the server (see
  @hyperlink["http://www.postgresql.org/docs/9.0/static/libpq-ssl.html"]{this
  explanation}). Host verification can be required via the
  @racket[ssl-context] argument; see @racket[ssl-set-verify!]. Some
  servers use SSL certificates to authenticate clients; see
  @racket[ssl-load-certificate-chain!] and
  @racket[ssl-load-private-key!]. SSL may only be used with TCP
  connections, not with local sockets.

  The @racket[notice-handler] is called on notice messages
  received asynchronously from the server. A common example is notice
  of an index created automatically for a table's primary key. The
  @racket[notice-handler] function takes two string arguments: the
  condition's SQLSTATE and a message. The
  @racket[notification-handler] is called in response to an event
  notification (see the @tt{LISTEN} and @tt{NOTIFY} statements); its
  argument is the name of the event as a string. An output port may be
  supplied instead of a procedure, in which case a message is printed
  to the given port. Finally, the symbol @racket['output] causes the
  message to be printed to the current output port, and
  @racket['error] causes the message to be printed to the current
  error port.

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
         path-string?]{

  Attempts to guess the path for the socket based on conventional
  locations. This function returns the first such path that exists in
  the filesystem. It does not check that the path is a socket file,
  nor that the path is connected to a PostgreSQL server.

  If none of the attempted paths exist, an exception is raised.
}

@defproc[(mysql-connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 3306]
                  [#:socket socket (or/c path-string? false/c) #f]
                  [#:password password (or/c string? false/c) #f]
                  [#:notice-handler notice-handler
                   (or/c 'output 'error output-port?
                         (-> exact-nonnegative-integer? string? any))
                   void])
         connection?]{

  Creates a connection to a MySQL server. The meaning of the keyword
  arguments is similar to those of the @racket[postgresql-connect]
  function, except that the first argument to a
  @racket[notice-handler] function is a MySQL-specific integer code
  rather than a SQLSTATE string.

  If the connection cannot be made, an exception is raised.

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
         path-string?]{

  Attempts to guess the path for the socket based on conventional
  locations. This function returns the first such path that exists in
  the filesystem. It does not check that the path is a socket file,
  nor that the path is connected to a MySQL server.

  If none of the attempted paths exist, an exception is raised.
}

@defproc[(sqlite3-connect
                [#:database database (or/c path-string? 'memory 'temporary)]
                [#:mode mode (or/c 'read-only 'read/write 'create) 'read/write]
                [#:busy-retry-limit busy-retry-limit 
                 (or/c exact-nonnegative-integer? +inf.0) 10]
                [#:busy-retry-delay busy-retry-delay
                 (and/c rational? (not/c negative?)) 0.1])
         connection?]{

  Opens the SQLite database at the file named by @racket[database], if
  @racket[database] is a string or path. If @racket[database] is
  @racket['temporary], a private disk-based database is created. If
  @racket[database] is @racket['memory], a private memory-based
  database is created.

  If @racket[mode] is @racket['read-only], the database is opened in
  read-only mode. If @racket[mode] is @racket['read/write] (the
  default), the database is opened for reading and writing (if
  filesystem permissions permit). The @racket['create] mode is like
  @racket['read/write], except that if the given file does not exist,
  it is created as a new database.

  SQLite uses @hyperlink["http://www.sqlite.org/lockingv3.html"]{coarse-grained
  locking}, and many internal operations fail with the
  @tt{SQLITE_BUSY} condition when a lock cannot be acquired. When an
  internal operation fails because the database is busy, the
  connection sleeps for @racket[busy-retry-delay] seconds and retries
  the operation, up to @racket[busy-retry-limit] additional times. If
  @racket[busy-retry-limit] is @racket[0], the operation is only
  attempted once. If after @racket[busy-retry-limit] retries the
  operation still does not succeed, an exception is raised.

  If the connection cannot be made, an exception is raised.

  @(examples/results
    [(sqlite3-connect #:database "/path/to/my.db")
     (new connection%)]
    [(sqlite3-connect #:database "relpath/to/my.db"
                      #:mode 'create)
     (new connection%)])
}

@defproc[(odbc-connect [#:dsn dsn (or/c string? #f) #f]
                       [#:database database (or/c string? #f) #f]
                       [#:user user (or/c string? #f) #f]
                       [#:password password (or/c string? #f) #f]
                       [#:notice-handler notice-handler
                        (or/c output-port? 'output 'error 
                              (-> string? string? any))
                        void]
                       [#:strict-parameter-types? strict-parameter-types? boolean? #f]
                       [#:character-mode character-mode
                        (or/c 'wchar 'utf-8 'latin-1)
                        'wchar])
         connection?]{

  Creates a connection to the ODBC Data Source named @racket[dsn]. The
  @racket[user] and @racket[password] arguments are optional, since
  that information may be incorporated into the data source
  definition, or it might not be relevant to the data source's driver.
  The @racket[notice-handler] argument behaves the same as in
  @racket[postgresql-connect].  The @racket[database] argument is a
  deprecated equivalent of @racket[dsn]. One or the other must be
  provided, but not both.

  If @racket[strict-parameter-types?] is true, then the connection
  attempts to determine and enforce specific types for query
  parameters. See @secref["odbc-types"] for more details.

  By default, connections use ODBC's @tt{SQL_C_WCHAR}-based character
  encoding (as UTF-16) to send Unicode character data. Unfortunately,
  some drivers' support for this method is buggy. To use
  @tt{SQL_C_CHAR} instead, set @racket[character-mode] to
  @racket['utf-8] or @racket['latin-1].

  See @secref["odbc-status"] for notes on specific ODBC drivers and
  recommendations for connection options.

  If the connection cannot be made, an exception is raised.
}

@defproc[(odbc-driver-connect [connection-string string?]
                              [#:notice-handler notice-handler
                               (or/c output-port? 'output 'error
                                     (-> string? string? any))
                               void]
                              [#:strict-parameter-types? strict-parameter-types? boolean? #f]
                              [#:character-mode character-mode
                               (or/c 'wchar 'utf-8 'latin-1)
                               'wchar])
         connection?]{

  Creates a connection using an ODBC connection string containing a
  sequence of keyword and value connection parameters. The syntax of
  connection strings is described in
  @hyperlink["http://msdn.microsoft.com/en-us/library/ms715433%28v=VS.85%29.aspx"]{SQLDriverConnect}
  (see Comments section); supported attributes depend on the
  driver. The other arguments are the same as in @racket[odbc-connect].

  If the connection cannot be made, an exception is raised.
}

@defproc[(odbc-data-sources)
         (listof (list/c string? string?))]{

  Returns a list of known ODBC Data Sources. Each data souce is
  represented by a list of two strings; the first string is the name
  of the data source, and the second is the name of its associated
  driver.
}

@defproc[(odbc-drivers)
         (listof (cons/c string? any/c))]{

  Returns a list of known ODBC Drivers. Each driver is represented by
  a list, the first element of which is the name of the driver. The
  contents of the rest of each entry is currently undefined.
}


@section[#:tag "managing-connections"]{Mangaging connections}

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
@item[@racket['odbc]]
]
}

@defproc[(dbsystem-supported-types [sys dbsystem?])
         (listof symbol?)]{

Returns a list of symbols identifying types supported by the database
system. See @secref["db-types"].
}


@section{System-specific modules}

The @(my-racketmodname) module exports all of the functions listed in
this manual except those described in @secref["util"]. The database
system-specific connection modules are loaded lazily to avoid
unnecessary dependencies on foreign libraries.

The following modules provide subsets of the bindings described in
this manual.

@(my-defmodule/nd base)

Provides all generic connection operations (those described in
@secref{managing-connections} and @secref{query-api}) and SQL data
support (@secref{sql-types}).

@(my-defmodule/nd postgresql)

Provides only @racket[postgresql-connect] and
@racket[postgresql-guess-socket-path].

@(my-defmodule/nd mysql)

Provides only @racket[mysql-connect] and
@racket[mysql-guess-socket-path].

@(my-defmodule/nd sqlite3)

Provides only @racket[sqlite3-connect]. In contrast to
@(my-racketmodname), this module immediately attempts to load the
SQLite native library when required, and it raises an exception if it
cannot be found.

@(my-defmodule/nd odbc)

Provides only @racket[odbc-connect], @racket[odbc-driver-connect],
@racket[odbc-data-sources], and @racket[odbc-drivers]. In contrast to
@(my-racketmodname), this module immediately attempts to load the ODBC
native library when required, and it raises an exception if it cannot
be found.
