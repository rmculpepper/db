#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt")

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
                  [#:ssl ssl (symbols 'yes 'optional 'no) 'no]
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

  The @racket[notice-handler] is called on informational messages
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
  locations. This function returns the first such conventional path
  that exists in the filesystem. It does not check that the path is a
  socket file, nor that the path is connected to a PostgreSQL server.

  If the socket file cannot be found, an error is raised.
}

@defproc[(mysql-connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 3306]
                  [#:socket socket (or/c path-string? false/c) #f]
                  [#:password password (or/c string? false/c) #f])
         connection?]{

  Creates a connection to a MySQL server. The meaning of the keyword
  arguments is similar to those of the @racket[postgresql-connect]
  function.

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
  locations. This function returns the first such conventional path
  that exists in the filesystem. It does not check that the path is a
  socket file, nor that the path is connected to a MySQL server.

  If the socket file cannot be found, an error is raised.
}

@defproc[(sqlite3-connect
                [#:database database (or/c path-string? 'memory 'temporary)]
                [#:mode mode (or/c 'read-only 'read/write 'create) 'read/write])
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

  @(examples/results
    [(sqlite3-connect #:database "/path/to/my.db")
     (new connection%)]
    [(sqlite3-connect #:database "relpath/to/my.db"
                      #:mode 'create)
     (new connection%)])
}

@defproc[(odbc-connect [#:database database string?]
                       [#:user user (or/c string? #f) #f]
                       [#:password password (or/c string? #f) #f]
                       [#:notice-handler notice-handler
                        (or/c output-port? 'output 'error 
                              (-> string? string? any))
                        void])
         connection?]{

  Creates a connection to the ODBC Data Source named
  @racket[database]. The @racket[user] and @racket[password] arguments
  are optional.

  The @racket[notice-handler] argument behaves the same as in
  @racket[postgresql-connect].
}

@defproc[(odbc-driver-connect [connection-string string?]
                              [#:notice-handler notice-handler
                               (or/c output-port? 'output 'error
                                     (-> string? string? any))
                               void])
         connection?]{

  Creates a connection using a connection string containing a sequence
  of keyword and value connection parameters.
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
@tt{libsqlite3} foreign library when required, and it raises an error
if the foreign library cannot be found.

@(my-defmodule/nd odbc)

Provides only @racket[odbc-connect], @racket[odbc-driver-connect],
@racket[odbc-data-sources], and @racket[odbc-drivers]. In contrast to
@(my-racketmodname), this module immediately attempts to load the
@tt{libodbc} foreign library when required, and it raises an error if
the foreign library cannot be found.
