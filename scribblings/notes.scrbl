#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt")

@title{System-specific notes}

This section describes issues specific to particular database systems.

@section{PostgreSQL}

@subsection[#:tag "connecting-to-server"]{Connecting to a server}

Some PostgreSQL servers are configured by default to listen only on
local sockets (also called ``unix domain sockets''). This library
provides support for communication over local sockets, but only on
Linux (x86) and Mac OS X. If local socket communication is not
available, the server must be reconfigured to listen to a TCP port and
restarted.

The socket file is located in the directed specified by the
@tt{unix_socket_directory} variable in the @tt{postgresql.conf} server
configuration file.  For example, on Ubuntu Feisty Fawn running
PostgreSQL 8.2, the socket directory is @tt{/var/run/postgresql} and
the socket file is @tt{/var/run/postgresql/.s.PGSQL.5432}. Common
socket paths may be searched automatically using the
@racket[postgresql-guess-socket-path] function.

@subsection{Authentication}

PostgreSQL supports a large variety of authentication mechanisms,
controlled by the @tt{pg_hba.conf} server configuration file. This
library currently supports only cleartext and md5-hashed passwords,
and it does not send cleartext passwords unless explicitly ordered to
(see @racket[postgresql-connect]). These correspond to the @tt{md5}
and @tt{password} authentication methods in the parlance of
@tt{pg_hba.conf}, respectively. On Linux, @tt{ident} authentication
seems to work for unix domain sockets. The @tt{gss}, @tt{sspi},
@tt{krb5}, @tt{pam}, and @tt{ldap} methods are not supported.

@subsection{Notices and notifications}

This library does not currently handle notices or notifications.

@subsection{Character encoding}

In most cases, a PostgreSQL database's character encoding is
irrelevant, since this library always requests translation to Unicode
(UTF-8) when creating a connection. If a database's character encoding
is @tt{SQL_ASCII}, however, PostgreSQL will not honor the connection
encoding; it will instead send untranslated octets, which will cause
corrupt data or internal errors in the client connection.

To convert a PostgreSQL from @tt{SQL_ASCII} to something sensible,
@tt{pg_dump} the database, recode the dump file, create a new database
with the desired encoding, and @tt{pg_restore} from the recoded dump
file. For example, to interpret strings in the old database as
@tt{LATIN1} and load them into a @tt{UTF8} database, convert the dump
file thus:

@tt{iconv -f latin1 -t utf8 < dump.sql > dump-utf8.sql}

Changing the connection encoding (via the @tt{SET client_encoding}
statement) is not allowed; the connection will observe the change and
automatically disconnect with an error.

@section{MySQL}

@subsection{SQL types}

The support for MySQL types is not as complete as that for PostgreSQL
types. Variations like @tt{unsigned}, precisions are ignored or
dropped.

MySQL does not support @tt{real} or @tt{numeric} infinities.

@subsection{Prepared query parameter types}

MySQL frequently fails to infer reasonable types for parameters in
prepared queries. Currently there is no provision for declaring
parameter types when creating a prepared statement.


@section{SQLite}

Requires the @tt{libsqlite3} shared library (specifically
@tt{libsqlite3.so.0}).


@section{ODBC}

Requires the @tt{libodbc} shared library (specifically
@tt{libodbc.so.1}), provided by a package such as @tt{unixODBC} or
@tt{iODBC}.
