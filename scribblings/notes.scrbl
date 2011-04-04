#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt")

@title[#:tag "system-specific-notes"]{System-specific notes}

This section describes miscellaneous issues specific to particular
database systems.

@section[#:tag "connecting-to-server"]{Connecting to a server}

PostgreSQL and MySQL servers are sometimes configured by default to
listen only on local sockets (also called ``unix domain
sockets''). This library provides support for communication over local
sockets, but only on Linux (x86) and Mac OS X. If local socket
communication is not available, the server must be reconfigured to
listen on a TCP port.

The socket file for a PostgreSQL server is located in the directory
specified by the @tt{unix_socket_directory} variable in the
@tt{postgresql.conf} server configuration file.  For example, on
Ubuntu 10.10 running PostgreSQL 8.4, the socket directory is
@tt{/var/run/postgresql} and the socket file is
@tt{/var/run/postgresql/.s.PGSQL.5432}. Common socket paths may be
searched automatically using the @racket[postgresql-guess-socket-path]
function.

The socket file for a MySQL server is located at the path specified by
the @tt{socket} variable in the @tt{my.cnf} configuration file. For
example, on Ubuntu 10.10 running MySQL 5.1, the socket is located at
@tt{/var/run/mysqld/mysqld.sock}. Common socket paths for MySQL can be
searched using the @racket[mysql-guess-socket-path] function.


@section{Character encodings}

Changing a connection's encoding via SQL statements such as @tt{SET
NAMES} is not allowed; when possible, the connection will observe the
change and automatically disconnect with an error.

In most cases, a PostgreSQL or MySQL database's character encoding is
irrelevant, since the connect function always requests translation to
Unicode (UTF-8) when creating a connection. If a database's character
encoding is @tt{SQL_ASCII}, however, PostgreSQL will not honor the
connection encoding; it will instead send untranslated octets, which
will cause corrupt data or internal errors in the client connection.

To convert a PostgreSQL database from @tt{SQL_ASCII} to something
sensible, @tt{pg_dump} the database, recode the dump file, create a
new database with the desired encoding (eg, with @tt{iconv}), and
@tt{pg_restore} from the recoded dump file.


@section{Prepared query parameter types}

Different database systems vary in their handling of query parameter
types. For example, consider the following parameterized SQL
statement:

@tt{SELECT 1 + ?;}

PostgreSQL reports an expected type of @tt{int4} for the parameter and
will not accept other types. MySQL and SQLite, in contrast, report no
useful parameter type information, and ODBC connections vary in
behavior based on the driver and even the connection parameters.


@section{PostgreSQL authentication}

PostgreSQL supports a large variety of authentication mechanisms,
controlled by the @tt{pg_hba.conf} server configuration file. This
library currently supports only cleartext and md5-hashed passwords,
and it does not send cleartext passwords unless explicitly ordered to
(see @racket[postgresql-connect]). These correspond to the @tt{md5}
and @tt{password} authentication methods in the parlance of
@tt{pg_hba.conf}, respectively. On Linux, @tt{ident} authentication
seems to work for unix domain sockets. The @tt{gss}, @tt{sspi},
@tt{krb5}, @tt{pam}, and @tt{ldap} methods are not supported.

@section{SQLite native library}

Requires the @tt{libsqlite3} native library, specifically
@tt{libsqlite3.so.0}.


@section{ODBC}

Requires the @tt{libodbc} native library, specifically
@tt{libodbc.so.1}. This library is provided by packages such as
@tt{unixODBC} or @tt{iODBC}. In addition, the appropriate ODBC Drivers
must be installed and any Data Sources configured.

ODBC support is experimental.
