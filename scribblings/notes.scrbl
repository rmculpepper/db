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
sockets, but only on Linux (x86 and x86-64) and Mac OS X. If local
socket communication is not available, the server must be reconfigured
to listen on a TCP port.

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
Unicode (UTF-8) when creating a connection. If a PostgreSQL database's
character encoding is @tt{SQL_ASCII}, however, PostgreSQL will not
honor the connection encoding; it will instead send untranslated
octets, which will cause corrupt data or internal errors in the client
connection.

To convert a PostgreSQL database from @tt{SQL_ASCII} to something
sensible, @tt{pg_dump} the database, recode the dump file (using a
utility such as @tt{iconv}), create a new database with the desired
encoding, and @tt{pg_restore} from the recoded dump file.


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
@tt{pg_hba.conf}, respectively. On Linux, @tt{ident} authentication is
automatically supported for unix domain sockets (but not TCP). The
@tt{gss}, @tt{sspi}, @tt{krb5}, @tt{pam}, and @tt{ldap} methods are
not supported.


@section{SQLite and ODBC native libraries}

SQLite support requires the appropriate native library, specifically
@tt{libsqlite3.so.0} on Unix or @tt{sqlite3.dll} on Windows.

ODBC support requires the appropriate native library, specifically
@tt{libodbc.so.1} on Unix or @tt{odbc32.dll} on Windows. In addition,
the appropriate ODBC Drivers must be installed and any Data Sources
configured.


@section{ODBC}

ODBC support is experimental. This library is compatible only with
ODBC 3.x Driver Managers. The behavior of ODBC connections can vary
widely depending on the driver in use and even the configuration of a
particular data source.

This library is tested with the following configurations, where
@bold{win32} means Windows Vista on a 32-bit processor and
@bold{linux} means Ubuntu 11.04 and unixODBC on both x86 (32-bit) and
x86-64 processors unless otherwise specified.
@itemlist[
@item{@bold{PostgreSQL Unicode} (version 09.00.0300) on @bold{win32} and
  @bold{linux}: Set the following Data Source options to get specific
  parameter type information: @tt{Protocol = 7.4} and
  @tt{UserServerSidePrepare = 1}, and use the
  @racket[#:strict-parameter-types?] connection option. One test
  fails: no error is reported for multiple SQL statements in a
  string. Older versions of the driver have a bug in @tt{WCHAR}
  conversion; use @racket[#:character-mode 'utf-8] as a workaround.}
@item{@bold{MySQL} on @bold{win32} and @bold{linux}: Avoid using the
  @racket[#:strict-parameter-types?] connection option, as the driver
  assigns all parameters the type @tt{varchar}. All tests pass.}
@item{@bold{SQLite3} on @bold{linux}: Avoid
  using the @racket[#:strict-parameter-types?] connection option, as
  the driver assigns all parameters the type
  @tt{longvarchar}. Furthermore, this driver interprets the
  declared types of columns strictly, replacing nonconforming values
  in query results with @tt{NULL}. All computed columns, even those
  with explicit @tt{CAST}s, seem to be returned as @tt{text}. Several
  tests fail because of this behavior.}
]
In addition, the following configurations have been tried but are not
thoroughly tested:
@itemlist[
@item{@bold{DB2} (IBM DB2 Express-C v9.7) on @bold{linux} (32-bit only):
  With @tt{Driver} set to @tt{/home/db2inst1/sqllib/lib32/libdb2.so}
  basic interactions work fine, but the automated test suite cannot
  be run because it uses an incompatible SQL dialect.}
@item{@bold{Oracle} (Oracle Database 10g Release 2, Express
  Edition) on @bold{linux} (32-bit version only): It seems the
  @tt{ORACLE_HOME} and @tt{LD_LIBRARY_PATH} environment variables must
  be set according to the @tt{oracle_env.{csh,sh}} script for the
  driver to work. Basic interactions work fine, but the automated test
  suite cannot be run because it uses an incompatible SQL dialect.}
]
Reports of success or failure on other platforms or with other drivers
would be appreciated.
