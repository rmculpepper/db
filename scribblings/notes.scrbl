#lang scribble/doc

@(require scribble/manual)
@(require scribble/eval)
@(require scribble/struct)
@(require scheme/sandbox)

@(require (for-label scheme/base)
          (for-label "../generic/main.ss"))

@title{Notes}

@section{Synchronization}

Connections are internally synchronized: it is safe to perform
concurrent queries on the same connection object from different
threads (by direct query methods or prepared query procedures).

The extent of the synchronization covers only the communication with
the server, not the processing of returned results. Clients requiring
synchronized processing of query results must implement the
synchronization themselves.


@section{Character Encodings}

Both PostgreSQL and MySQL allow the character set used for
communication between client and server to be set via SQL
statements. This library, however, assumes that the encoding is always
set to UTF-8. Changing the encoding may cause errors or silent data
corruption. Don't do that.


@section{PostgreSQL-Specific Notes}

@subsection[#:tag "connecting-to-server"]{Connecting to a Server}

By default, many PostgreSQL servers only listen on local domain
sockets. Support is provided for communication over local domain
sockets, but only on Linux (x86) and Mac OS X.

To find the socket path, look in the directory named by the
@tt{unix_socket_directory} variable in @tt{postgresql.conf}.  For
example, on Ubuntu Feisty Fawn running PostgreSQL 8.2, the socket
directory is @tt{/var/run/postgresql} and the socket file is at
@tt{/var/run/postgresql/.s.PGSQL.5432}.

If local socket communication is not available, the server must be
reconfigured to listen to a TCP port and restarted. See the PostgreSQL
manual for information on doing this.

@subsection{Passwords and Authentication}

PostgreSQL also comes with a configuration file (@tt{pg_hba.conf})
which lists accepted authentication methods. It is sometimes necessary
to add lines to this file that describe what authentication method is
used for TCP connections. By default, this library supports only
cleartext and md5-hashed passwords, and it does not send cleartext
passwords unless explicitly ordered to (see @scheme[connect]).

@section{Server Parameters}

This library only understands the the UTF-8 client encoding.
Connections set the encoding when they are created; no server
configuration or user action is necessary. This library does not
support other encodings. If the server changes the client
encoding---for example, in response to a @tt{SET} statement issued by
the user---the connection automatically disconnects and raises an
error.


@section{MySQL-Specific Notes}

The implementation of the MySQL protocol is incomplete. Here are some
known bugs and issues:

@itemize{

@item{Most errors kill the connection unnecesarily.}

@item{SQL types. The support for MySQL types is not as complete as
that for PostgreSQL types. Variations like @tt{unsigned}, precisions
are ignored or dropped.}

@item{No provision is made to declare types for prepared statement
parameters. This sometimes results in prepared statements that expect
strings instead of the intended data.}

@item{MySQL does not support @tt{real} or @tt{numeric} infinities.}

}
