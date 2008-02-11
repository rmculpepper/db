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

(Synchronization was added in spgsql version 5.1.)

@section[#:tag "connecting-to-server"]{Connecting to a Server}

By default, many PostgreSQL servers only listen on local domain
sockets. spgsql 5 provides experimental support for communication over
local domain sockets, but only on Linux (x86) and Mac OS X.

To find the socket path, look in the directory named by the
@tt{unix_socket_directory} variable in @tt{postgresql.conf}.  For
example, on Ubuntu Feisty Fawn running PostgreSQL 8.2, the socket
directory is @tt{/var/run/postgresql} and the socket file is at
@tt{/var/run/postgresql/.s.PGSQL.5432}.

If local socket communication is not available, the server must be
reconfigured to listen to a TCP port and restarted. See the PostgreSQL
manual for information on doing this.

@section{Passwords and Authentication}

PostgreSQL also comes with a configuration file (@tt{pg_hba.conf})
which lists accepted authentication methods. It is sometimes necessary
to add lines to this file that describe what authentication method is
used for TCP connections. By default, spgsql only supports cleartext
and md5-hashed passwords, and it does not send cleartext passwords
unless explicitly ordered to (see @scheme[connect]).

@section{Server Parameters}

The spgsql library only understands the the UTF-8 client encoding.
Connections set the encoding when they are created; no server
configuration or user action is necessary. This library does not
support other encodings. If the server changes the client
encoding---for example, in response to a @tt{SET} statement issued by
the user---the connection automatically disconnects and raises an
error.

@section{Changes from version 4}

Version 5 of spgsql is not source-compatible with prior versions. This
section outlines the substantial differences.

@subsection{Connecting}

The @scheme[connect] procedure once took fixed arguments; now it takes
keyword arguments.

Support for @tt{crypt()}-passwords has been dropped.

@subsection{Queries}

All queries are now restricted to a single SQL statement. Support for
@tt{COPY} statements has been dropped.

The old low-level query methods, such as @scheme[query], have been
changed or removed. The new low-level query methods use different
datatypes.

The high-level query methods are mostly the same. The
@scheme[fold-right] method has been dropped, and the old misnamed
@scheme[query-tuple] method has been renamed @scheme[query-row] in
anticipation of a proper tuple representation in the future.

Type conversions are enabled automatically and cannot be
disabled. Unconverted data is represented as strings. Future
releases may add the capability to configure type conversions.

The @scheme[sql-format] procedure has been replaced with the
@scheme[format-sql] macro.

@subsection{Errors and Events}

The exception hierarchy has been dropped. There is no way to tell a
fatal error from a nonfatal error by the exception object; use the
@method[connection<%> connected?] method instead. Generally, internal
errors and communication errors are fatal; query errors and user
errors are not.

The @scheme[set-notification-handler] and @scheme[set-notice-handler]
methods have been dropped. Future releases may restore the capability
to react to asynchronous events.

