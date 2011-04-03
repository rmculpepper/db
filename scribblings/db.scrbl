#lang scribble/doc
@(require scribble/manual
          scribble/struct
          "config.rkt")

@title[#:version (my-package-version)]{db: Database connectivity}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@section-index["postgresql"]
@section-index["mysql"]
@section-index["sqlite" "sqlite3"]
@section-index["database" "sql"]

@centered{@bold{@italic{A database interface for functional programmers.}}}

@(my-defmodule)

This package provides a high-level interface to several database
systems. The following database systems are currently supported:
@itemlist[
@item{@bold{PostgreSQL versions 7.4 and later}}
@item{@bold{MySQL versions 5 and later}}
@item{@bold{SQLite version 3} --- requires @tt{libsqlite3} native library}
@item{@bold{ODBC} --- requires @tt{libodbc} native library}
]
Support for PostgreSQL and MySQL does not rely on any
native client libraries; this package is everything you
need to connect to a PostgreSQL or MySQL server. SQLite and ODBC
support require the appropriate native libraries to be installed.

The query operations are functional in spirit. Queries return results;
they do not stow them away in the connection for later manipulation
and retrieval. In other words, connections do not contain query
state. Connections are internally synchronized, so multiple threads
can use a connection simultaneously.

@bold{Acknowledgments} Thanks to Dave Gurnell, Noel Welsh, Mike Burns,
and Doug Orleans for contributions to @tt{spgsql}, the PostgreSQL-only
predecessor of this package.

@include-section["introduction.scrbl"]
@include-section["connect.scrbl"]
@include-section["query.scrbl"]
@include-section["sql-types.scrbl"]
@include-section["notes.scrbl"]