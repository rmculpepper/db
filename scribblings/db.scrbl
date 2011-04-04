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
@item{@bold{ODBC} --- experimental, requires @tt{libodbc} native library}
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
predecessor of this package. The SQLite support is based in part on
code from Jay McCarthy's
@hyperlink["http://planet.racket-lang.org/display.ss?package=sqlite.plt&owner=jaymccarthy"]{sqlite}
package.

@bold{Copying} This program is free software: you can redistribute
it and/or modify it under the terms of the
@hyperlink["http://www.gnu.org/licenses/lgpl.html"]{GNU Lesser General
Public License} as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License and GNU Lesser General Public License for more
details.

@bold{Development} Development of this library is hosted by
@hyperlink["http://github.com"]{GitHub} at the following project page:

@centered{@hyperlink["https://github.com/rmculpepper/db"]{https://github.com/rmculpepper/db}}

@include-section["introduction.scrbl"]
@include-section["connect.scrbl"]
@include-section["query.scrbl"]
@include-section["sql-types.scrbl"]
@include-section["notes.scrbl"]
