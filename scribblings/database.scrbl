#lang scribble/doc
@(require scribble/manual
          scribble/struct
          "config.ss"
          (for-label scheme/base)
          (for-label "../generic/main.ss"))

@title[#:version (package-version)]{database: Database connectivity}
@author{Ryan Culpepper}

@section-index["postgresql"]
@section-index["mysql"]
@section-index["database"]

This manual documents the database package version
@(package-version). This package is the successor to
spgsql.

@defmodule/this-package[]

The database package provides a high-level interface to PostgreSQL and
MySQL database servers. It does not rely on any locally-installed
client libraries; this package is everything you need to connect PLT
Scheme to a PostgreSQL or MySQL server.

The library is object-based, using @schememodname[scheme/class]
objects, but connection objects do not contain query state. The query
methods are functional in spirit: queries return results; they do not
stow them away in the connection for later manipulation and
retrieval. The higher-order query methods are patterned after standard
higher-order list processing functions.

Since this package does not use foreign connectivity libraries, it
works seamlessly with PLT Scheme's resource management systems. The
library communicates with servers using normal, custodian-managed
ports. Consequently, communication blocks only the thread performing
the communication, unlike some FFI-based approaches.

The database package is compatible with PostgreSQL servers version 7.4
and later and MySQL servers version 5 and later.

@include-section["connect.scrbl"]
@include-section["query.scrbl"]
@include-section["sql-types.scrbl"]
@include-section["class-api.scrbl"]
@include-section["notes.scrbl"]
@include-section["example.scrbl"]

@section{Acknowledgments}

Thanks to Dave Gurnell, Noel Welsh, Mike Burns, and Doug Orleans for
contributions to spgsql, the PostgreSQL-only predecessor of this
package.
