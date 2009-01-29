#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          (for-label scheme/base)
          (for-label "../generic/main.ss"))
@(define the-eval (make-base-eval))
@(interaction-eval #:eval the-eval
                   (require scheme/class
                            "../generic/main.ss"))
@(define-syntax-rule (examples/results [example result] ...)
   (examples #:eval the-eval (eval:alts example result) ...))
@(define-syntax-rule (my-interaction [example result] ...)
   (interaction #:eval the-eval (eval:alts example result) ...))

@title[#:version "1.0"]{db: Database connectivity}
@author{Ryan Culpepper}

This manual documents db version 1.0 (PLaneT version 1 0).

Keywords: _postgresql_, _postgres_, _mysql_, _database_, _db_, _spgsql_

@section{Introduction}

The spgsql library provides a high-level interface to PostgreSQL
database servers. It does not rely on any locally-installed PostgreSQL
client libraries: spgsql is everything you need to connect PLT Scheme
to a PostgreSQL server.

The spgsql library is object-based, using @schememodname[scheme/class]
objects, but connection objects do not contain query state, only
connection state. The query methods are functional in spirit: queries
return results; they do not stow them away in the connection for later
manipulation and retrieval. The higher-order query methods are
patterned after the standard higher-order list processing functions.

Since spgsql does not use foreign connectivity libraries, it works
seamlessly with PLT Scheme's resource management systems. The library
communicates with servers using normal, custodian-managed
ports. Consequently, communication blocks only the thread performing
the communication, unlike some FFI-based approaches.

Spgsql 5 (that is, PLaneT major version 2) only works with PostgreSQL
servers version 7.4 and later. For older servers, use spgsql 4
(PLaneT major version 1).

@include-section["postgresql.scrbl"]
@include-section["mysql.scrbl"]
@include-section["connection.scrbl"]
@include-section["notes.scrbl"]
@include-section["example.scrbl"]

@section{Acknowledgments}

Thanks to Dave Gurnell and Noel Welsh for help implementing SSL
connections. Further thanks to Dave Gurnell for helping to implement,
test, and document date and time handling. Thanks to Mike Burns and
Doug Orleans for help updating spgsql to PLT Scheme 30x.

