#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt")

@title[#:tag "util"]{Utilities}

The bindings described in this section are not provided by
@(my-racketmodname).

@section[#:tag "connect-util"]{Connection utilities}

@(my-defmodule util/connect)

@defproc[(connection-generator
             [generator (-> connection?)]
             [#:timeout timeout (or/c #f (and/c rational? positive?)) #f])
         connection?]{

Creates a virtual connection that creates actual connections on demand
using the @racket[generator] function. A connection generator
encapsulates a mapping of threads to actual connections. When a query
function is called with a connection generator, the current thread's
associated actual connection is used to execute the query. If there is
no actual connection associated with the current thread, one is
obtained by calling @racket[generator]. An actual connection is
disconnected when its associated thread dies or if @racket[timeout]
seconds elapse since the actual connection was last used.

When given a connection produced by @racket[connection-generator],
@racket[connected?] indicates whether there is an actual connection
associated with the current thread. Likewise, @racket[disconnect]
causes the current actual connection associated with the thread (if
there is one) to be disconnected, but the connection will be recreated
if a query function is executed.

@examples/results[
[(define c
   (connection-generator
    (lambda ()
      (printf "connecting!\n")
      (postgresql-connect ....))))
 (void)]
[(connected? c)
 (values #f)]
[(query-value c "select 1")
 (begin (printf "connecting!\n") 1)]
[(connected? c)
 (values #t)]
[(void (thread (lambda () (displayln (query-value c "select 2")))))
 (begin (printf "connecting!\n") (displayln 2))]
[(disconnect c)
 (void)]
[(connected? c)
 (values #f)]
[(query-value c "select 3")
 (begin (printf "connecting!\n") 3)]
]

Connections produced by @racket[connection-generator] may not be used
with the @racket[prepare] function. However, they may still be used to
execute parameterized queries expressed as strings or encapsulated via
@racket[statement-generator].

@examples/results[
[(prepare c "select 2 + $1")
 (error 'prepare "cannot prepare statement with connection-generator")]
[(query-value c "select 2 + $1" 2)
 4]
[(define pst (statement-generator "select 2 + $1"))
 (void)]
[(query-value c pst 3)
 5]
]
}


@defproc[(kill-safe-connection [c connection?]) 
         connection?]{

Creates a proxy for connection @racket[c]. All queries performed
through the proxy are kill-safe; that is, if a thread is killed during
a call to a query function such as @racket[query], the connection will
not become locked or damaged. (Connections are normally thread-safe but
not kill-safe.)

Note: A kill-safe connection whose underlying connection uses ports to
communicate with a database server is not protected from a custodian
shutting down its ports.
}


@;{========================================}

@section{RaDSN (Racket Data Source Names)}

@(my-defmodule util/radsn)

Inspired by ODBC's Data Source Names, this library defines a mechanism
for abbreviating connection information. A RaDSN (Racket Data Source
name) is a symbol associated with a connection specification in a
RaDSN file.

@defstruct*[data-source
              ([connector (or/c 'postgresql 'mysql 'sqlite3 'odbc)]
               [args list?]
               [extensions (listof (list/c symbol? any/c))])
            #:mutable]{

  Represents a data source. The @racket[connector] field determines
  which connection function is used to create the connection. The
  @racket[args] field is a partial list of arguments passed to the
  connection function; additional arguments may be added when
  @racket[radsn-connect] is called. The @racket[extensions] field
  contains additional information about a connection; for example,
  this library's testing framework uses it to store SQL dialect
  flags.

  Data sources can also be created using the
  @racket[postgresql-data-source], etc auxiliary functions.
}

@defproc[(radsn-connect [radsn (or/c symbol? data-source?)]
                        [#:radsn-file radsn-file path-string? (current-radsn-file)]
                        [arg any/c] ...
                        [#:<kw> kw-arg any/c] ...)
         connection?]{

  Makes a connection using the connection information associated with
  @racket[radsn] in @racket[radsn-file]. The given @racket[arg]s and
  @racket[kw-arg]s are added to those specified by @racket[radsn] to
  form the complete arguments supplied to the connect function.

  If @racket[radsn-file] does not exist, or if it contains no entry
  for @racket[radsn], an exception is raised. If @racket[radsn] is a
  @racket[data-source], then @racket[radsn-file] is ignored.
}

@defparam[current-radsn-file x path-string?]{

  A parameter holding the location of the default RaDSN file. The
  initial value is a file located immediately within
  @racket[(find-system-path 'prefs-dir)].
}

@defproc[(get-radsn [radsn symbol?]
                    [default any/c #f]
                    [radsn-file path-string? (current-radsn-file)])
         (or/c data-source? any/c)]{

  Returns the @racket[data-source] associated with @racket[radsn] in
  @racket[radsn-file].

  If @racket[radsn-file] does not exist, an exception is raised. If
  @racket[radsn-file] does not have an entry for @racket[radsn],
  @racket[default] is called if it is a function or returned
  otherwise.
}

@defproc[(put-radsn [radsn symbol?]
                    [ds (or/c data-source? #f)]
                    [radsn-file path-string? (current-radsn-file)])
         void?]{

  Associates @racket[radsn] with the given data source @racket[ds] in
  @racket[radsn-file], replacing the previous association, if one
  exists.
}

@(define absent @italic{absent})

@deftogether[[
@defproc[(postgresql-data-source
           [#:user user string? @#,absent]
           [#:database database string? @#,absent]
           [#:server server string? @#,absent]
           [#:port port exact-positive-integer? @#,absent]
           [#:socket socket (or/c path-string? #f) @#,absent]
           [#:password password (or/c string? #f) @#,absent]
           [#:allow-cleartext-password? allow-cleartext-password? boolean? @#,absent]
           [#:ssl ssl (or/c 'yes 'optional 'no) @#,absent]
           [#:notice-handler notice-handler (or/c 'output 'error) @#,absent]
           [#:notification-handler notification-handler (or/c 'output 'error) @#,absent])
         data-source?]
@defproc[(mysql-data-source
           [#:user user string? @#,absent]
           [#:database database string? @#,absent]
           [#:server server string? @#,absent]
           [#:port port exact-positive-integer? @#,absent]
           [#:socket socket (or/c path-string? #f) @#,absent]
           [#:password password (or/c string? #f) @#,absent]
           [#:notice-handler notice-handler (or/c 'output 'error) @#,absent])
         data-source?]
@defproc[(sqlite3-data-source
           [#:database database (or/c path-string? 'memory 'temporary) @#,absent]
           [#:mode mode (or/c 'read-only 'read/write 'create) @#,absent]
           [#:busy-retry-limit busy-retry-limit 
            (or/c exact-nonnegative-integer? +inf.0) @#,absent]
           [#:busy-retry-delay busy-retry-delay
            (and/c rational? (not/c negative?)) @#,absent])
         data-source?]
@defproc[(odbc-data-source
           [#:dsn dsn (or/c string? #f) @#,absent]
           [#:database database (or/c string? #f) @#,absent]
           [#:user user (or/c string? #f) @#,absent]
           [#:password password (or/c string? #f) @#,absent]
           [#:notice-handler notice-handler (or/c 'output 'error) @#,absent]
           [#:strict-parameter-types? strict-parameter-types? boolean? @#,absent]
           [#:character-mode character-mode (or/c 'wchar 'utf-8 'latin-1) @#,absent])
         data-source?]]]{

  Analogues of @racket[postgresql-connect], @racket[mysql-connect],
  @racket[sqlite3-connect], and @racket[odbc-connect], respectively,
  that return a @racket[data-source] describing the (partial)
  connection information. All arguments are optional, even those that
  are mandatory in the corresponding connection function; the missing
  arguments must be supplied when @racket[radsn-connect] is called.
}


@;{========================================}

@section[#:tag "geometry"]{Geometric types}

@(my-defmodule util/geometry)

The following structures and functions deal with geometric values
based on the OpenGIS (ISO 19125) model.

@section-index{PostGIS}

Note: Geometric columns defined using the PostGIS extension to
PostgreSQL are not directly supported. Instead, data should be
exchanged in the Well-Known Binary format; conversion of the following
structures to and from WKB format is supported by the
@racket[wkb->geometry] and @racket[geometry->wkb] functions.

@defstruct*[point
            ([x real?] [y real?])]{
  Represents an OpenGIS @tt{Point}.
}
@defstruct*[line-string
            ([points (listof point?)])]{
  Represents an OpenGIS @tt{LineString}.
}
@defstruct*[polygon
            ([exterior linear-ring?]
             [interior (listof linear-ring?)])]{
  Represents an OpenGIS @tt{Polygon}.
}
@defstruct*[multi-point ([elements (listof point?)])]{
  Represents an OpenGIS @tt{MultiPoint}, a collection of points.
}
@defstruct*[multi-line-string ([elements (listof line-string?)])]{
  Represents an OpenGIS @tt{MultiLineString}, a collection of line-strings.
}
@defstruct*[multi-polygon ([elements (listof polygon?)])]{
  Represents an OpenGIS @tt{MultiPolygon}, a collection of polygons.
}
@defstruct*[geometry-collection ([elements (listof geometry2d?)])]{
  Represents an OpenGIS @tt{GeometryCollection}, a collection of
  arbitrary geometric values.
}

@defproc[(geometry2d? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[point],
  @racket[line-string], @racket[polygon], @racket[multi-point],
  @racket[multi-line-string], @racket[multi-polygon], or
  @racket[geometry-collection]; @racket[#f] othewise.
}

@defproc[(line? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[line-string]
  consisting of exactly two points (cf OpenGIS @tt{Line}); @racket[#f]
  otherwise.
}

@defproc[(linear-ring? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[line-string] whose
  first and last points are equal (cf OpenGIS @tt{LinearRing});
  @racket[#f] otherwise.
}

@defproc[(geometry->wkb [g geometry2d?]
                        [#:big-endian? big-endian? (system-big-endian?)])
         bytes?]{

  Returns the Well-Known Binary (WKB) encoding of the geometric value
  @racket[g]. The @racket[big-endian?] argument determines the byte
  order used (the WKB format includes byte-order markers, so a robust
  client should accept either encoding).
}

@defproc[(wkb->geometry [b bytes?])
         geometry2d?]{

  Decodes the Well-Known Binary (WKB) representation of a geometric
  value.
}

@;{========================================}

@section[#:tag "postgresql-ext"]{PostgreSQL-specific types}

@(my-defmodule util/postgresql)

The following structures represent certain of PostgreSQL's built-in
geometric types that have no appropriate analogue in the OpenGIS
model: @tt{box}, @tt{path}, and @tt{circle}. The @tt{point},
@tt{lseg}, and @tt{polygon} PostgreSQL built-in types are represented
using @racket[point], @racket[line-string] (@racket[line?]), and
@racket[polygon] structures.

Note: PostgreSQL's built-in geometric types are distinct from those
provided by the PostGIS extension library (see @secref["geometry"]).

@defstruct*[pg-box
            ([ne point?] [sw point?])]

@defstruct*[pg-path
            ([closed? boolean?] [points (listof point?)])]

@defstruct*[pg-circle
            ([center point?] [radius real?])]
