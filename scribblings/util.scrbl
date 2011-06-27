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

@subsection{Connection pooling}

Creating an ordinary connection is often a relatively costly
operation; it may involve steps such as process creation and SSL
negotiation. A @deftech{connection pool} helps reduce connection costs
by reusing connections.

@defproc[(connection-pool
             [connect (-> connection?)]
             [#:max-connections max-connections (or/c (integer-in 1 10000) +inf.0) +inf.0]
             [#:max-idle-connections max-idle-connections (or/c (integer-in 1 10000) +inf.0) 10])
         connection-pool?]{

Creates a @tech{connection pool}. The pool consists of up to
@racket[max-connections], divided between leased connections and up to
@racket[max-idle-connections] idle connections. The pool uses
@racket[connect] to create new connections when needed; the
@racket[connect] function must return a fresh connection each time it
is called.

@examples/results[
[(define pool
  (connection-pool
   (lambda () (displayln "connecting!") (sqlite3-connect ....))
   #:max-idle-connections 1))
 (void)]
[(define c1 (connection-pool-lease pool))
 (displayln "connecting!")]
[(define c2 (connection-pool-lease pool))
 (displayln "connecting!")]
[(disconnect c1)
 (void)]
[(code:line (define c3 (connection-pool-lease pool)) (code:comment "reuses actual conn. from c1"))
 (void)]
]

See also @racket[virtual-connection] for a mechanism that eliminates
the need to explicitly call @racket[connection-pool-lease] and
@racket[disconnect].
}

@defproc[(connection-pool? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a connection pool, @racket[#f]
otherwise.
}

@defproc[(connection-pool-lease
             [pool connection-pool?]
             [release (or/c evt? custodian?) (current-thread)])
         connection?]{

Obtains a connection from the connection pool, using an existing idle
connection in @racket[pool] if one is available. If no idle connection
is available and the pool contains fewer than its maximum allowed
connections, a new connection is created; otherwise an exception is
raised.

Calling @racket[disconnect] on the connection obtained causes the
connection to be released back to the connection pool. The connection
is also released if @racket[release] becomes available, if it is a
synchronizable event, or if @racket[release] is shutdown, if it is a
custodian. The default for @racket[release] is the current thread, so
the resulting connection is released when the thread that requested it
terminates.

When a connection is released, it is kept as an idle connection if
@racket[pool]'s idle connection limit would not be exceeded;
otherwise, it is disconnected. If the connection is in a transaction,
the transaction is rolled back.
}


@;{========================================}

@subsection{Virtual connections}

@;{Should have called this "virtual-connection" to start with.}

A @deftech{virtual connection} creates actual connections on demand and
automatically releases them when they are no longer needed.

@defproc[(virtual-connection
             [connect (or/c (-> connection?) connection-pool?)]
             #| [#:timeout timeout (and/c real? positive?) +inf.0] |#)
         connection?]{

Creates a @tech{virtual connection} that creates actual connections on
demand using the @racket[connect] function, or by calling
@racket[(connection-pool-lease connect)] if @racket[connect] is a
@tech{connection pool}. A virtual connection encapsulates a mapping
of threads to actual connections. When a query function is called with
a virtual connection, the current thread's associated actual
connection is used to execute the query. If there is no actual
connection associated with the current thread, one is obtained by
calling @racket[connect]. An actual connection is disconnected when
its associated thread dies.

@;{or if @racket[timeout] seconds elapse since the actual connection was last used.}

Virtual connections are especially useful in contexts such as web
servlets, where each request is handled in a fresh thread. A single
global virtual connection can be defined, freeing each servlet request
from explicitly opening and closing its own connections. In
particular, a @tech{virtual connection} backed by a @tech{connection
pool} combines convenience with efficiency:

@examples/results[
[(define the-connection
   (virtual-connection (connection-pool (lambda () ....))))
 (void)]
]

The resulting virtual connection leases a connection from the pool on
demand for each servlet request thread and releases it when the thread
terminates (that is, when the request has been handled).

When given a connection produced by @racket[virtual-connection],
@racket[connected?] indicates whether there is an actual connection
associated with the current thread. Likewise, @racket[disconnect]
causes the current actual connection associated with the thread (if
there is one) to be disconnected, but the connection will be recreated
if a query function is executed.

@examples/results[
[(define c
   (virtual-connection
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

Connections produced by @racket[virtual-connection] may not be used
with the @racket[prepare] function. However, they may still be used to
execute parameterized queries expressed as strings or encapsulated via
@racket[virtual-statement].

@examples/results[
[(prepare c "select 2 + $1")
 (error 'prepare "cannot prepare statement with virtual connection")]
[(query-value c "select 2 + $1" 2)
 4]
[(define pst (virtual-statement "select 2 + $1"))
 (void)]
[(query-value c pst 3)
 5]
]
}

@defproc[(connection-generator [connect (-> connection?)])
         connection?]{

Deprecated. Equivalent to @racket[(virtual-connection connect)].
}

@;{========================================}

@subsection{Kill-safe connections}

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

@subsection{Data source names}

A DSN (Data Source name) is a symbol associated with a
connection specification in a DSN file (inspired by ODBC's DSNs).

@defstruct*[data-source
              ([connector (or/c 'postgresql 'mysql 'sqlite3 'odbc)]
               [args list?]
               [extensions (listof (list/c symbol? any/c))])
            #:mutable]{

  Represents a data source. The @racket[connector] field determines
  which connection function is used to create the connection. The
  @racket[args] field is a partial list of arguments passed to the
  connection function; additional arguments may be added when
  @racket[dsn-connect] is called. The @racket[extensions] field
  contains additional information about a connection; for example,
  this library's testing framework uses it to store SQL dialect
  flags.

  Data sources can also be created using the
  @racket[postgresql-data-source], etc auxiliary functions.
}

@defproc[(dsn-connect [dsn (or/c symbol? data-source?)]
                      [#:dsn-file dsn-file path-string? (current-dsn-file)]
                      [arg any/c] ...
                      [#:<kw> kw-arg any/c] ...)
         connection?]{

  Makes a connection using the connection information associated with
  @racket[dsn] in @racket[dsn-file]. The given @racket[arg]s and
  @racket[kw-arg]s are added to those specified by @racket[dsn] to
  form the complete arguments supplied to the connect function.

  If @racket[dsn-file] does not exist, or if it contains no entry
  for @racket[dsn], an exception is raised. If @racket[dsn] is a
  @racket[data-source], then @racket[dsn-file] is ignored.

@examples/results[
[(put-dsn 'mydb
          (postgresql-data-source #:user "me"
                                  #:database "mydb" 
                                  #:password "icecream"))
 (void)]
[(dsn-connect 'me)
 (new connection%)]
[(dsn-connect 'me #:notice-handler (lambda (code msg) ....))
 (new connection%)]
]
}

@defparam[current-dsn-file x path-string?]{

  A parameter holding the location of the default DSN file. The
  initial value is a file located immediately within
  @racket[(find-system-path 'prefs-dir)].
}

@defproc[(get-dsn [dsn symbol?]
                  [default any/c #f]
                  [#:dsn-file dsn-file path-string? (current-dsn-file)])
         (or/c data-source? any/c)]{

  Returns the @racket[data-source] associated with @racket[dsn] in
  @racket[dsn-file].

  If @racket[dsn-file] does not exist, an exception is raised. If
  @racket[dsn-file] does not have an entry for @racket[dsn],
  @racket[default] is called if it is a function or returned
  otherwise.
}

@defproc[(put-dsn [dsn symbol?]
                  [ds (or/c data-source? #f)]
                  [#:dsn-file dsn-file path-string? (current-dsn-file)])
         void?]{

  Associates @racket[dsn] with the given data source @racket[ds] in
  @racket[dsn-file], replacing the previous association, if one
  exists.
}

@(define absent @italic{absent})

@deftogether[[
@defproc[(postgresql-data-source
           [#:user user string? @#,absent]
           [#:database database string? @#,absent]
           [#:server server string? @#,absent]
           [#:port port exact-positive-integer? @#,absent]
           [#:socket socket (or/c path-string? 'guess #f) @#,absent]
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
           [#:socket socket (or/c path-string? 'guess #f) @#,absent]
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
  arguments must be supplied when @racket[dsn-connect] is called.
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
