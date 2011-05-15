#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt")

@title[#:tag "util"]{Utilities}

This section describes bindings that are not provided by
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
  Represents an OpenGIS @tt{POINT}.
}
@defstruct*[line-string
            ([points (listof point?)])]{
  Represents an OpenGIS @tt{LINESTRING}.
}
@defstruct*[polygon
            ([exterior linear-ring?]
             [interior (listof linear-ring?)])]{
  Represents an OpenGIS @tt{POLYGON}.
}
@defstruct*[multi-point ([elements (listof point?)])]{
  Represents an OpenGIS @tt{MULTIPOINT}, a collection of points.
}
@defstruct*[multi-line-string ([elements (listof line-string?)])]{
  Represents an OpenGIS @tt{MULTILINESTRING}, a collection of line-strings.
}
@defstruct*[multi-polygon ([elements (listof polygon?)])]{
  Represents an OpenGIS @tt{MULTIPOLYGON}, a collection of polygons.
}
@defstruct*[geometry-collection ([elements (listof geometry?)])]{
  Represents an OpenGIS @tt{GEOMETRYCOLLECTION}, a collection of
  arbitrary geometric values.
}

@defproc[(geometry? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[point],
  @racket[line-string], @racket[polygon], @racket[multi-point],
  @racket[multi-line-string], @racket[multi-polygon], or
  @racket[geometry-collection]; @racket[#f] othewise.
}

@defproc[(line? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[line-string]
  consisting of exactly two points, which are distinct; @racket[#f]
  otherwise.
}

@defproc[(linear-ring? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[line-string] whose
  first and last points are equal; @racket[#f] otherwise.
}

@defproc[(geometry->wkb [g geometry?]
                        [#:big-endian? big-endian? (system-big-endian?)])
         bytes?]{

  Returns the Well-Known Binary (WKB) encoding of the geometric value
  @racket[g]. The @racket[big-endian?] argument determines the byte
  order used, but the WKB format includes byte-order markers, so a
  robust client should accept either encoding.
}

@defproc[(wkb->geometry [b bytes?])
         geometry?]{

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
