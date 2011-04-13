#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt")

@title[#:tag "util"]{Utilities}

@section[#:tag "connect-util"]{Connection utilities}

@(my-declare-exporting util/connect)

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
