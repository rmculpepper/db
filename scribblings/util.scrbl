#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt")

@title[#:tag "util"]{Utilities}

@section[#:tag "connect-util"]{Connection utilities}

@(my-declare-exporting util/connect)

@defproc[(connection-generator [generator (-> connection?)])
         connection?]{

Creates a virtual connection that creates actual connections on demand
using the @racket[generator] function. A connection generator
encapsulates a mapping of threads to actual connections. When a query
function is called with a connection generator, the current thread's
associated actual connection is used to execute the query. If there is
no actual connection associated with the current thread, one is
obtained by calling @racket[generator]. When a thread dies, its
associated actual connection is disconnected.

When given a connection produced by @racket[connection-generator],
@racket[connected?] indicates whether there is an actual connection
associated with the current thread. Likewise, @racket[disconnect]
causes the current actual connection associated with the thread (if
there is one) to be disconnected, but the connection will be recreated
if a query function is executed.

Connections produced by @racket[connection-generator] may not be used
with the @racket[prepare] function. However, they may still be used to
execute parameterized queries expressed as strings or encapsulated via
@racket[statement-generator].
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
