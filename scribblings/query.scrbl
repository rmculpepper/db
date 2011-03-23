#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt")

@title[#:tag "query-api"]{Connections and Queries}

@(my-declare-exporting)

Connection procedures are divided into three APIs of generic
procedures: administrative procedures, query procedures, and prepared
query procedures. The connections for PostgreSQL and MySQL servers
support all three APIs, and both kinds of connections use the same
procedures.

This section also documents several auxiliary interfaces and data
types.

@section{Administrative API}

@defproc[(connection? [x any])
         boolean?]{

Predicate for connections.

}

@defproc[(disconnect [connection connection?])
         void?]{
Disconnects from the server.
}

@defproc[(connected? [connection connection?])
         boolean?]{
Indicates whether the connection is connected.
}

@defproc[(connection-dbsystem [connection connection?])
         dbsystem?]{

Gets an object encapsulating information about the database system of
the connection.
}

@defproc[(dbsystem? [x any/c])
         boolean?]{

Predicate for objects representing database systems.
}

@defproc[(dbsystem-name [sys dbsystem?])
         symbol?]{

Returns a symbol that names the database system. Currently one of the
following:
@itemize[
@item[@racket['postgresql]]
@item[@racket['mysql]]
@item[@racket['sqlite3]]
]
}


@section{Query API}

The database package implements a high-level, functional query
API. Once connected, connections are relatively stateless. When
a query procedure is invoked, it either returns a result or, if the
query caused an error, raises an exception. Different query procedures
impose different constraints on the query results and offer different
mechanisms for processing the results.


@subsection{Simple queries}

The simple query API consists of a set of functions specialized to
various types of queries. For example, @racket[query-value] is
specialized to queries that return a recordset of exactly one column
and exactly one row.

This API also provides a simple interface to parameterized queries:
the statement's parameters are simply given after the SQL
statement. If any parameter values are given, the SQL statement must
be either a string or prepared-statement, not a statement-binding.

@defproc[(query-exec [connection connection?]
                     [stmt (or/c string? prepared-statement? statement-binding?)]
                     [arg any/c] ...)
         void?]{

  Executes a SQL statement for effect.
}

@defproc[(query-rows [connection connection?]
                     [stmt (or/c string? prepared-statement? statement-binding?)]
                     [arg any/c] ...)
         (listof (vectorof _field))]{

  Executes a SQL query, which must return a recordset. Returns the
  list of rows (as vectors) from the query.
}

@defproc[(query-list [connection connection?]
                     [stmt (or/c string? prepared-statement? statement-binding?)]
                     [arg any/c] ...)
         (listof _field)]{

  Executes a SQL query, which must return a recordset of exactly one
  column. Returns the list of values from the query.
}

@defproc[(query-row [connection connection?]
                    [stmt (or/c string? prepared-statement? statement-binding?)]
                    [arg any/c] ...)
         (vectorof _field)]{

  Executes a SQL query, which must return a recordset of exactly one
  row. Returns its (single) row result as a vector.
}

@defproc[(query-maybe-row [connection connection?]
                          [stmt (or/c string? prepared-statement? statement-binding?)]
                          [arg any/c] ...)
         (or/c (vectorof _field) false/c)]{

  Like @scheme[query-row], but the query may return zero rows; in that
  case, @scheme[#f] is returned.
}

@defproc[(query-value [connection connection?]
                      [stmt (or/c string? prepared-statement? statement-binding?)]
                      [arg any/c] ...)
         _field]{

  Executes a SQL query, which must return a recordset of exactly one
  column and exactly one row. Returns its single value result.
}

@defproc[(query-maybe-value [connection connection?]
                            [stmt (or/c string? prepared-statement? statement-binding?)]
                            [arg any/c] ...)
         (or/c _field false/c)]{

  Like @scheme[query-value], but the query may return zero rows; in
  that case, @scheme[#f] is returned.
}


@subsection{General query support}

A @deftech{Statement} is either a string containing a single
non-parameterized SQL statement or a statement-binding value
returned by @scheme[bind-prepared-statement].

A @deftech{QueryResult} is either a @scheme[SimpleResult] or a
@scheme[Recordset].

@defstruct[SimpleResult
           ([command string?])]{

Represents the result of a SQL statement that does not return a
relation.
}

@defstruct[Recordset
           ([info (listof FieldInfo?)]
            [data (listof (vectorof any/c))])]{

Represents the result of SQL statement that results in a relation,
such as a @tt{SELECT} query.
}

@defstruct[FieldInfo
           ([name string?])]{

Represents the name of a column.
}

@deftogether[[
@defproc[(query [connection connection?]
                [stmt (unsyntax @techlink{Statement})])
         #, @tech{QueryResult}]
@defproc[(query-multiple [connection connection?]
                         [stmts (listof #, @tech{Statement})])
         (listof #, @techlink{QueryResult})]]]{

  Executes queries, returning structures that describe the
  results. Unlike the more specialized query procedures,
  @scheme[query-multiple] supports a mixture of recordset-returning
  queries and effect-only queries.

}

@defproc[(query-exec* [connection connection?]
                      [stmt #, @tech{Statement}] ...)
         void?]{

  Executes SQL statements for effect and discards the result(s).
  Calling @scheme[query-exec*] on multiple statements at once may be
  more efficient than calling @scheme[query-exec] multiple times on
  the statements individually.

  Example:
  @schemeinput[
    (query-exec* c
      "create table the_numbers (n integer, name varchar)"
      "insert into the_numbers (n, name) values (0, 'zero')")]

  @bold{PostgreSQL note}: The set of statements passed to
  @scheme[query-exec*] are executed within their own
  ``mini-transaction''; if any statement fails, the effects of all
  previous statements in the set are rolled back.

}

@;{

@defproc[(query-map [connection connection?]
                    [stmt #, @tech{Statement}]
                    [proc (_field _... -> _alpha)])
         (listof _alpha)]{

  Executes a SQL query and applies the given procedure to the contents
  of each row, returning a list of results. The arity of @scheme[proc]
  must include the number of columns returned by the query.
}

@defproc[(query-for-each [connection connection?]
                         [stmt #, @tech{Statement}]
                         [proc (_field _... -> any)])
         void?]{

  Executes a SQL query and applies the given function to the contents
  of each row, discarding the results. The arity of @scheme[proc] must
  include the number of columns returned by the query.
}

@defproc[(query-mapfilter [connection connection?]
                          [stmt #, @tech{Statement}]
                          [map-proc (_field _... -> _alpha)]
                          [filter-proc (_field _... -> boolean?)])
         (listof _alpha)]{

  Like @scheme[query-map], but applies @scheme[map-proc] to only those
  rows which satisfy @scheme[filter-proc]. The arities of both
  @scheme[map-proc] and @scheme[filter-proc] must include the number
  of columns returned by the query.
}
}

@defproc[(query-fold [connection connection?]
                     [stmt #, @tech{Statement}]
                     [fold-proc (_alpha _field _... -> _alpha)]
                     [init _alpha])
         _alpha]{

  Left fold over the results of the query. The arity of
  @scheme[fold-proc] must include a number one greater than the number
  of columns returned by the query.
}

@subsection{Prepared statements}

Connections also support functions for preparing parameterized
queries. A parameterized query may be executed any number of times
with different values for its parameters.

A @deftech{PreparedStatement} is the result of a call to
@scheme[prepare] or @scheme[prepare-multiple].

The syntax of parameterized queries varies depending on the database
system.

PostgreSQL:
@verbatim{select * from the_numbers where num > $1;}

MySQL:
@verbatim{select * from the_numbers where num > ?;}


@deftogether[[
@defproc[(prepare [connection connection?]
                  [prep string?])
         #, @tech{PreparedStatement}]
@defproc[(prepare-multiple [connection connection?]
                           [preps (listof string?)])
         (listof #, @tech{PreparedStatement})]]]{

  Prepares parameterized queries. The resulting
  @tech{PreparedStatement}s are tied to the connection that prepared
  them; it is an error to use them with any other connection. (The
  @tech{PreparedStatement} holds its connection link weakly; a
  reference to a @tech{PreparedStatement} will not keep a connection
  from being garbage collected.)

}

@defproc[(bind-prepared-statement
            [pst #, @tech{PreparedStatement}]
            [params (listof any/c)])
         #, @tech{Statement}]{

  Fills in the parameters of a parameterized prepared query. The
  resulting @tech{Statement} can be executed with @scheme[query],
  @scheme[query-multiple], or any of the other query procedures,
  but it must be used with the same connection that created it.

  @(examples/results
    [(let* ([get-name-pst
            (prepare c "select name from the_numbers where n = $1")]
            [get-name1
             (bind-prepared-statement get-name-pst (list 1))]
            [get-name2
             (bind-prepared-statement get-name-pst (list 2))])
       (list (query-value c get-name1)
             (query-value c get-name2)))
     (list "one" "two")])
}

@defproc[(prepared-statement? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a prepared statement created by
  @racket[prepare] or @racket[prepare-multiple], @racket[#f] otherwise.
}

@subsection{Prepared Queries as Procedures}

The following procedures prepare the parameterized SQL statement for
later execution and and encapsulate it as a procedure. The
prepared-statement procedure accepts the parameter values and executes
the prepared statement, processing the results like the corresponding
query procedures.

A prepared-statement procedure may be executed any number of times.  It
is possible to prepare a statement that contains no parameters; the
resulting procedure should be called with zero arguments.

Prepared-statement procedures hold their associated connections
strongly.

@defproc[(prepare-query-exec [connection connection?]
                             [prep string?])
         (_param _... -> void?)]{

  Prepared version of @scheme[query-exec].
}

@defproc[(prepare-query-rows [connection connection?]
                             [prep string?])
         (_param _... -> (listof (vectorof _field)))]{

  Prepared version of @scheme[query-rows].
}

@defproc[(prepare-query-list [connection connection?]
                             [prep string?])
         (_param _... -> (listof _field))]{

  Prepared version of @scheme[query-list].
}

@defproc[(prepare-query-row [connection connection?]
                            [prep string?])
         (_param _... -> (vectorof _field))]{

  Prepared version of @scheme[query-row].
}

@defproc[(prepare-query-maybe-row [connection connection?]
                                  [prep string?])
         (_param _... -> (or/c (vectorof _field) false?))]{

  Prepared version of @scheme[query-maybe-row].
}

@defproc[(prepare-query-value [connection connection?]
                              [prep string?])
         (_param _... -> _field)]{

  Prepared version of @scheme[query-value].
}

@defproc[(prepare-query-maybe-value [connection connection?]
                                    [prep string?])
         (_param _... -> (or/c _field false?))]{

  Prepared version of @scheme[query-maybe-value].
}

@;{
@defproc[(prepare-query-map [connection connection?]
                            [prep string?]
                            [proc (_field _... -> _alpha)])
         (_param _... -> (listof _alpha))]{

  Prepared version of @scheme[query-map].
}

@defproc[(prepare-query-for-each [connection connection?]
                                 [prep string?]
                                 [proc (_field _... -> void?)])
         (_param _... -> void?)]{

  Prepared version of @scheme[query-for-each].
}

@defproc[(prepare-query-mapfilter [connection connection?]
                                  [prep string?]
                                  [map-proc (_field _... -> _alpha)]
                                  [filter-proc (_field _... -> boolean?)])
         (_param _... -> (listof _alpha))]{

  Prepared version of @scheme[query-mapfilter].
}
}

@defproc[(prepare-query-fold [connection connection?]
                             [prep string?]
                             [proc (_alpha _field _... -> _alpha)]
                             [init _alpha])
         (_param _... -> _alpha)]{

  Prepared version of @scheme[query-fold].
}
