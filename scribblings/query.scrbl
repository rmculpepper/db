#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt"
          (for-label scheme/base scheme/class scheme/contract)
          (for-label "../main.rkt"))

@(define-syntax-rule (qmeth meth) (method connection:query<%> meth))
@(define-syntax-rule (pmeth meth) (method connection:query/prepare<%> meth))

@title[#:tag "query-api"]{Connections and Queries}
@(declare-exporting/this-package (main) ())

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

Returns a symbol that names the database system. Currently either
@scheme['postrgresql] or @scheme['mysql].
}


@section{Query API}

The database package implements a high-level, functional query
API. Once connected, connections are relatively stateless. When
a query procedure is invoked, it either returns a result or, if the
query caused an error, raises an exception. Different query procedures
impose different constraints on the query results and offer different
mechanisms for processing the results.

@;{
The query API does not expose any low-level machinery. Programmers who
want cursors may use SQL-language cursors via the @tt{DECLARE CURSOR},
@tt{MOVE}, and @tt{FETCH} statements if they are available in their
database's SQL dialect.
}

A @deftech{Statement} is either a string containing a single SQL
statement or a @scheme[StatementBinding] value returned by
@scheme[bind-prepared-statement].

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

@;{
@defproc[(connection:query? [x any])
         boolean?]{

Predicate for connections implementing the query API.
}
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

@defproc[(query-exec [connection connection?]
                     [stmt #, @tech{Statement}] ...)
         void?]{

  Executes SQL statements for effect and discards the result(s).
  Calling @scheme[query-exec] on multiple statements at once may be
  more efficient than calling @scheme[query-exec] multiple times on
  the statements individually.

  Example:
  @schemeinput[
    (query-exec c
      "create table the_numbers (n integer, name varchar)"
      "insert into the_numbers (n, name) values (0, 'zero')")]

  @bold{PostgreSQL note}: The set of statements passed to
  @scheme[query-exec] are executed within their own
  ``mini-transaction''; if any statement fails, the effects of all
  previous statements in the set are rolled back.

}

@defproc[(query-rows [connection connection?]
                     [stmt #, @tech{Statement}])
         (listof (vectorof _field))]{

  Executes a SQL query, which must return a recordset. Returns the
  list of rows (as vectors) from the query.
}

@defproc[(query-list [connection connection?]
                     [stmt #, @tech{Statement}])
         (listof _field)]{

  Executes a SQL query, which must return a recordset of exactly one
  column. Returns the list of values from the query.
}

@defproc[(query-row [connection connection?]
                    [stmt #, @tech{Statement}])
         (vectorof _field)]{

  Executes a SQL query, which must return a recordset of exactly one
  row. Returns its (single) row result as a vector.
}

@defproc[(query-maybe-row [connection connection?]
                          [stmt #, @tech{Statement}])
         (or/c (vectorof _field) false/c)]{

  Like @scheme[query-row], but the query may return zero rows; in that
  case, @scheme[#f] is returned.
}

@defproc[(query-value [connection connection?]
                      [stmt #, @tech{Statement}])
         _field]{

  Executes a SQL query, which must return a recordset of exactly one
  column and exactly one row. Returns its single value result.
}

@defproc[(query-maybe-value [connection connection?]
                            [stmt #, @tech{Statement}])
         (or/c _field false/c)]{

  Like @scheme[query-value], but the query may return zero rows; in
  that case, @scheme[#f] is returned.
}

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

@defproc[(query-fold [connection connection?]
                     [stmt #, @tech{Statement}]
                     [fold-proc (_alpha _field _... -> _alpha)]
                     [init _alpha])
         _alpha]{

  Left fold over the results of the query. The arity of
  @scheme[fold-proc] must include a number one greater than the number
  of columns returned by the query.
}

@section{Prepared Query API}

Connections also support procedures for preparing parameterized
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

@;{
The following procedures provide a convenient functional interface for
common uses of parameterized prepared statements:
}
@;{
@defproc[(connection:query/prepare? [x any])
         boolean?]{

Predicate for connections supporting the prepared query API.
}
}

@deftogether[[
@defproc[(prepare [connection connection?]
                  [prep string?])
         #, @tech{PreparedStatement}]
@defproc[(prepare-multiple [connection connection?]
                           [preps (listof string?)])
         (listof #, @tech{PreparedStatement})]]]{

  Prepare parameterized queries. The resulting
  @tech{PreparedStatement}s are tied to the connection that prepared
  them; it is an error to use them with any other connection.

  The @tech{PreparedStatement} holds its connection link weakly; a
  reference to a @tech{PreparedStatement} will not keep a connection
  from being garbage collected.

}

@defproc[(bind-prepared-statement
            [pst #, @tech{PreparedStatement}]
            [params (listof any/c)])
         #, @tech{Statement}]{

  Fill in a parameterized prepared query with its parameters. The
  resulting @tech{Statement} can be executed with @scheme[query],
  @scheme[query-multiple], or any of the high-level query procedures,
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

The following procedures prepares the parameterized SQL statement for
later execution and returns a procedure. The prepared-statement
procedure accepts the parameter values and executes the prepared
statement, processing the results like the corresponding query
procedures.

A prepared-statement procedure may be executed any number of times.  It
is possible to prepare a statement that contains no parameters; the
resulting procedure should be called with zero arguments.

Prepared-statement procedures hold their associated connections
strongly.

@defproc[(prepare-query-exec [connection connection?]
                             [prep string?])
         (_param _... -> void?)]{

  Prepared version of @scheme[query-exec]. Unlike @scheme[query-exec],
  @scheme[prepare-query-exec] permits only a single statement.

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

@defproc[(prepare-query-fold [connection connection?]
                             [prep string?]
                             [proc (_alpha _field _... -> _alpha)]
                             [init _alpha])
         (_param _... -> _alpha)]{

  Prepared version of @scheme[query-fold].

}
