#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.ss"
          (for-label scheme/base scheme/class scheme/contract)
          (for-label "../main.ss"))

@(define-syntax-rule (qmeth meth) (method connection:query<%> meth))
@(define-syntax-rule (pmeth meth) (method connection:query/prepare<%> meth))

@title{Connections and Queries}
@(declare-exporting/this-package (main) ())

Connection methods are divided into three interfaces:
@scheme[connection:admin<%>], @scheme[connection:query<%>], and
@scheme[connection:query/prepare<%>]. The connections for PostgreSQL
and MySQL servers support all three interfaces.

This section also documents several auxiliary interfaces and data
types.

@section{Administrative methods}

@definterface[connection:admin<%> ()]{

This interface contains a connection's administrative methods.

@defmethod[(disconnect)
           void?]{
Disconnects from the server.
}
@defmethod[(connected?)
           boolean?]{
Indicates whether the connection is connected.
}

@defmethod[(get-system)
           (is-a/c dbsystem<%>)]{

Gets an object encapsulating information about the database system of
the connection.
}
}

@definterface[dbsystem<%> ()]{

This interface provides access to information about particular
database systems, their SQL dialects, and details about the database
package's implementation of their protocols.

@defmethod[(get-name)
           symbol?]{

Returns a symbol identifying the database system, such as
@scheme['postgresql] or @scheme['mysql].
}

@defmethod[(get-known-types)
           (listof symbol?)]{

Returns a list of the type names and aliases understood by connections
to this kind of database.
}
}

@section{Query API}

The database package implements a high-level, functional query
interface. Once connected, connection objects are relatively
stateless. When a query method is invoked, it either returns a result
or, if the query caused an error, raises an exception. Different query
methods impose different constraints on the query results and offer
different mechanisms for processing the results.

The query interface does not expose any low-level
machinery. Programmers who want cursors may use SQL-language cursors
via the @tt{DECLARE CURSOR}, @tt{MOVE}, and @tt{FETCH} statements if
they are available in their database's SQL dialect.

A @deftech{Statement} is either a string containing a single SQL
statement or a @scheme[StatementBinding] value returned by
@method[connection:prepare-query<%> bind-prepared-statement].

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


@definterface[connection:query<%> ()]{

This interface contains a connection's main query methods.

@defmethod[(query [stmt (unsyntax @techlink{Statement})])
           (unsyntax @techlink{QueryResult})]
@defmethod[(query-multiple [stmts (listof (unsyntax @techlink{Statement}))])
           (listof (unsyntax @techlink{QueryResult}))]{

  Executes queries, returning structures that describe the
  results. Unlike more specialized query methods,
  @method[connection:query<%> query-multiple] supports a mixture of
  recordset-returning queries and effect-only queries.

}

@defmethod[(exec [stmt (unsyntax @techlink{Statement})] ...)
           void?]{

  Executes SQL statements for effect and discards the result(s).
  Calling @method[connection:query<%> exec] on multiple statements at
  once may be more efficient than calling @method[connection:query<%>
  exec] multiple times on the statements individually.

  Example:
  @schemeinput[
    (send c #, @qmeth[exec] "create table the_numbers (n integer, name varchar)"
                 "insert into the_numbers (n, name) values (0, 'zero')")]

  @bold{PostgreSQL note}: The set of statements passed to
  @method[connection:query<%> exec] are executed within their own
  ``mini-transaction''; if any statement fails, the effects of all
  previous statements in the set are rolled back.

}

@defmethod[(query-rows [stmt (unsyntax @techlink{Statement})])
           (listof (vectorof _field))]{

  Executes a SQL query which must return a recordset. Returns the list
  of rows (as vectors) from the query.
}

@defmethod[(query-list [stmt (unsyntax @techlink{Statement})])
           (listof _field)]{

  Executes a SQL query which must return a recordset of exactly one
  column. Returns the list of values from the query.
}

@defmethod[(query-row [stmt (unsyntax @techlink{Statement})])
           (vectorof _field)]{

  Executes a SQL query which must return a recordset of exactly one
  row. Returns its (single) row result as a vector.
}

@defmethod[(query-maybe-row [stmt (unsyntax @techlink{Statement})])
           (or/c (vectorof _field) false/c)]{

  Like @method[connection:query<%> query-row], but the query may
  return zero rows; in that case, the method returns @scheme[#f].
}

@defmethod[(query-value [stmt (unsyntax @techlink{Statement})])
           _field]{

  Executes a SQL query which must return a recordset of exactly one
  column and exactly one row. Returns its single value result.
}

@defmethod[(query-maybe-value [stmt (unsyntax @techlink{Statement})])
           (or/c _field false/c)]{

  Like @method[connection:query<%> query-value], but the query may
  return zero rows; in that case, the method returns false.
}

@defmethod[(map [stmt (unsyntax @techlink{Statement})]
                [proc (_field _... -> _alpha)])
           (listof _alpha)]{

  Executes a SQL query and applies the given function to the contents
  of each row, returning a list of results.
}

@defmethod[(for-each [stmt (unsyntax @techlink{Statement})]
                     [proc (_field _... -> any)])
           void?]{

  Executes a SQL query and applies the given function to the contents
  of each row, discarding the results.
}

@defmethod[(mapfilter [stmt (unsyntax @techlink{Statement})]
                      [map-proc (_field _... -> _alpha)]
                      [filter-proc (_field _... -> boolean?)])
           (listof _alpha)]{

  Like @method[connection:query<%> map], but applies the map procedure
  (given first) to only those rows which satisfy the given predicate
  (given second).
}

@defmethod[(fold [stmt (unsyntax @techlink{Statement})]
                 [proc (_alpha _field _... -> _alpha)]
                 [init _alpha])
           _alpha]{

  Left fold over the results of the query.
}
}

Connections also support methods for preparing parameterized
queries. A parameterized query may be executed any number of times
with different values for its parameters.

A @deftech{PreparedStatement} is the result of a call to
@method[connection:query/prepare<%> prepare] or similar method.

The syntax of parameterized queries varies depending on the database
system.

PostgreSQL:
@verbatim{select * from the_numbers where num > $1;}

MySQL:
@verbatim{select * from the_numbers where num > ?;}

The following methods provide a convenient functional interface for
common uses of parameterized prepared statements:

@definterface[connection:query/prepare<%> (connection:query<%>)]{

@defmethod[(prepare [prep string?])
           (unsyntax @techlink{PreparedStatement})]
@defmethod[(prepare-multiple [preps (listof string?)])
           (listof (unsyntax @techlink{PreparedStatement}))]{

  Prepare parameterized queries. The resulting
  @techlink{PreparedStatement}s are tied to the connection object that
  prepared them; it is an error to use them with any other connection.

}

@defmethod[(bind-prepared-statement
            [pst (unsyntax @techlink{PreparedStatement})]
            [params (listof any/c)])
           (unsyntax @techlink{Statement})]{

  Fill in a parameterized prepared query with its parameters. The
  @techlink{PreparedStatement} must have been prepared with the same
  connection. The resulting @techlink{Statement} can be executed with
  @method[connection:query<%> query-multiple] or any of the high-level
  query methods, but it must be used with the same connection object
  that created it.

  @(examples/results
    [(let ([get-name-pst
            (send c #, @pmeth[prepare] "select name from the_numbers where n = $1")])
       (let ([get-name1
              (send c #, @pmeth[bind-prepared-statement] get-name-pst (list 1))]
             [get-name2
              (send c #, @pmeth[bind-prepared-statement] get-name-pst (list 2))])
         (send c #, @qmeth[query-multiple] (list get-name1 get-name2))))
     (list (make-Recordset (list (make-FieldInfo "name")) (list "one"))
           (make-Recordset (list (make-FieldInfo "name")) (list "two")))])
}

Each of the following methods prepares the parameterized SQL statement
for later execution and returns a procedure. The procedure accepts the
parameter values and executes the prepared statement, processing the
results like the corresponding query method.

A prepared-statement procedure may be executed any number of times.  It
is possible to prepare a statement that contains no parameters; the
resulting procedure should be called with zero arguments.

@defmethod[(prepare-exec [prep string?])
           (_param _... -> void?)]{

  Prepared version of @method[connection:query<%> exec]. Unlike
  @method[connection:query<%> exec],
  @method[connection:query/prepare<%> prepare-exec] permits only a
  single statement.

}

@defmethod[(prepare-query-rows [prep string?])
           (_param _... -> (listof (vectorof _field)))]{
  Prepared version of @method[connection:query<%> query-rows].
}

@defmethod[(prepare-query-list [prep string?])
           (_param _... -> (listof _field))]{
  Prepared version of @method[connection:query<%> query-list].
}

@defmethod[(prepare-query-row [prep string?])
           (_param _... -> (vectorof _field))]{
  Prepared version of @method[connection:query<%> query-row].
}
@defmethod[(prepare-query-maybe-row [prep string?])
           (_param _... -> (or/c (vectorof _field) false?))]{
  Prepared version of @method[connection:query<%> query-maybe-row].
}

@defmethod[(prepare-query-value [prep string?])
           (_param _... -> _field)]{
  Prepared version of @method[connection:query<%> query-value].
}

@defmethod[(prepare-query-maybe-value [prep string?])
           (_param _... -> (or/c _field false?))]{
  Prepared version of @method[connection:query<%> query-maybe-value].
}

@defmethod[(prepare-map [prep string?] [proc (_field _... -> _alpha)])
           (_param _... -> (listof _alpha))]{
  Prepared version of @method[connection:query<%> map].
}

@defmethod[(prepare-for-each [prep string?] [proc (_field _... -> void?)])
           (_param _... -> void?)]{
  Prepared version of @method[connection:query<%> for-each].
}

@defmethod[(prepare-mapfilter [prep string?]
                              [map-proc (_field _... -> _alpha)]
                              [filter-proc (_field _... -> boolean?)])
           (_param _... -> (listof _alpha))]{
  Prepared version of @method[connection:query<%> mapfilter].
}

@defmethod[(prepare-fold [prep string?]
                         [proc (_alpha _field _... -> _alpha)]
                         [init _alpha])
           (_param _... -> _alpha)]{
  Prepared version of @method[connection:query<%> fold].
}
}
