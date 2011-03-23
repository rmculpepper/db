#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt")

@title[#:tag "query-api"]{Queries}

@(my-declare-exporting)

The database package implements a high-level functional query API,
unlike many other database libraries, which present a stateful,
iteration-based interface to queries. When a query function is
invoked, it either returns a result or, if the query caused an error,
raises an exception. Different query functions impose different
constraints on the query results and offer different mechanisms for
processing the results.

In most cases, a query error does not cause the connection to be
disconnected. Specifically, the following kinds of errors should never
cause a connection to be disconnected:
@itemize[
@item{SQL syntax errors, including references to undefined tables,
  columns, or operations, etc}
@item{violations of a specialized query function's expectations, such
  as @racket[query-value] getting a recordset with multiple columns}
@item{supplying the wrong number of parameters to a prepared query,
  executing a prepared query with the wrong connection, etc}
]
The following kinds of errors may cause a connection to be
disconnected:
@itemize[
@item{changing communication settings, such as changing the encoding
  to anything other than UTF-8}
@item{communication failures and internal errors in the library}
]

@section{Simple queries}

The simple query API consists of a set of functions specialized to
various types of queries. For example, @racket[query-value] is
specialized to queries that return a recordset of exactly one column
and exactly one row.

This API also provides a simple interface to parameterized queries:
the statement's parameters are given after the SQL statement. If any
parameter values are given, the SQL statement must be either a string
or prepared statement, not a statement-binding.

@defproc[(query-exec [connection connection?]
                     [stmt (or/c string? prepared-statement? statement-binding?)]
                     [arg any/c] ...)
         void?]{

  Executes a SQL statement for effect.

@examples/results[
[(query-exec c "insert into some_table values (1, 'a')")
 (void)]
[(query-exec c "delete from some_table where n = $1" 42)
 (void)]
]
}

@defproc[(query-rows [connection connection?]
                     [stmt (or/c string? prepared-statement? statement-binding?)]
                     [arg any/c] ...)
         (listof (vectorof _field))]{

  Executes a SQL query, which must produce a recordset, and returns the
  list of rows (as vectors) from the query.

@examples/results[
[(query-rows c "select n, s from some_table where n = $1" 42)
 (list (vector 42 "the answer to life, the universe, and everything"))]
[(query-rows c "select 17")
 (list (vector 17))]
]
}

@defproc[(query-list [connection connection?]
                     [stmt (or/c string? prepared-statement? statement-binding?)]
                     [arg any/c] ...)
         (listof _field)]{

  Executes a SQL query, which must produce a recordset of exactly one
  column, and returns the list of values from the query.

@examples/results[
[(query-list c "select n from some_table where n < 2")
 (list 0 1)]
[(query-list c "select 'hello'")
 (list "hello")]
]
}

@defproc[(query-row [connection connection?]
                    [stmt (or/c string? prepared-statement? statement-binding?)]
                    [arg any/c] ...)
         (vectorof _field)]{

  Executes a SQL query, which must produce a recordset of exactly one
  row, and returns its (single) row result as a vector.

@examples/results[
[(query-row c "select n, s from some_table where n = $1" 42)
 (vector 42 "the answer to life, the universe, and everything")]
[(query-row c "select 17")
 (vector 17)]
]
}

@defproc[(query-maybe-row [connection connection?]
                          [stmt (or/c string? prepared-statement? statement-binding?)]
                          [arg any/c] ...)
         (or/c (vectorof _field) false/c)]{

  Like @scheme[query-row], but the query may produce zero rows; in
  that case, @scheme[#f] is returned.

@examples/results[
[(query-maybe-row c "select n, s from some_table where n = $1" 43)
 #f]
[(query-maybe-row c "select 17")
 (vector 17)]
]
}

@defproc[(query-value [connection connection?]
                      [stmt (or/c string? prepared-statement? statement-binding?)]
                      [arg any/c] ...)
         _field]{

  Executes a SQL query, which must produce a recordset of exactly one
  column and exactly one row, and returns its single value result.

@examples/results[
[(query-value c "select timestamp 'epoch'")
 (sql-timestamp 1970 1 1 0 0 0 0 #f)]
[(query-value c "select s from some_table where n = $1" 42)
 "the answer to life, the universe, and everything"]
]
}

@defproc[(query-maybe-value [connection connection?]
                            [stmt (or/c string? prepared-statement? statement-binding?)]
                            [arg any/c] ...)
         (or/c _field false/c)]{

  Like @scheme[query-value], but the query may produce zero rows; in
  that case, @scheme[#f] is returned.

@examples/results[
[(query-value c "select s from some_table where n = $1" 43)
 #f]
[(query-value c "select 17")
 17]
]
}


@section{General query support}

A statement is either a string containing a single non-parameterized
SQL statement or a statement-binding value returned by
@scheme[bind-prepared-statement].

A query result is either a @scheme[simple-result] or a
@scheme[recordset].

@defstruct*[simple-result
            ([command (or/c #f string?)])]{

Represents the result of a SQL statement that does not return a
relation, such as a @tt{INSERT} or @tt{DELETE} statement.
}

@defstruct*[recordset
            ([info (listof field-info?)]
             [data (listof (vectorof any/c))])]{

Represents the result of SQL statement that results in a relation,
such as a @tt{SELECT} query.
}

@defstruct*[field-info
            ([name string?])]{

Represents the name of a column.
}

@deftogether[[
@defproc[(query [connection connection?]
                [stmt (or/c string? statement-binding?)])
         (or/c simple-result? recordset?)]
@defproc[(query-multiple [connection connection?]
                         [stmts (listof (or/c string? statement-binding?))])
         (listof (or/c simple-result? recordset?))]]]{

  Executes queries, returning structures that describe the
  results. Unlike the more specialized query functions,
  @scheme[query-multiple] supports a mixture of recordset-returning
  queries and effect-only queries.
}

@defproc[(query-exec* [connection connection?]
                      [stmt (or/c string? statement-binding?)] ...)
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

@defproc[(query-fold [connection connection?]
                     [stmt (or/c string? statement-binding?)]
                     [fold-proc (_alpha _field _... -> _alpha)]
                     [init _alpha])
         _alpha]{

  Left fold over the results of the query. The arity of
  @scheme[fold-proc] must include a number one greater than the number
  of columns returned by the query.
}

@section{Prepared statements}

This package also includes functions for preparing parameterized
queries. A parameterized query may be executed any number of times
with different values for its parameters.

A @deftech{prepared statement} is the result of a call to
@scheme[prepare] or @scheme[prepare-multiple].

The syntax of parameterized queries varies depending on the database
system. For example:

PostgreSQL: @verbatim{select * from the_numbers where num > $1;}

MySQL: @verbatim{select * from the_numbers where num > ?;}

SQLite supports both syntaxes and possibly others.

@deftogether[[
@defproc[(prepare [connection connection?]
                  [prep string?])
         prepared-statement?]
@defproc[(prepare-multiple [connection connection?]
                           [preps (listof string?)])
         (listof prepared-statement?)]]]{

  Prepares parameterized queries. The resulting @tech{prepared
  statement}s are tied to the connection that prepared them; it is an
  error to use them with any other connection. (The prepared statement
  holds its connection link weakly; a reference to a prepared
  statement will not keep a connection from being garbage collected.)
}

@defproc[(bind-prepared-statement
            [pst prepared-statement?]
            [params (listof any/c)])
         statement-binding?]{

  Fills in the parameters of a parameterized prepared query. The
  resulting statement can be executed with @scheme[query],
  @scheme[query-multiple], or any of the other query functions, but it
  must be used with the same connection that created it.

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

@defproc[(statement-binding? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a statement created by
  @racket[bind-prepared-statement], @racket[#f] otherwise.
}

@section{Prepared queries as functions}

The following functions prepare a parameterized SQL statement for
later execution and and encapsulate it as a function. The
prepared-statement function accepts the parameter values and executes
the prepared statement, processing the results like the corresponding
query functions.

A prepared-statement function may be executed any number of times.  It
is possible to prepare a statement that contains no parameters; the
resulting function should be called with zero arguments.

Prepared-statement functions hold their associated connections
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

@defproc[(prepare-query-fold [connection connection?]
                             [prep string?]
                             [proc (_alpha _field _... -> _alpha)]
                             [init _alpha])
         (_param _... -> _alpha)]{

  Prepared version of @scheme[query-fold].
}
