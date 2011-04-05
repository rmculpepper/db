#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
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

@bold{Errors} In most cases, a query error does not cause the
connection to be disconnected. Specifically, the following kinds of
errors should never cause a connection to be disconnected:
@itemize[
@item{SQL syntax errors, including references to undefined tables,
  columns, or operations, etc}
@item{violations of a specialized query function's expectations, such
  as @racket[query-value] getting a recordset with multiple columns}
@item{supplying the wrong number or wrong types of parameters to a
  prepared query, executing a prepared query with the wrong
  connection, etc}
]
The following kinds of errors may cause a connection to be
disconnected:
@itemize[
@item{changing communication settings, such as changing the
  connection's character encoding}
@item{communication failures and internal errors in the library}
]

@bold{Character encoding} This library is designed to interact with
database systems using the UTF-8 character encoding. The connection
functions attempt to negotiate UTF-8 communication at the beginning of
every connection, but some systems also allow the character encoding
to be changed via SQL commands. If this happens, the client might be
unable to reliably communicate with the database, and data might get
corrupted in transmission. @emph{Avoid changing a connection's
character set encoding.}

@bold{Synchronization} Connections are internally synchronized: it is
safe to perform concurrent queries on the same connection object from
different threads. @emph{Connections are not kill-safe:} killing a
thread that is using a connection---or shutting down the connection's
managing custodian---may leave the connection in a damaged state,
causing future operations to return garbage, raise errors, or block
indefinitely.

@section{Statements}

All queries require both a connection and a @deftech{statement}, which
is one of the following:
@itemlist[
@item{a string containing a single SQL statement, possibly with
parameters}
@item{a @tech{prepared statement} produced by @racket[prepare] or
@racket[prepare-multiple]}
@item{a @tech{statement generator} produced by
@racket[statement-generator]}
@item{a statement-binding value produced by
@racket[bind-prepared-statement]}
]

@defproc[(statement? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @tech{statement}, @racket[#f]
  otherwise.

  Equivalent to the following:
  @racketblock[
    (or (string? x)
        (prepared-statement? x)
        (statement-generator? x)
        (statement-binding? x))
  ]
}


@section{Simple queries}

The simple query API consists of a set of functions specialized to
various types of queries. For example, @racket[query-value] is
specialized to queries that return a recordset of exactly one column
and exactly one row.

If a statement takes parameters, the parameter values are given as
additional arguments immediately after the SQL statement. Only a
statement given as a string, prepared statement, or statement
generator can be given ``inline'' parameters; if the statement is a
statement-binding, no inline parameters are permitted.

@defproc[(query-exec [connection connection?]
                     [stmt statement?]
                     [arg any/c] ...)
         void?]{

  Executes a SQL statement for effect.

@examples/results[
[(query-exec c "insert into some_table values (1, 'a')")
 (void)]
[(query-exec pgc "delete from some_table where n = $1" 42)
 (void)]
]
}

@defproc[(query-rows [connection connection?]
                     [stmt statement?]
                     [arg any/c] ...)
         (listof (vectorof _field))]{

  Executes a SQL query, which must produce a recordset, and returns the
  list of rows (as vectors) from the query.

@examples/results[
[(query-rows pgc "select * from the_numbers where n = $1" 2)
 (list (vector 2 "company"))]
[(query-rows c "select 17")
 (list (vector 17))]
]
}

@defproc[(query-list [connection connection?]
                     [stmt statement?]
                     [arg any/c] ...)
         (listof _field)]{

  Executes a SQL query, which must produce a recordset of exactly one
  column, and returns the list of values from the query.

@examples/results[
[(query-list c "select n from the_numbers where n < 2")
 (list 0 1)]
[(query-list c "select 'hello'")
 (list "hello")]
]
}

@defproc[(query-row [connection connection?]
                    [stmt statement?]
                    [arg any/c] ...)
         (vectorof _field)]{

  Executes a SQL query, which must produce a recordset of exactly one
  row, and returns its (single) row result as a vector.

@examples/results[
[(query-row myc "select * from the_numbers where n = ?" 2)
 (vector 2 "company")]
[(query-row c "select 17")
 (vector 17)]
]
}

@defproc[(query-maybe-row [connection connection?]
                          [stmt statement?]
                          [arg any/c] ...)
         (or/c (vectorof _field) false/c)]{

  Like @racket[query-row], but the query may produce zero rows; in
  that case, @racket[#f] is returned.

@examples/results[
[(query-maybe-row pgc "select * from the_numbers where n = $1" 100)
 #f]
[(query-maybe-row c "select 17")
 (vector 17)]
]
}

@defproc[(query-value [connection connection?]
                      [stmt statement?]
                      [arg any/c] ...)
         _field]{

  Executes a SQL query, which must produce a recordset of exactly one
  column and exactly one row, and returns its single value result.

@examples/results[
[(query-value pgc "select timestamp 'epoch'")
 (sql-timestamp 1970 1 1 0 0 0 0 #f)]
[(query-value pgc "select s from the_numbers where n = $1" 3)
 "a crowd"]
]
}

@defproc[(query-maybe-value [connection connection?]
                            [stmt statement?]
                            [arg any/c] ...)
         (or/c _field false/c)]{

  Like @racket[query-value], but the query may produce zero rows; in
  that case, @racket[#f] is returned.

@examples/results[
[(query-value myc "select s from some_table where n = ?" 100)
 #f]
[(query-value c "select 17")
 17]
]
}


@section{General query support}

A general query result is either a @racket[simple-result] or a
@racket[recordset].

@defstruct*[simple-result
            ([info (listof pair?)])]{

Represents the result of a SQL statement that does not return a
relation, such as an @tt{INSERT} or @tt{DELETE} statement.
The @racket[info] field is an association list containing arbitrary
information about the SQL statement's execution. Do not rely on the
contents of the @racket[info] field; it varies based on database
system and may change in future versions of this library.
}

@defstruct*[recordset
            ([info (listof field-info?)]
             [data (listof (vectorof any/c))])]{

Represents the result of SQL statement that results in a relation,
such as a @tt{SELECT} query.
}

@defstruct*[field-info
            ([name string?]
             [info (listof pair?)])]{

Represents a column in a recordset. The @racket[name] field is the
name of the column, and the @racket[info] field is an association list
containing arbitrary additional information about the column. Do not
rely on the contents of the @racket[info] field; it varies based on
database system and may change in future versions of this library.
}

@defproc[(query [connection connection?]
                [stmt statement?]
                [arg any/c] ...)
         (or/c simple-result? recordset?)]{

  Executes a query, returning a structure that describes the
  results. Unlike the more specialized query functions, @racket[query]
  supports both recordset-returning and effect-only queries.
}

@defproc[(query-multiple [connection connection?]
                         [stmts (listof (or/c string? statement-binding?))])
         (listof (or/c simple-result? recordset?))]{

  Executes queries, returning structures that describe the
  results. Unlike the more specialized query functions,
  @racket[query-multiple] supports a mixture of recordset-returning
  and effect-only queries. Inline parameter arguments are not
  supported; parameter binding must be done explicitly.
}

@defproc[(query-exec* [connection connection?]
                      [stmt (or/c string? statement-binding?)] ...)
         void?]{

  Executes SQL statements for effect and discards the result(s).
  Calling @racket[query-exec*] on multiple statements at once may be
  more efficient than calling @racket[query-exec] multiple times on
  the statements individually. Inline parameter arguments ae not
  supported; parameter binding must be done explicitly.

  Example:
  @racketinput[
    (query-exec* c
      "create table the_numbers (n integer, description varchar)"
      "insert into the_numbers values (0, 'nothing')"
      "insert into the_numbers values (1, 'the loneliest number')")
  ]

  @bold{PostgreSQL note}: The list of statements passed to
  @racket[query-exec*] are executed within their own
  ``mini-transaction''; if any statement fails, the effects of all
  previous statements in the list are rolled back.
}

@defproc[(query-fold [connection connection?]
                     [stmt (or/c string? statement-binding?)]
                     [fold-proc (_alpha _field _... -> _alpha)]
                     [init _alpha])
         _alpha]{

  Left fold over the results of the query. The arity of
  @racket[fold-proc] must include a number one greater than the number
  of columns returned by the query. Inline parameter arguments ae not
  supported; parameter binding must be done explicitly.
}

@section{Prepared statements}

This package also includes functions for preparing parameterized
queries. A parameterized query may be executed any number of times
with different values for its parameters.

A @deftech{prepared statement} is the result of a call to
@racket[prepare] or @racket[prepare-multiple].

The syntax of parameterized queries varies depending on the database
system. For example:

PostgreSQL: @verbatim{select * from the_numbers where num > $1;}

MySQL: @verbatim{select * from the_numbers where num > ?;}

SQLite supports both syntaxes.

@deftogether[[
@defproc[(prepare [connection connection?]
                  [stmt (or/c string? statement-generator?)])
         prepared-statement?]
@defproc[(prepare-multiple [connection connection?]
                           [stmts (listof (or/c string? statement-generator?))])
         (listof prepared-statement?)]]]{

  Prepares parameterized queries. The resulting @tech{prepared
  statement}s are tied to the connection that prepared them; it is an
  error to use them with any other connection. (The prepared statement
  holds its connection link weakly; a reference to a prepared
  statement will not keep a connection from being garbage collected.)
}

@defproc[(prepared-statement? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a prepared statement created by
  @racket[prepare] or @racket[prepare-multiple], @racket[#f] otherwise.
}

@defproc[(prepared-statement-parameter-types [pst prepared-statement?])
         (listof symbol?)]{

  Returns a list of symbols, one for each of the prepared statement's
  parameters, identifying the types of the parameters.
}

@defproc[(prepared-statement-result-types [pst prepared-statement?])
         (or/c (listof symbol?) #f)]{

  If @racket[pst] is a recordset-producing statement (eg,
  @tt{SELECT}), returns a list of symbols, identifying the SQL types
  of the result columns. If @racket[pst] does not produce a recordset,
  the function returns @racket[#f].
}


@defproc[(bind-prepared-statement
            [pst prepared-statement?]
            [params (listof any/c)])
         statement-binding?]{

  Fills in the parameters of a parameterized prepared query. The
  resulting statement can be executed with @racket[query],
  @racket[query-multiple], or any of the other query functions, but it
  must be used with the same connection that created it.

  @(examples/results
    [(let* ([get-name-pst
            (prepare pgc "select description from the_numbers where n = $1")]
            [get-name2
             (bind-prepared-statement get-name-pst (list 2))]
            [get-name3
             (bind-prepared-statement get-name-pst (list 3))])
       (list (query-value pgc get-name2)
             (query-value pgc get-name3)))
     (list "company" "a crowd")])

  Most query functions perform the binding step implicitly, but there
  are functions such as @racket[query-fold] that do not accept query
  parameters; @racket[bind-prepared-statement] is neccessary to use a
  parameterized query with such functions.
}

@defproc[(statement-binding? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a statement created by
  @racket[bind-prepared-statement], @racket[#f] otherwise.
}

@defproc[(statement-generator [gen (or/c string? (-> dbsystem? string?))])
         statement-generator?]{

  Creates a @deftech{statement generator} @racket[_stmt], which
  encapsulates a weak hash mapping connections to prepared statement
  objects. When a query function is called with @racket[_stmt] and a
  connection, the weak hash is consulted to see if the statement has
  already been prepared for that connection. If so, the prepared
  statement is used; otherwise, the statement is prepared and stored
  in the table.

  The @racket[gen] argument must be either a SQL string or a function
  that accepts a databse system object and produces a SQL string. The
  function variant allows the SQL syntax to be dynamically customized
  for the database system in use.

@examples/results[
[(define pst
   (statement-generator
    (lambda (dbsys)
      (case (dbsystem-name dbsys)
        ((postgresql) "select n from the_numbers where n < $1")
        ((sqlite3) "select n from the_numbers where n < ?")))))
 (void)]
[(query-list pgc pst 3)
 (list 1 2)]
[(query-list slc pst 3)
 (list 1 2)]
]
}

@defproc[(statement-generator? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a statement generator created
  by @racket[statement-generator], @racket[#f] otherwise.
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

@deftogether[[
@defproc[(prepare-query-exec [connection connection?]
                             [stmt (or/c string? statement-generator?)])
         (_param _... -> void?)]
@defproc[(prepare-query-rows [connection connection?]
                             [stmt (or/c string? statement-generator?)])
         (_param _... -> (listof (vectorof _field)))]
@defproc[(prepare-query-list [connection connection?]
                             [stmt (or/c string? statement-generator?)])
         (_param _... -> (listof _field))]
@defproc[(prepare-query-row [connection connection?]
                             [stmt (or/c string? statement-generator?)])
         (_param _... -> (vectorof _field))]
@defproc[(prepare-query-maybe-row [connection connection?]
                                  [stmt (or/c string? statement-generator?)])
         (_param _... -> (or/c (vectorof _field) false?))]
@defproc[(prepare-query-value [connection connection?]
                              [stmt (or/c string? statement-generator?)])
         (_param _... -> _field)]
@defproc[(prepare-query-maybe-value [connection connection?]
                                    [stmt (or/c string? statement-generator?)])
         (_param _... -> (or/c _field false?))]
@defproc[(prepare-query-fold [connection connection?]
                              [stmt (or/c string? statement-generator?)]
                             [proc (_alpha _field _... -> _alpha)]
                             [init _alpha])
         (_param _... -> _alpha)]
@defproc[(prepare-query [connection connection?]
                        [stmt (or/c string? statement-generator?)])
         (_param _... -> (or/c simple-result? recordset?))]]]{

  Prepared versions of @racket[query-exec], @racket[query-rows],
  @racket[query-list], @racket[query-row], @racket[query-maybe-row],
  @racket[query-value], @racket[query-maybe-value],
  @racket[query-fold], and @racket[query], respectively.
}
