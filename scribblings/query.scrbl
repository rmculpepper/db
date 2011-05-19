#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt"
          "tabbing.rkt")

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
@item{SQL runtime errors, including integrity constraint violations}
@item{violations of a specialized query function's expectations, such
  as using @racket[query-value] with a query that returns multiple
  columns}
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
indefinitely. See @secref["connect-util"] for a way to make kill-safe
connections.

@section{Statements}

All queries require both a connection and a @deftech{statement}, which
is one of the following:
@itemlist[
@item{a string containing a single SQL statement, possibly with
parameters}
@item{a @tech{prepared statement} produced by @racket[prepare]}
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

The types of parameters and returned fields are described in
@secref["sql-types"].

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
         (listof vector?)]{

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
         list?]{

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
         vector?]{

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
         (or/c vector? #f)]{

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
         any/c]{

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
         (or/c any/c #f)]{

  Like @racket[query-value], but the query may produce zero rows; in
  that case, @racket[#f] is returned.

@examples/results[
[(query-value myc "select s from some_table where n = ?" 100)
 #f]
[(query-value c "select 17")
 17]
]
}

@defproc[(in-query [connection connection?]
                   [stmt statement?]
                   [arg any/c] ...)
         sequence?]{

  Executes a SQL query, which must produce a recordset, and returns a
  sequence. Each step in the sequence produces as many values as the
  recordset has columns.

@examples/results[
[(for/list ([n (in-query pgc "select n from the_numbers where n < 2")])
   n)
 '(0 1)]
[(for ([(n d)
        (in-query pgc "select * from the_numbers where n < $1" 4)])
   (printf "~a is ~a\n" n d))
 (for-each (lambda (n d) (printf "~a: ~a\n" n d))
           '(0 1 2 3) '("nothing" "the loneliest number" "company" "a crowd"))]
]

An @racket[in-query] application can provide better performance when
it appears directly in a @racket[for] clause. In addition, it may
perform stricter checks on the number of columns returned by the query
based on the number of variables in the clause's left-hand side:

@examples/results[
[(for ([n (in-query pgc "select * from the_numbers")])
   (displayln n))
 (error 'in-query "query returned 2 columns (expected 1): ~e"
        "select * from the_numbers")]
]
}


@section{General query support}

A general query result is either a @racket[simple-result] or a
@racket[recordset].

@defstruct*[simple-result
            ([info any/c])]{

Represents the result of a SQL statement that does not return a
relation, such as an @tt{INSERT} or @tt{DELETE} statement.  

The @racket[info] field is usually an association list, but do not
rely on its contents; it varies based on database system and may
change in future versions of this library (even new minor versions).
}

@defstruct*[recordset
            ([headers (listof any/c)]
             [rows (listof vector?)])]{

Represents the result of SQL statement that results in a relation,
such as a @tt{SELECT} query.

The @racket[headers] field is a list whose length is the number of
columns in the result rows. Each header is usually an association list
containing information about the column, but do not rely on its
contents; it varies based on the database system and may change in
future version of this library (even new minor versions).
}

@defproc[(query [connection connection?]
                [stmt statement?]
                [arg any/c] ...)
         (or/c simple-result? recordset?)]{

  Executes a query, returning a structure that describes the
  results. Unlike the more specialized query functions, @racket[query]
  supports both recordset-returning and effect-only queries.
}

@defproc[(query-fold [connection connection?]
                     [stmt (or/c string? statement-binding?)]
                     [fold-proc (_alpha _field _... -> _alpha)]
                     [init _alpha])
         _alpha]{

  Left fold over the results of the query. The arity of
  @racket[fold-proc] must include a number one greater than the number
  of columns returned by the query. Inline parameter arguments are not
  supported; parameter binding must be done explicitly with
  @racket[bind-prepared-statement].

  This function is deprecated; use @racket[for/fold] with
  @racket[in-query] instead.
}


@section{Prepared statements}

A @deftech{prepared statement} is the result of a call to
@racket[prepare].

The syntax of parameterized queries varies depending on the database
system. For example:

@centered{
@tabbing{
PostgreSQL:   @&  @tt{select * from the_numbers where num > $1;} @//
MySQL, ODBC:  @&  @tt{select * from the_numbers where num > ?;} @//
SQLite:       @&  supports both syntaxes (plus others)
}
}

Any server-side or native-library resources associated with a prepared
statement are released when the prepared statement is
garbage-collected or when the connection that owns it is closed;
prepared statements do not need to be (and cannot be) explicitly
closed.

@defproc[(prepare [connection connection?]
                  [stmt (or/c string? statement-generator?)])
         prepared-statement?]{

  Prepares a (possibly parameterized) statement. The resulting
  @tech{prepared statement} is tied to the connection that prepared
  it; attempting to execute it with another connection will trigger an
  exception. (The prepared statement holds its connection link weakly;
  a reference to a prepared statement will not keep a connection from
  being garbage collected.)
}

@defproc[(prepared-statement? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @tech{prepared statement}
  created by @racket[prepare], @racket[#f] otherwise.
}

@defproc[(prepared-statement-parameter-types [pst prepared-statement?])
         (listof (list/c boolean? (or/c symbol? #f) any/c))]{

  Returns a list with one element for each of the prepared statement's
  parameters. Each element is itself a list of the following form:

  @racketblock[(list _supported? _type _typeid)]

  The @racket[_supported?] field is @racket[#t] if the type is
  supported by this library; the @racket[_type] field is a symbol
  corresponding to one of the tables in @secref["db-types"], and the
  @racket[_typeid] field is a system-specific type identifier. The
  type description list format may be extended with additional
  information in future versions of this library.
}

@defproc[(prepared-statement-result-types [pst prepared-statement?])
         (listof (list/c boolean? (or/c symbol? #f) any/c))]{

  If @racket[pst] is a recordset-producing statement (eg,
  @tt{SELECT}), returns a list of type descriptions as described
  above, identifying the SQL types (or pseudotypes) of the result
  columns. If @racket[pst] does not produce a recordset, the function
  returns the empty list.
}


@defproc[(bind-prepared-statement
            [pst prepared-statement?]
            [params (listof any/c)])
         statement-binding?]{

  Creates a statement-binding value pairing @racket[pst] with
  @racket[params], a list of parameter arguments. The result can be
  executed with @racket[query] or any of the other query functions,
  but it must be used with the same connection that created
  @racket[pst].

  @(examples/results
    [(let* ([get-name-pst
            (prepare pgc "select d from the_numbers where n = $1")]
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
  encapsulates a weak mapping of connections to prepared
  statements. When a query function is called with @racket[_stmt] and
  a connection, the weak hash is consulted to see if the statement has
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
        ((sqlite3) "select n from the_numbers where n < ?")
        (else (error "unknown system"))))))
 (void)]
[(query-list pgc pst 3)
 (list 1 2)]
[(query-list slc pst 3)
 (list 1 2)]
]
}

@defproc[(statement-generator? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @tech{statement generator}
  created by @racket[statement-generator], @racket[#f] otherwise.
}


@section{Transactions}

@;{FIXME: explain differences between these functions and various SQL stmts}

@defproc[(start-transaction [c connection?])
         void?]{

  Starts a transaction. If @racket[c] is already in a transaction, an
  exception is raised.
}

@defproc[(commit-transaction [c connection?]) void?]{

  Attempts to commit the current transaction, if one is active. If the
  transaction cannot be commited, an exception is raised.

  If no transaction is active, has no effect.
}

@defproc[(rollback-transaction [c connection?])
         'rollack]{

  Rolls back the current transaction, if one is active.

  If no transaction is active, has no effect.
}

@defproc[(in-transaction? [c connection?])
         boolean?]{

  Returns @racket[#t] if @racket[c] has a transaction is active,
  @racket[#f] otherwise.
}
