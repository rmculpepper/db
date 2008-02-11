#lang scribble/doc

@(require scribble/manual)
@(require scribble/eval)
@(require scribble/struct)
@(require scheme/sandbox)

@(require (for-label scheme/base)
          (for-label "../generic/main.ss"))

@(define the-eval (make-base-eval))
@(interaction-eval #:eval the-eval
                   (require scheme/class
                            "../generic/main.ss"))
@(define-syntax-rule (examples/results [example result] ...)
   (examples #:eval the-eval (eval:alts example result) ...))
@(define-syntax-rule (my-interaction [example result] ...)
   (interaction #:eval the-eval (eval:alts example result) ...))


@title{Connection API}

@defmodule["../generic/main.ss"]

@section{Administrative methods}

@definterface[connection<%> ()]{

A connection contains the following administrative methods:
@defmethod[(disconnect)
           void?]{
Disconnects from the server.
}
@defmethod[(connected?)
           boolean?]{
Indicates whether the connection is connected.
}
}

@section{High-level Query API}

Spgsql implements a high-level, functional query interface. Once
connected, connection objects are relatively stateless. When a query
method is invoked, it either returns a result or, if the query caused
an error, raises an exception. Different query methods impose
different constraints on the query results and offer different
mechanisms for processing the results.

The spgsql query interface does not expose any low-level
machinery. Programmers who want cursors should use SQL-language
cursors via the @tt{DECLARE CURSOR}, @tt{MOVE}, and @tt{FETCH} statements.

A Statement is either a string containing a single SQL statement or an
opaque value returned by @method[prepare-query<%> bind-prepared-statement].

@definterface[query<%> ()]{

@defmethod[(exec [stmt Statement?] ...)
           void?]{

  Executes SQL statements for effect and discards the result(s).
  Calling @method[query<%> exec] on multiple statements at once may be more
  efficient than calling @method[query<%> exec] multiple times on the
  statements individually.

  Example:
  @schemeinput[
    (send c exec "create table the_numbers (n integer, name varchar)"
                 "insert into the_numbers (n, name) values (0, 'zero')")]

  @bold{PostgreSQL note}: The set of statements passed to
  @method[query<%> exec] are executed within their own
  ``mini-transaction''; if any statement fails, the effects of all
  previous statements in the set are rolled back.

}

@defmethod[(query-list [stmt Statement?])
           (listof _field)]{

  Executes a SQL query which must return a recordset of exactly one
  column; returns the list of values from the query.
}
@defmethod[(query-row [stmt Statement?])
           (vectorof _field)]{

  Executes a SQL query which must return a recordset of exactly one
  row; returns its (single) row result as a vector.
}
@defmethod[(query-maybe-row [stmt Statement?])
           (or/c (vectorof _field) false/c)]{

  Like @method[query<%> query-row], but the query may return zero rows; in that
  case, the method returns @scheme[#f].
}
@defmethod[(query-value [stmt Statement?])
           _field]{

  Executes a SQL query which must return a recordset of exactly one
  column and exactly one row; returns its single value result.
}
@defmethod[(query-maybe-value [stmt Statement?])
           (or/c _field false/c)]{

  Like @method[query<%> query-value], but the query may return zero rows; in
  that case, the method returns false.
}
@defmethod[(map [stmt Statement?]
                [proc (_field _... -> _a)])
           (listof _a)]{

  Executes a SQL query and applies the given function to the contents
  of each row, returning a list of results.
}
@defmethod[(for-each [stmt Statement?]
                     [proc (_field _... -> any)])
           void?]{

  Executes a SQL query and applies the given function to the contents
  of each row, discarding the results.
}
@defmethod[(mapfilter [stmt Statement?]
                      [map-proc (_field _... -> _a)]
                      [filter-proc (_field _... -> boolean?)])
           (listof _a)]{

  Like @method[query<%> map], but applies the map procedure (given first) to
  only those rows which satisfy the given predicate (given second).
}
@defmethod[(fold [stmt Statement?]
                 [proc (_a _field _... -> _a)]
                 [init _a])
           _a]{

  Left fold over the results of the query.
}
}

Connections also support methods for preparing parameterized
queries. A parameterized query may be executed any number of times
with different values for its parameters.

A parameterized query is written with positional arguments. For example:

@verbatim{select * from the_numbers where num > $1;}

The following methods provide a convenient functional interface for
common uses of parameterized prepared statements:

@definterface[query/prepare<%> ()]{

  Each of the following methods prepares the parameterized SQL
  statement for later execution and returns a closure. The closure
  accepts the parameter values and executes the prepared statement,
  processing the results like the corresponding query method.

  A prepared-statement closure may be executed any number of times.
  It is possible to prepare a statement that contains no parameters;
  the resulting procedure should be called with zero arguments.

@defmethod[(prepare-exec [prep string?])
           (_param _... -> void?)]{
  Prepared version of @method[query<%> exec]. Unlike @method[query<%>
  exec], @method[query/prepare<%> prepare-exec] permits only a single
  statement.
}

@defmethod[(prepare-query-list [prep string?])
           (_param _... -> (listof _field))]{
  Prepared version of @method[query<%> query-list].
}

@defmethod[(prepare-query-row [prep string?])
           (_param _... -> (vectorof _field))]{
  Prepared version of @method[query<%> query-row].
}
@defmethod[(prepare-query-maybe-row [prep string?])
           (_param _... -> (or/c (vectorof _field) false?))]{
  Prepared version of @method[query<%> query-maybe-row].
}

@defmethod[(prepare-query-value [prep string?])
           (_param _... -> _field)]{
  Prepared version of @method[query<%> query-value].
}

@defmethod[(prepare-query-maybe-value [prep string?])
           (_param _... -> (or/c _field false?))]{
  Prepared version of @method[query<%> query-maybe-value].
}

@defmethod[(prepare-map [prep string?] [proc (_field _... -> _a)])
           (_param _... -> (listof _a))]{
  Prepared version of @method[query<%> map].
}

@defmethod[(prepare-for-each [prep string?] [proc (_field _... -> void?)])
           (_param _... -> void?)]{
  Prepared version of @method[query<%> for-each].
}

@defmethod[(prepare-mapfilter [prep string?]
                              [map-proc (_field _... -> _a)]
                              [filter-proc (_field _... -> boolean?)])
           (_param _... -> (listof _a))]{
  Prepared version of @method[query<%> mapfilter].
}

@defmethod[(prepare-fold [prep string?]
                         [proc (_a _field _... -> _a)]
                         [init _a])
           (_param _... -> _a)]{
  Prepared version of @method[query<%> fold].
}

}

@section{Low-level Query API}

In addition to the high-level query API, spgsql connections support
the following methods for preparing, binding, and executing queries:

@tt{
A QueryResult is one of:
  - (make-SimpleResult string)
  - (make-Recordset (list-of FieldInfo) (list-of (vector-of datum)))
}

@tt{
A FieldInfo is (make-FieldInfo string)
}

@defstruct[SimpleResult
           ((command any/c))]
@defstruct[Recordset
           ((info any/c)
            (data any/c))]
@defstruct[FieldInfo
           ((name symbol?))]

@definterface[query<%> ()]{

The @scheme[query<%>] interface contains the following additional
low-level query methods:

@defmethod[(query [stmt Statement?])
           QueryResult?]
@defmethod[(query-multiple [stmts (listof Statement?)])
           (listof QueryResult?)]{
  Executes queries, returning structures that describe the
  results. Unlike the high-level query methods, @method[query<%> query-multiple]
  supports a mixture of recordset-returning queries and effect-only
  queries.
}

@defmethod[(prepare [prep string?])
           PreparedStatement?]
@defmethod[(prepare-multiple [preps (listof string?)])
           (listof PreparedStatement?)]{
  Prepare parameterized queries. The resulting PreparedStatements are
  tied to the connection object that prepared them; it is an error to
  use them with any other connection.
}

@defmethod[(bind-prepared-statement [pst PreparedStatement?]
                                    [params (listof any/c)])
           Statement?]{

  Fill in a parameterized prepared query with its parameters. The
  resulting Statement can be executed with @method[query<%>
  query-multiple] or any of the high-level query methods, but it must
  be used with the same connection object.

  @(examples/results
    [(let ([get-name-pst
            (send c prepare "select name from the_numbers where n = $1")])
       (let ([get-name1 (send c bind-prepared-statement get-name-pst (list 1))]
             [get-name2 (send c bind-prepared-statement get-name-pst (list 2))])
         (send c query-multiple (list get-name1 get-name2))))
     (list (make-Recordset (list (make-FieldInfo "name")) (list "one"))
           (make-Recordset (list (make-FieldInfo "name")) (list "two")))])
}
}

@section{SQL Types and Conversions}

For most basic SQL types, connections automatically process query
results and paramterized query parameters to convert between Scheme
values and SQL external representations. When there is no automatic
conversion to or from a SQL type, you must supply or accept a string
containing the SQL value's external representation.

@(examples/results
  [(send c query-value "select 18") 18]
  [(send c query-value "select false") #f]

  [(send c query-value "select '{1,2,3}'::int[]") "{1,2,3}"]
  [(send c query-value "select point (1,2)") "(1,2)"])

SQL NULL values are always translated into the unique @scheme[sql-null] value.

@defthing[sql-null sql-null?]
@defproc[(sql-null? [val any/c])
         boolean?]{
  A special value and predicate used to represent NULL values in
  query results.
}
         
@subsection{Conversions}

Here are the SQL types known to spgsql with their corresponding Scheme
representations. The type is listed in the notation accepted by
spgsql; it generally corresponds to the SQL notation with spaces
replaced by dashes.

@(require "tabbing.ss")
@tabbing{
  @bold{Type name}                @& @bold{Aliases}      @& @bold{Scheme datatype} @//
  @tt{bigint}                     @& @tt{int8}           @& @scheme[exact-integer?] @//
  @tt{bigserial}                  @& @tt{serial8}        @& @scheme[exact-integer?] @//
  @tt{boolean}                    @& @tt{bool}           @& @scheme[boolean?] @//
  @tt{bytea}                      @& @tt{}               @& @scheme[bytes?] @//
  @tt{character-varying}          @& @tt{varchar}        @& @scheme[string?] @//
  @tt{character}                  @& @tt{char}           @& @scheme[string?] @//
  @tt{date}                       @& @tt{}               @& @scheme[sql-date?] @//
  @tt{double-precision}           @& @tt{double float8}  @& @scheme[real?] @//
  @tt{integer}                    @& @tt{int int4}       @& @scheme[exact-integer?] @//
  @tt{numeric}                    @& @tt{decimal}        @& @scheme[number?] @//
  @tt{real}                       @& @tt{float4}         @& @scheme[real?] @//
  @tt{smallint}                   @& @tt{int2}           @& @scheme[exact-integer?] @//
  @tt{serial}                     @& @tt{serial4}        @& @scheme[exact-integer?] @//
  @tt{text}                       @& @tt{}               @& @scheme[string?] @//
  @tt{time-without-time-zone}     @& @tt{time}           @& @scheme[sql-time?] @//
  @tt{time-with-time-zone}        @& @tt{timetz}         @& @scheme[sql-time?] @//
  @tt{timestamp-without-time-zone}@& @tt{timestamp}      @& @scheme[sql-timestamp?] @//
  @tt{timestamp-with-time-zone}   @& @tt{timestamptz}    @& @scheme[sql-timestamp?] @//
  @tt{oid}                        @& @tt{}               @& @scheme[exact-integer?]
}

A SQL value of type @tt{numeric/decimal} is always converted to
either an exact rational or @scheme[+nan.0]. When converting Scheme
values to SQL @tt{numeric}, exact rational values representable by
finite decimal strings are converted without loss of precision. Other
real values are converted to decimals with a loss of precision.

PostgreSQL defines other datatypes, such as network addresses and
various geometric concepts. These are not supported by spgsql.

Array types are also not currently supported by spgsql. Support may be
added in a future version.

@subsection{SQL Data}

Spgsql provides datatypes for a few SQL types that have no close
analogues in Scheme.

@defstruct[sql-date
           ([year exact-nonnegative-integer?]
            [month exact-nonnegative-integer?]
            [day exact-nonnegative-integer?])]
@defstruct[sql-time
           ([hour exact-nonnegative-integer?]
            [minute exact-nonnegative-integer?]
            [second exact-nonnegative-integer?]
            [nanosecond exact-nonnegative-integer?]
            [tz (or/c exact-nonnegative-integer? false/c)])]
@defstruct[sql-timestamp
           ([year exact-nonnegative-integer?]
            [month exact-nonnegative-integer?]
            [day exact-nonnegative-integer?]
            [hour exact-nonnegative-integer?]
            [minute exact-nonnegative-integer?]
            [second exact-nonnegative-integer?]
            [nanosecond exact-nonnegative-integer?]
            [tz (or/c exact-nonnegative-integer? false/c)])]{

  Representations of SQL dates, times, and timestamps. The @scheme[tz]
  field may be false to indicate no time zone information.

  The @scheme[sql-time] and @scheme[sql-timestamp] structures store
  fractional seconds to nanosecond accuracy for compatibility with
  SRFI 19. Note, however, that PostgreSQL only supports microsecond
  time accuracy. Fractional seconds are rounded to the nearest
  microsecond when they are stored in the database.
}

@defproc[(sql-datetime->srfi-date [t (or/c sql-date? sql-time? sql-timestamp?)])
         date?]
@defproc[(srfi-date->sql-date [d date?])
         sql-date?]
@defproc[(srfi-date->sql-time [d date?])
         sql-time?]
@defproc[(srfi-date->sql-time-tz [d date?])
         sql-time?]
@defproc[(srfi-date->sql-timestamp [d date?])
         sql-timestamp?]
@defproc[(srfi-date->sql-timestamp-tz [d date?])
         sql-timestamp?]{
  Converts between this library's date and time values and SRFI 19's
  date values. SRFI dates store more information than SQL dates and
  times, so converting a SQL time to a SRFI date, for example, puts
  zeroes in the year, month, and day fields.
}

@subsection{Creating SQL Strings}

The @scheme[format-sql] and @scheme[concat-sql] macros help construct
SQL query strings safely:

@defform/subs[(format-sql format-string ... sql-spec ...)
              ([sql-spec [type-id expr]
                          [#:trust expr type-string-expr]
                          [#:name expr]
                          [#:Name expr]
                          [#:sql expr]])]{
  Encodes each tagged datum to a SQL literal expression and inserts it
  into the format string. The result is a string. A @scheme[sql-spec] has one
  of the following forms, categorized by the SQL statement context it
  is used in:

  SQL literal expressions:
  @specsubform[[type-id expr]]{

      Converts the result of @scheme[expr] to the SQL type named by
      @scheme[type-id]. The @scheme[type-id] must be a syntactic
      identifier naming a SQL type, and @scheme[expr] must evaluate to
      a value of the appropriate Scheme datatype.

      Example: This code generates a query that returns all numbers 
      greater than the value of the Scheme variable @scheme[lower-bound]:
      @schemeblock[
        (format-sql "select n from the_numbers where n >= ~a"
                    [int4 lower-bound])
      ]

      Warning: the type name is not checked. You must avoid specifying
      type names that contain SQL delimiters such as @litchar{)} or
      @litchar{--}.
  }
  @specsubform[[#:trust expr type-string-expr]]{

      Performs minimal escaping on the value of @scheme[expr], which must be
      a string that is a suitable external representation for the SQL
      type named by @scheme[type-string-expr].

      Example: Generates a query for boxes containing the origin:
      @schemeblock[
        (format-sql "select b from some_boxes where ~a < b"
                    [#:trust "(0,0)" "point"])
      ]

      Warning: the type name is not checked. You must avoid specifying
      type names that contain SQL delimiters such as @litchar{)} or
      @litchar{--}.
  }

  SQL identifiers:
  @specsubform[[#:name expr]]
  @specsubform[[#:Name expr]]{

      Quotes a SQL identifier (eg, table name or field name). The
      expression must produce a string. Using @scheme[#:name] performs
      the default case conversion on the name. The following are
      equivalent:

        @scheme[[#:name "table"]] = @scheme[[#:name "TABLE"]] = @scheme[[#:name "TaBlE"]]

      If @scheme[#:Name] is used, the case of the name is preserved.

      Example:
      @schemeblock[
        (format-sql "select n from ~a"
                    [#:name "the_numbers"])
      ]
  }

  SQL code:
  @specsubform[[#:sql expr]]{

      Splices in a string containing arbitrary SQL code. No escaping
      of is done, but whitespace is added around the spliced code.

      Example:
      @schemeblock[
        (format-sql "select n from ~a ~a"
                    [#:name "the_numbers"]
                    [#:sql (if only-pos? "where n > 0" "")])
      ]

  Note: The format string must contain only @litchar{~a} placeholders
  (using @litchar{~s} placeholders would generally result in invalid
  SQL). Literal @litchar{~} characters may be written as @litchar{~~}.
  }
}

@defform/subs[(concat-sql string-or-sql-spec ...)
              ([string-or-sql-spec string
                                    sql-spec])]{

  Composes a SQL string by concatenating the literal strings and the
  interpretations of the @scheme[sql-spec] (as above; see
  @scheme[format-sql]).

  Whitespace is added after every fragment. Unlike
  @scheme[format-sql], the @litchar{~} character has no special
  meaning within the strings.

  Example:
  @schemeblock[
    (concat-sql "select n"
                "from" [#:name "the_numbers"]
                [#:sql (if only-pos? "where n > 0" "")])
  ]
}
