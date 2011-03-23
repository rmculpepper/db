#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt"
          "tabbing.rkt")

@(define-syntax-rule (qmeth meth) (method connection:query<%> meth))
@(define-syntax-rule (pmeth meth) (method connection:query/prepare<%> meth))

@title{SQL Types and Conversions}

@(my-declare-exporting)

For most basic SQL types, connections automatically process query
results and paramterized query parameters to convert between Scheme
values and SQL external representations. When there is no automatic
conversion to or from a SQL type, you must supply or accept a string
containing the SQL value's external representation.

@(examples/results
  [(query-value psql-c "select 18") 18]
  [(query-value psql-c "select false") #f]
  [(query-value psql-c "select '{1,2,3}'::int[]") "{1,2,3}"]
  [(query-value psql-c "select point (1,2)") "(1,2)"])

The sections below describe this library's support for PostgreSQL and
MySQL data types. The listed type names and aliases are not used by
any operation defined in this library yet, but more operations on SQL
data types are planned, and they will use the names listed below.
         
@section{PostgreSQL Types and Conversions}

Here are the PostgreSQL types known to this package with their
corresponding Scheme representations. The type is listed by its Scheme
notation, which generally corresponds to the SQL notation with spaces
replaced by dashes.

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

PostgreSQL defines many other datatypes, such as network addresses and
various geometric types. These are not converted.

Array types are also not currently supported. Support may be added in
a future version.

@(examples/results
  [(query-value psql-c "select real '+Infinity'")
   +inf.0]
  [(query-value psql-c "select decimal '12345678901234567890'")
   12345678901234567890])

See also date and time examples in @secref{sql-data}.

@section{MySQL Types and Conversions}

Here are the MySQL types known to this package with their
corresponding Scheme representations. The type is listed by its Scheme
notation, which generally corresponds to the SQL notation with spaces
replaced by dashes.

@tabbing{
  @bold{Type name}                @& @bold{Aliases}      @& @bold{Scheme datatype} @//
  @tt{integer}                    @& @tt{int long}       @& @scheme[exact-integer?] @//
  @tt{tinyint}                    @& @tt{}               @& @scheme[exact-integer?] @//
  @tt{smallint}                   @& @tt{}               @& @scheme[exact-integer?] @//
  @tt{mediumint}                  @& @tt{}               @& @scheme[exact-integer?] @//
  @tt{biginteger}                 @& @tt{bigint}         @& @scheme[exact-integer?] @//
  @tt{real}                       @& @tt{float}          @& @scheme[real?] @//
  @tt{double-precision}           @& @tt{double}         @& @scheme[real?] @//
  @tt{numeric}                    @& @tt{decimal}        @& @scheme[number?] @//
  @tt{character-varying}          @& @tt{varchar}        @& @scheme[string?] @//
  @tt{date}                       @& @tt{}               @& @scheme[sql-date?] @//
  @tt{time-without-time-zone}     @& @tt{time}           @& @scheme[sql-time?] @//
  @tt{timestamp-without-time-zone}@& @tt{datetime}       @& @scheme[sql-timestamp?]
}

A SQL value of type @tt{numeric/decimal} is always converted to an
exact rational (MySQL seems not to support infinite numerics).


@section[#:tag "sql-data"]{SQL Data}

SQL NULL values are always translated into the unique @scheme[sql-null] value.

@defthing[sql-null sql-null?]
@defproc[(sql-null? [val any/c])
         boolean?]{

A special value and predicate used to represent NULL values in query
results. The @scheme[sql-null] value may be recognized using
@scheme[eq?].

@(examples/results
  [(query-value psql-c "select NULL")
   sql-null])
}

New Scheme datatypes are also provided for a few SQL types that have
no close analogues in Scheme.

@defstruct[sql-date
           ([year exact-nonnegative-integer?]
            [month exact-nonnegative-integer?]
            [day exact-nonnegative-integer?])]
@defstruct[sql-time
           ([hour exact-nonnegative-integer?]
            [minute exact-nonnegative-integer?]
            [second exact-nonnegative-integer?]
            [nanosecond exact-nonnegative-integer?]
            [tz (or/c exact-integer? #f)])]
@defstruct[sql-timestamp
           ([year exact-nonnegative-integer?]
            [month exact-nonnegative-integer?]
            [day exact-nonnegative-integer?]
            [hour exact-nonnegative-integer?]
            [minute exact-nonnegative-integer?]
            [second exact-nonnegative-integer?]
            [nanosecond exact-nonnegative-integer?]
            [tz (or/c exact-integer? #f)])]{

  Representations of SQL dates, times, and timestamps. The @scheme[tz]
  field may be @scheme[#f] to indicate no time zone information.

  The @scheme[sql-time] and @scheme[sql-timestamp] structures store
  fractional seconds to nanosecond accuracy for compatibility with
  SRFI 19. Note, however, that PostgreSQL only supports microsecond
  time accuracy. Fractional seconds are rounded to the nearest
  microsecond when they are stored in the database.

@(examples/results
  [(query-value psql-c "select date '25-dec-1980'")
   (make-sql-date 1980 12 25)]
  [(query-value psql-c "select time '7:30'")
   (make-sql-time 7 30 0 0 #f)]
  [(query-value psql-c "select timestamp 'epoch'")
   (make-sql-timestamp 1970 1 1 0 0 0 0 #f)]
  [(query-value psql-c "select timestamp with time zone 'epoch'")
   (make-sql-timestamp 1969 12 31 19 0 0 0 -18000)])
}

@(examples/results
  [(query-value mysql-c "select date('1980-12-25')")
   (make-sql-date 1980 12 25)]
  [(query-value mysql-c "select time('7:30')")
   (make-sql-time 7 30 0 0 #f)]
  [(query-value mysql-c "select from_unixtime(0)")
   (make-sql-timestamp 1969 12 31 19 0 0 0 #f)])

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

@(examples/results
  [(sql-datetime->srfi-date
    (query-value psql-c "select time '7:30'"))
   (sql-datetime->srfi-date (make-sql-time 7 30 0 0 #f))]
  [(sql-datetime->srfi-date
    (query-value psql-c "select date '25-dec-1980'"))
   (sql-datetime->srfi-date
    (make-sql-date 1980 12 25))]
  [(sql-datetime->srfi-date
    (query-value psql-c "select timestamp 'epoch'"))
   (sql-datetime->srfi-date (make-sql-timestamp 1970 1 1 0 0 0 0 #f))])

}

@;{
@section{Creating SQL Strings}

The @scheme[format-sql] and @scheme[concat-sql] macros help construct
SQL query strings. 

@emph{Note:} Constructing query strings is error-prone and
dangerous. When possible, use prepared statements or other means
instead.

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
}
