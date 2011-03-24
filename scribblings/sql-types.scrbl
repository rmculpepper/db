#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt"
          "tabbing.rkt")

@title[#:tag "sql-types"]{SQL types and conversions}

@(my-declare-exporting)

For most basic SQL types, connections automatically convert query
results to appropriate Racket types. Likewise, query parameters are
accepted as Racket values and converted to the appropriate SQL type.

@examples/results[
[(query-value pgc "select count(*) from the_numbers") 4]
[(query-value pgc "select false") #f]
[(query-value pgc "select 1 + $1" 2) 3]
]


@section{Database system type correspondences}

This sections describes the correspondences between SQL types and
Racket types for the supported database systems. 
         
@subsection[#:tag "postgresql-types"]{PostgreSQL}

The following table lists the PostgreSQL types known to this library,
along with their corresponding Racket representations. The type name
as listed generally corresponds to the SQL notation with spaces
replaced by dashes.

@tabbing{
  @bold{PostgreSQL type}          @& @bold{Aliases}      @& @bold{Racket type} @//
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

A SQL value of type @tt{numeric} (aka @tt{decimal}) is always
converted to either an exact rational or @scheme[+nan.0]. When
converting Scheme values to SQL @tt{numeric}, exact rational values
representable by finite decimal strings are converted without loss of
precision. (Precision may be lost, of course, if the value is then
stored in a database field of lower precision.) Other real values are
converted to decimals with a loss of precision.

@examples/results[
[(query-value pgc "select real '+Infinity'")
 +inf.0]
[(query-value pgc "select decimal '12345678901234567890'")
 12345678901234567890]
]

PostgreSQL defines many other types, such as network addresses and
various geometric shapes. These are currently converted to and from
Racket strings, but future versions of this library may include new
type correspondences and conversions.

Array types are also not currently supported. Support may be added in
a future version.

@examples/results[
[(query-value pgc "select point (1,2)") "(1,2)"]
[(query-value pgc "select '{1,2,3}'::int[]") "{1,2,3}"]
]

See also date and time examples in @secref{sql-data}.


@subsection[#:tag "mysql-types"]{MySQL}

The following table lists the MySQL types known to this package, along
with their corresponding Racket representations. The type name as
listed generally corresponds to the SQL notation with spaces replaced
by dashes.

@tabbing{
  @bold{MySQL type}               @& @bold{Aliases}      @& @bold{Racket type} @//
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

MySQL does not infer parameter types in prepared queries, instead
assigning them the pseudotype @tt{var-string}. Consequently,
conversion of Racket values to @tt{var-string} parameters accepts, in
addition to strings, numbers (@racket[rational?]---no infinities or
NaN) and SQL date/time structures (@racket[sql-date?],
@racket[sql-time?], and @racket[sql-timestamp?]).

A SQL value of type @tt{numeric} (aka @tt{decimal}) is always
converted to an exact rational (MySQL seems not to support infinite
@tt{numeric} values).

See also date and time examples in @secref{sql-data}.


@subsection[#:tag "sqlite-types"]{SQLite}

The following table lists the SQLite types known to this package,
along with their corresponding Racket representations. The type name
as listed generally corresponds to the SQL notation with spaces
replaced by dashes.

Unlike PostgreSQL and MySQL, SQLite does not enforce declared type
constraints (with the exception of integer primary keys). Rather,
every SQLite value has an associated ``storage class''.

@tabbing{
  @bold{SQLite storage class}  @& @bold{Racket type} @//
  @tt{integer}                 @& @scheme[exact-integer?] @//
  @tt{real}                    @& @scheme[real?] @//
  @tt{text}                    @& @scheme[string?] @//
  @tt{blob}                    @& @scheme[bytes?]
}

An exact integer that cannot be represented as a 64-bit signed integer
is converted as @tt{real}, not @tt{integer}.

@examples/results[
[(query-value slc "select ?" (expt 2 80))
 1.2089258196146292e+24]
]


@;{----------------------------------------}

@section[#:tag "sql-data"]{SQL data}

SQL @tt{NULL} values are always translated into the unique @scheme[sql-null] value.

@defthing[sql-null sql-null?]
@defproc[(sql-null? [val any/c])
         boolean?]{

A special value and predicate used to represent @tt{NULL} values in
query results. The @scheme[sql-null] value may be recognized using
@scheme[eq?].

@(examples/results
  [(query-value psql-c "select NULL")
   sql-null])
}

New Racket datatypes are also provided for a few SQL types that have
no existing close analogues.

@defstruct*[sql-date
            ([year exact-nonnegative-integer?]
             [month exact-nonnegative-integer?]
             [day exact-nonnegative-integer?])]
@defstruct*[sql-time
            ([hour exact-nonnegative-integer?]
             [minute exact-nonnegative-integer?]
             [second exact-nonnegative-integer?]
             [nanosecond exact-nonnegative-integer?]
             [tz (or/c exact-integer? #f)])]
@defstruct*[sql-timestamp
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
  fractional seconds to nanosecond precision for compatibility with
  SRFI 19. Note, however, that database systems generally do not
  support nanosecond precision; PostgreSQL, for example, only supports
  microsecond precision.

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

@examples/results[
[(query-value mysql-c "select date('1980-12-25')")
 (make-sql-date 1980 12 25)]
[(query-value mysql-c "select time('7:30')")
 (make-sql-time 7 30 0 0 #f)]
[(query-value mysql-c "select from_unixtime(0)")
 (make-sql-timestamp 1969 12 31 19 0 0 0 #f)]
]

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
