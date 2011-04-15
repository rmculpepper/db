#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.rkt"
          "tabbing.rkt"
          (for-label (prefix-in srfi: srfi/19)))

@title[#:tag "sql-types"]{SQL types and conversions}

@(my-declare-exporting)

For most basic SQL types, connections automatically convert query
results to appropriate Racket types. Likewise, query parameters are
accepted as Racket values and converted to the appropriate SQL type.

@examples/results[
[(query-value pgc "select count(*) from the_numbers") 4]
[(query-value pgc "select false") (values #f)]
[(query-value pgc "select 1 + $1" 2) 3]
]

If a query result contains a column with a SQL type not supported by
this library, an error is raised. As a workaround, cast the column to
a supported type:

@examples/results[
[(query-value pgc "select point(1,2)")
 (error 'query-value "unsupported type: point (typeid 600)")]
[(query-value pgc "select cast(point(1,2) as varchar)")
 "(1,2)"]
]

The error for unsupported types in result columns is raised when the
query is executed; for parameters it is raised when the parameter
values are supplied. Thus even unexecutable prepared statements can be
inspected using @racket[prepared-statement-parameter-types] and
@racket[prepared-statement-result-types].


@section[#:tag "db-types"]{Type correspondences}

This sections describes the correspondences between SQL types and
Racket types for the supported database systems. 

@subsection[#:tag "postgresql-types"]{PostgreSQL}

The following table lists the PostgreSQL types known to this library,
along with their corresponding Racket representations.

@centered{
@tabbing{
  @bold{PostgreSQL type}  @& @bold{pg_type.typname}  @& @bold{Racket type} @//
  @racket['boolean]       @& @tt{bool}               @& @scheme[boolean?] @//
  @racket['char1]         @& @tt{char}               @& @scheme[char?] @//
  @racket['smallint]      @& @tt{int2}               @& @scheme[exact-integer?] @//
  @racket['integer]       @& @tt{int4}               @& @scheme[exact-integer?] @//
  @racket['bigint]        @& @tt{int8}               @& @scheme[exact-integer?] @//
  @racket['real]          @& @tt{float4}             @& @scheme[real?] @//
  @racket['double]        @& @tt{float8}             @& @scheme[real?] @//
  @racket['decimal]       @& @tt{numeric}            @& @scheme[number?] @//
  @racket['character]     @& @tt{bpchar}             @& @scheme[string?] @//
  @racket['varchar]       @& @tt{varchar}            @& @scheme[string?] @//
  @racket['text]          @& @tt{text}               @& @scheme[string?] @//
  @racket['bytea]         @& @tt{bytea}              @& @scheme[bytes?] @//
  @racket['date]          @& @tt{date}               @& @scheme[sql-date?] @//
  @racket['time]          @& @tt{time}               @& @scheme[sql-time?] @//
  @racket['timetz]        @& @tt{timetz}             @& @scheme[sql-time?] @//
  @racket['timestamp]     @& @tt{timestamp}          @& @scheme[sql-timestamp?] @//
  @racket['timestamptz]   @& @tt{timestamptz}        @& @scheme[sql-timestamp?] @//
  @racket['interval]      @& @tt{interval}           @& @scheme[sql-interval?]
}
}

The @racket['char1] type, written @tt{"char"} in PostgreSQL's SQL
syntax (the quotation marks are significant), is one byte, essentially
a tiny integer written as a character.

A SQL value of type @tt{decimal} is converted to either an exact
rational or @scheme[+nan.0]. When converting Scheme values to SQL
@tt{decimal}, exact rational values representable by finite decimal
strings are converted without loss of precision. (Precision may be
lost, of course, if the value is then stored in a database field of
lower precision.) Other real values are converted to decimals with a
loss of precision. In PostgreSQL, @tt{numeric} and @tt{decimal} refer
to the same type.

@examples/results[
[(query-value pgc "select real '+Infinity'")
 +inf.0]
[(query-value pgc "select numeric '12345678901234567890'")
 12345678901234567890]
]

PostgreSQL defines many other types, such as network addresses,
various geometric shapes, and array types. These are currently not
supported, but future versions of this library may include new type
correspondences and conversions.


@subsection[#:tag "mysql-types"]{MySQL}

The following table lists the MySQL types known to this package, along
with their corresponding Racket representations.

@centered{
@tabbing[#:spacing 8]{
  @bold{MySQL type}            @& @bold{Racket type} @//
  @racket['integer]            @& @scheme[exact-integer?] @//
  @racket['tinyint]            @& @scheme[exact-integer?] @//
  @racket['smallint]           @& @scheme[exact-integer?] @//
  @racket['mediumint]          @& @scheme[exact-integer?] @//
  @racket['bigint]             @& @scheme[exact-integer?] @//
  @racket['real]               @& @scheme[real?] @//
  @racket['double]             @& @scheme[real?] @//
  @racket['decimal]            @& @scheme[number?] @//
  @racket['varchar]            @& @scheme[string?] @//
  @racket['var-string]         @& @scheme[string?] or @scheme[bytes?], but see below @//
  @racket['date]               @& @scheme[sql-date?] @//
  @racket['time]               @& @scheme[sql-time?] or @racket[sql-day-time-interval?] @//
  @racket['datetime]           @& @scheme[sql-timestamp?]
@;{FIXME: blob types?}
}
}

MySQL does not report specific parameter types for prepared queries,
instead assigning them the type @tt{var-string}. Consequently,
conversion of Racket values to @tt{var-string} parameters accepts, in
addition to strings, numbers (@racket[rational?]---no infinities or
NaN) and SQL date/time structures (@racket[sql-date?],
@racket[sql-time?], @racket[sql-timestamp?], and
@racket[sql-day-time-interval?]).

A SQL value of type @tt{decimal} is converted to an exact rational
(MySQL seems not to support infinite @tt{decimal} values).

In MySQL, the @tt{time} type represents time intervals, which may not
correspond to times of day (for example, the interval may be negative
or larger than 24 hours). In conversion from MySQL results to Racket
values, those @tt{time} values that represent times of day are
converted to @racket[sql-time] values; the rest are represented by
@racket[sql-interval] values.


@subsection[#:tag "sqlite-types"]{SQLite}

The following table lists the SQLite types known to this package,
along with their corresponding Racket representations.

Unlike PostgreSQL and MySQL, SQLite does not enforce declared type
constraints (with the exception of @tt{integer primary key}) on
@emph{columns}. Rather, every SQLite @emph{value} has an associated
``storage class''.

@centered{
@tabbing{
  @bold{SQLite storage class}  @& @bold{Racket type} @//
  @tt{integer}                 @& @scheme[exact-integer?] @//
  @tt{real}                    @& @scheme[real?] @//
  @tt{text}                    @& @scheme[string?] @//
  @tt{blob}                    @& @scheme[bytes?]
}
}

SQLite does not report specific parameter and result types for
prepared queries. Instead, they are assigned the pseudotype
@racket['any]. Conversion of Racket values to parameters accepts
strings, bytes, and real numbers.

An exact integer that cannot be represented as a 64-bit signed integer
is converted as @tt{real}, not @tt{integer}.

@examples/results[
[(expt 2 80)
 (expt 2 80)]
[(query-value slc "select ?" (expt 2 80))
 1.2089258196146292e+24]
]


@subsection[#:tag "odbc-types"]{ODBC}

The following table lists the ODBC types known to this package,
along with their corresponding Racket representations.

@centered{
@tabbing[#:spacing 8]{
  @bold{ODBC type}       @& @bold{Racket type} @//
  @racket['character]     @& @scheme[string?] @//
  @racket['varchar]       @& @scheme[string?] @//
  @racket['longvarchar]   @& @scheme[string?] @//
  @racket['numeric]       @& @scheme[rational?] @//
  @racket['decimal]       @& @scheme[rational?] @//
  @racket['integer]       @& @scheme[exact-integer?] @//
  @racket['tinyint]       @& @scheme[exact-integer?] @//
  @racket['smallint]      @& @scheme[exact-integer?] @//
  @racket['bigint]        @& @scheme[exact-integer?] @//
  @racket['float]         @& @scheme[real?] @//
  @racket['real]          @& @scheme[real?] @//
  @racket['double]        @& @scheme[real?] @//
  @racket['date]          @& @scheme[sql-date?] @//
  @racket['time]          @& @scheme[sql-time?] @//
  @racket['datetime]      @& @scheme[sql-timestamp?] @//
  @racket['timestamp]     @& @scheme[sql-timestamp?] @//
  @racket['binary]        @& @scheme[bytes?] @//
  @racket['varbinary]     @& @scheme[bytes?] @//
  @racket['longvarbinary] @& @scheme[bytes?] @//
  @racket['bit1]          @& @scheme[boolean?]
}
}

Not all ODBC drivers provide parameter type information for prepared
queries. In such situations the connection assigns the parameter the
pseudotype @racket['unknown]. Conversion of Racket values to
@racket['unknown] parameters accepts strings, bytes, numbers
(@racket[rational?]---no infinities or NaN) and SQL date/time
structures (@racket[sql-date?], @racket[sql-time?], and
@racket[sql-timestamp?]).

The ODBC type @racket['bit1] represents a single bit, unlike the
standard SQL @tt{bit(N)} type.


@;{----------------------------------------}

@section[#:tag "sql-data"]{SQL data}

SQL @tt{NULL} is translated into the unique @scheme[sql-null] value.

@defthing[sql-null sql-null?]
@defproc[(sql-null? [val any/c])
         boolean?]{

A special value and predicate used to represent @tt{NULL} values in
query results. The @scheme[sql-null] value may be recognized using
@scheme[eq?].

@(examples/results
  [(query-value c "select NULL")
   sql-null])
}

@defproc[(sql-null->false [x any/c]) any/c]{

If @racket[x] is @racket[sql-null], returns @racket[#f], otherwise returns @racket[x].

@examples[#:eval the-eval
(sql-null->false "apple")
(sql-null->false sql-null)
(sql-null->false #f)
]
}

@defproc[(false->sql-null [x any/c]) any/c]{

If @racket[x] is @racket[#f], returns @racket[sql-null], otherwise returns @racket[x].

@examples[#:eval the-eval
(false->sql-null "apple")
(false->sql-null #f)
]
}


New Racket datatypes are also provided for a few SQL types that have
no existing close analogues.

@defstruct*[sql-date
            ([year exact-integer?]
             [month (integer-in 0 12)]
             [day (integer-in 0 31)])]{

  Represents a SQL date.

  MySQL allows @tt{DATE} values with zero components as an extension.
}

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

  Represents SQL times and timestamps.

  The @scheme[tz] field indicates the time zone offset as the number
  of seconds east of GMT (as in SRFI 19). If @racket[tz] is
  @racket[#f], the time or timestamp does not carry time zone
  information.

  The @scheme[sql-time] and @scheme[sql-timestamp] structures store
  fractional seconds to nanosecond precision for compatibility with
  SRFI 19. Note, however, that database systems generally do not
  support nanosecond precision; PostgreSQL, for example, only supports
  microsecond precision.

@(examples/results
  [(query-value pgc "select date '25-dec-1980'")
   (make-sql-date 1980 12 25)]
  [(query-value pgc "select time '7:30'")
   (make-sql-time 7 30 0 0 #f)]
  [(query-value pgc "select timestamp 'epoch'")
   (make-sql-timestamp 1970 1 1 0 0 0 0 #f)]
  [(query-value pgc "select timestamp with time zone 'epoch'")
   (make-sql-timestamp 1969 12 31 19 0 0 0 -18000)])
}

@examples/results[
[(query-value myc "select date('1980-12-25')")
 (make-sql-date 1980 12 25)]
[(query-value myc "select time('7:30')")
 (make-sql-time 7 30 0 0 #f)]
[(query-value myc "select from_unixtime(0)")
 (make-sql-timestamp 1969 12 31 19 0 0 0 #f)]
]

@deftogether[[
@defproc[(sql-datetime->srfi-date [t (or/c sql-date? sql-time? sql-timestamp?)])
         srfi:date?]
@defproc[(srfi-date->sql-date [d srfi:date?])
         sql-date?]
@defproc[(srfi-date->sql-time [d srfi:date?])
         sql-time?]
@defproc[(srfi-date->sql-time-tz [d srfi:date?])
         sql-time?]
@defproc[(srfi-date->sql-timestamp [d srfi:date?])
         sql-timestamp?]
@defproc[(srfi-date->sql-timestamp-tz [d srfi:date?])
         sql-timestamp?]]]{

  Converts between this library's date and time values and SRFI 19's
  date values (see @racketmodname[srfi/19]). SRFI dates store more
  information than SQL dates and times, so converting a SQL time to a
  SRFI date, for example, puts zeroes in the year, month, and day
  fields.

@(examples/results
  [(sql-datetime->srfi-date
    (query-value pgc "select time '7:30'"))
   (sql-datetime->srfi-date (make-sql-time 7 30 0 0 #f))]
  [(sql-datetime->srfi-date
    (query-value pgc "select date '25-dec-1980'"))
   (sql-datetime->srfi-date
    (make-sql-date 1980 12 25))]
  [(sql-datetime->srfi-date
    (query-value pgc "select timestamp 'epoch'"))
   (sql-datetime->srfi-date (make-sql-timestamp 1970 1 1 0 0 0 0 #f))])
}

@defstruct*[sql-interval
            ([years exact-integer?]
             [months exact-integer?]
             [days exact-integer?]
             [hours exact-integer?]
             [minutes exact-integer?]
             [seconds exact-integer?]
             [nanoseconds exact-integer?])]{

  Represents lengths of time. An interval may contain a mixture of
  positive and negative fields.

  On construction, intervals are normalized to satisfy the following
  constraints:
  @itemlist[
  @item{@racket[years] and @racket[months] have the same sign}
  @item{@racket[months] ranges from @racket[-11] to @racket[11]}
  @item{@racket[days], @racket[hours], @racket[minutes],
    @racket[seconds], and @racket[nanoseconds] all have the same sign}
  @item{@racket[hours] range from @racket[-23] to @racket[23]}
  @item{@racket[minutes] and @racket[seconds] range from @racket[-59]
    to @racket[59]} 
  @item{@racket[nanoseconds] ranges from
    @racket[(- (sub1 #, @racketvalfont{#e1e9}))] to
    @racket[(sub1 #, @racketvalfont{#e1e9})]}
  ]

  That is, an interval consists of two groups of components:
  year-month and day-time, and normalization is done only within
  groups. In fact, the SQL standard recognizes those two types of
  intervals separately (see @racket[sql-year-month-interval?] and
  @racket[sql-day-time-interval?], below), and does not permit
  combining them. Representing intervals such as @tt{1 month 3 days}
  is a PostgreSQL extension.
}

@defproc[(sql-year-month-interval? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[sql-interval] value
  where the @racket[days], @racket[hours], @racket[minutes],
  @racket[seconds], and @racket[nanoseconds] fields are zero.
}

@defproc[(sql-day-time-interval? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[sql-interval] value
  where the @racket[years], @racket[months], and @racket[days]
  fields are zero.
}

@defproc[(sql-day-time-interval->seconds [interval sql-day-time-interval?])
         rational?]{

  Returns the length of @racket[interval] in seconds.
}

@defproc[(sql-interval->sql-time [interval sql-interval?]
                                 [failure any/c (lambda () (error ....))])
         any]{

  If @racket[interval] is a @racket[sql-day-time-interval] that
  represents a time of day, returns the corresponding
  @racket[sql-time] value. In particular, the following must be true:
  @itemlist[
  @item{@racket[hours], @racket[minutes], @racket[seconds], and
    @racket[nanoseconds] must all be non-negative}
  @item{@racket[hours] must be between @racket[0] and @racket[23]}
  ]
  The corresponding constraints on @racket[minutes], etc are
  enforced by the constructor.

  If @racket[interval] is out of range, the @racket[failure] value is
  called, if it is a procedure, or returned, otherwise.

  The @racket[sql-interval->sql-time] function can be used as a
  predicate for intervals representing times of day by passing
  @racket[#f] as the @racket[failure] argument.
}

@defproc[(sql-time->sql-interval [time sql-time?])
         sql-day-time-interval?]{

  Converts @racket[time] to an interval. If @racket[time] has
  time-zone information, it is ignored.
}
