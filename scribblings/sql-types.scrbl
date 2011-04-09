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
[(query-value pgc "select false") #f]
[(query-value pgc "select 1 + $1" 2) 3]
]


@section{Database system type correspondences}

This sections describes the correspondences between SQL types and
Racket types for the supported database systems. 

@subsection[#:tag "postgresql-types"]{PostgreSQL}

The following table lists the PostgreSQL types known to this library,
along with their corresponding Racket representations.

@tabbing{
  @bold{PostgreSQL type} @& @bold{Aliases}      @& @bold{Racket type} @//
  @tt{boolean}       @& @tt{bool}               @& @scheme[boolean?] @//
  @tt{char1}         @& @tt{}                   @& @scheme[char?] @//
  @tt{smallint}      @& @tt{int2}               @& @scheme[exact-integer?] @//
  @tt{integer}       @& @tt{int int4}           @& @scheme[exact-integer?] @//
  @tt{bigint}        @& @tt{int8}               @& @scheme[exact-integer?] @//
  @tt{real}          @& @tt{float4}             @& @scheme[real?] @//
  @tt{double}        @& @tt{float8}             @& @scheme[real?] @//
  @tt{decimal}       @& @tt{numeric}            @& @scheme[number?] @//
  @tt{character}     @& @tt{char bpchar}        @& @scheme[string?] @//
  @tt{varchar}       @& @tt{}                   @& @scheme[string?] @//
  @tt{text}          @& @tt{}                   @& @scheme[string?] @//
  @tt{bytea}         @& @tt{}                   @& @scheme[bytes?] @//
  @tt{date}          @& @tt{}                   @& @scheme[sql-date?] @//
  @tt{time}          @& @tt{}                   @& @scheme[sql-time?] @//
  @tt{timetz}        @& @tt{}                   @& @scheme[sql-time?] @//
  @tt{timestamp}     @& @tt{}                   @& @scheme[sql-timestamp?] @//
  @tt{timestamptz}   @& @tt{}                   @& @scheme[sql-timestamp?] @//
  @tt{interval}      @& @tt{}                   @& @scheme[sql-interval?] @//
  @tt{oid}           @& @tt{}                   @& @scheme[exact-integer?]
}

The @tt{char1} type, written @tt{"char"} in PostgreSQL's SQL syntax
(the quotation marks are significant), is always one byte, essentially
a tiny integer written as a character.

A SQL value of type @tt{numeric} is always converted to either an
exact rational or @scheme[+nan.0]. When converting Scheme values to
SQL @tt{numeric}, exact rational values representable by finite
decimal strings are converted without loss of precision. (Precision
may be lost, of course, if the value is then stored in a database
field of lower precision.) Other real values are converted to decimals
with a loss of precision.

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

Array types are not currently supported.

@examples/results[
[(query-value pgc "select point (1,2)") "(1,2)"]
[(query-value pgc "select '{1,2,3}'::int[]") "{1,2,3}"]
]


@subsection[#:tag "mysql-types"]{MySQL}

The following table lists the MySQL types known to this package, along
with their corresponding Racket representations.

@tabbing{
  @bold{MySQL type}  @& @bold{Aliases}                   @& @bold{Racket type} @//
  @tt{integer}       @& @tt{int}                         @& @scheme[exact-integer?] @//
  @tt{tinyint}       @& @tt{}                            @& @scheme[exact-integer?] @//
  @tt{smallint}      @& @tt{}                            @& @scheme[exact-integer?] @//
  @tt{mediumint}     @& @tt{}                            @& @scheme[exact-integer?] @//
  @tt{bigint}        @& @tt{biginteger}                  @& @scheme[exact-integer?] @//
  @tt{real}          @& @tt{float}                       @& @scheme[real?] @//
  @tt{double}        @& @tt{}                            @& @scheme[real?] @//
  @tt{decimal}       @& @tt{numeric}                     @& @scheme[number?] @//
  @tt{varchar}       @& @tt{}                            @& @scheme[string?] @//
  @tt{var-string}    @& @tt{}                            @& @scheme[string?], but see below @//
  @tt{date}          @& @tt{}                            @& @scheme[sql-date?] @//
  @tt{time}          @& @tt{}                            @& @scheme[sql-simple-interval?] @//
  @tt{datetime}      @& @tt{}                            @& @scheme[sql-timestamp?]
@;{FIXME: blob types?}
}

MySQL does not report specific parameter types for prepared queries,
instead assigning them the type @tt{var-string}. Consequently,
conversion of Racket values to @tt{var-string} parameters accepts, in
addition to strings, numbers (@racket[rational?]---no infinities or
NaN) and SQL date/time structures (@racket[sql-date?],
@racket[sql-time?], and @racket[sql-timestamp?]).

A SQL value of type @tt{decimal} is always converted to an exact
rational (MySQL seems not to support infinite @tt{decimal} values).

Note that in MySQL, the @tt{time} type represents time intervals,
which may not correspond to times of day (for example, the interval
may be negative).


@subsection[#:tag "sqlite-types"]{SQLite}

The following table lists the SQLite types known to this package,
along with their corresponding Racket representations.

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

SQLite does not report specific parameter and result types for
prepared queries. Instead, they are assigned the pseudotype
@tt{any}. Conversion of Racket values to @tt{any} parameters accepts
strings, bytes, and real numbers. 

An exact integer that cannot be represented as a 64-bit signed integer
is converted as @tt{real}, not @tt{integer}.

@examples/results[
[(query-value slc "select ?" (expt 2 80))
 1.2089258196146292e+24]
]


@subsection[#:tag "odbc-types"]{ODBC}

The following table lists the ODBC types known to this package,
along with their corresponding Racket representations.

@tabbing{
  @bold{ODBC type}  @& @bold{Racket type} @//
  @tt{character}    @& @scheme[string?] @//
  @tt{varchar}      @& @scheme[string?] @//
  @tt{longvarchar}  @& @scheme[string?] @//
  @tt{numeric}      @& @scheme[rational?] @//
  @tt{decimal}      @& @scheme[rational?] @//
  @tt{integer}      @& @scheme[exact-integer?] @//
  @tt{tinyint}      @& @scheme[exact-integer?] @//
  @tt{smallint}     @& @scheme[exact-integer?] @//
  @tt{bigint}       @& @scheme[exact-integer?] @//
  @tt{float}        @& @scheme[real?] @//
  @tt{real}         @& @scheme[real?] @//
  @tt{double}       @& @scheme[real?] @//
  @tt{date}         @& @scheme[sql-date?] @//
  @tt{time}         @& @scheme[sql-time?] @//
  @tt{datetime}     @& @scheme[sql-timestamp?] @//
  @tt{timestamp}    @& @scheme[sql-timestamp?] @//
  @tt{binary}       @& @scheme[bytes?] @//
  @tt{varbinary}    @& @scheme[bytes?] @//
  @tt{longvarbinary}@& @scheme[bytes?] @//
  @tt{bit1}         @& @scheme[boolean?]
}

Not all ODBC drivers provide parameter type information for prepared
queries. In such situations the connection assigns the parameter the
pseudotype @tt{unknown}. Conversion of Racket values to @tt{unknown}
parameters accepts strings, bytes, numbers (@racket[rational?]---no
infinities or NaN) and SQL date/time structures (@racket[sql-date?],
@racket[sql-time?], and @racket[sql-timestamp?]).

The ODBC type @tt{bit1} always represents a single bit, unlike the
standard SQL @tt{bit(N)} type.

@;{----------------------------------------}

@section[#:tag "sql-data"]{SQL data}

SQL @tt{NULL} is always translated into the unique @scheme[sql-null]
value.

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
  @item{@racket[hours], @racket[minutes], @racket[seconds], and
    @racket[nanoseconds] all have the same sign}
  @item{@racket[minutes] and @racket[seconds] range from @racket[-59]
    to @racket[59]} 
  @item{@racket[nanoseconds] ranges from
    @racket[(- (sub1 #, @racketvalfont{#e1e9}))] to
  @racket[(sub1 #, @racketvalfont{#e1e9})]}
  ]
}

@defproc[(sql-day-time-interval? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[sql-interval] value
  where the @racket[years], @racket[months], and @racket[days]
  fields are zero.
}

@defproc[(sql-year-month-interval? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[sql-interval] value
  where the @racket[days], @racket[hours], @racket[minutes],
  @racket[seconds], and @racket[nanoseconds] fields are zero.
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

@defproc[(sql-day-time-interval->seconds [interval sql-day-time-interval?])
         rational?]{

  Returns the length of @racket[interval] in seconds.
}
