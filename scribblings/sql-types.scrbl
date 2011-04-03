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
along with their corresponding Racket representations.

@tabbing{
  @bold{PostgreSQL type} @& @bold{Aliases}      @& @bold{Racket type} @//
  @tt{bool}          @& @tt{boolean}            @& @scheme[boolean?] @//
  @tt{char1}         @& @tt{}                   @& @scheme[char?] @//
  @tt{int2}          @& @tt{smallint}           @& @scheme[exact-integer?] @//
  @tt{int4}          @& @tt{integer int}        @& @scheme[exact-integer?] @//
  @tt{int8}          @& @tt{bigint}             @& @scheme[exact-integer?] @//
  @tt{float4}        @& @tt{real}               @& @scheme[real?] @//
  @tt{float8}        @& @tt{double}             @& @scheme[real?] @//
  @tt{numeric}       @& @tt{decimal}            @& @scheme[number?] @//
  @tt{serial4}       @& @tt{serial}             @& @scheme[exact-integer?] @//
  @tt{serial8}       @& @tt{bigserial}          @& @scheme[exact-integer?] @//
  @tt{bpchar}        @& @tt{character}          @& @scheme[string?] @//
  @tt{varchar}       @& @tt{}                   @& @scheme[string?] @//
  @tt{bytea}         @& @tt{}                   @& @scheme[bytes?] @//
  @tt{date}          @& @tt{}                   @& @scheme[sql-date?] @//
  @tt{text}          @& @tt{}                   @& @scheme[string?] @//
  @tt{time}          @& @tt{}                   @& @scheme[sql-time?] @//
  @tt{timetz}        @& @tt{}                   @& @scheme[sql-time?] @//
  @tt{timestamp}     @& @tt{}                   @& @scheme[sql-timestamp?] @//
  @tt{timestamptz}   @& @tt{}                   @& @scheme[sql-timestamp?] @//
  @tt{oid}           @& @tt{}                   @& @scheme[exact-integer?]
}

The type name @tt{bpchar} corresponds to the standard SQL blank-padded
string type written @tt{character} and abbreviated @tt{char}. The type
@tt{char1} is a one-byte integer type written @tt{"char"} in PostgreQL
syntax (the quotation marks are significant).

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
  @tt{int}           @& @tt{integer}                     @& @scheme[exact-integer?] @//
  @tt{tinyint}       @& @tt{}                            @& @scheme[exact-integer?] @//
  @tt{smallint}      @& @tt{}                            @& @scheme[exact-integer?] @//
  @tt{mediumint}     @& @tt{}                            @& @scheme[exact-integer?] @//
  @tt{biginteger}    @& @tt{bigint}                      @& @scheme[exact-integer?] @//
  @tt{float}         @& @tt{real}                        @& @scheme[real?] @//
  @tt{double}        @& @tt{}                            @& @scheme[real?] @//
  @tt{decimal}       @& @tt{numeric}                     @& @scheme[number?] @//
  @tt{varchar}       @& @tt{}                            @& @scheme[string?] @//
  @tt{var-string}    @& @tt{}                            @& @scheme[string?], but see below @//
  @tt{date}          @& @tt{}                            @& @scheme[sql-date?] @//
  @tt{time}          @& @tt{}                            @& @scheme[sql-time?] @//
  @tt{datetime}      @& @tt{}                            @& @scheme[sql-timestamp?]
@;{FIXME: blob types?}
}

MySQL does not report specific parameter types for prepared queries,
instead assigning them the type @tt{var-string}. Consequently,
conversion of Racket values to @tt{var-string} parameters accepts, in
addition to strings, numbers (@racket[rational?]---no infinities or
NaN) and SQL date/time structures (@racket[sql-date?],
@racket[sql-time?], and @racket[sql-timestamp?]).

A SQL value of type @tt{decimal} (aka @tt{numeric}) is always
converted to an exact rational (MySQL seems not to support infinite
@tt{decimal} values).


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
  @tt{char}         @& @scheme[string?] @//
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
  @tt{bit}          @& @scheme[boolean?]
}

Not all ODBC drivers provide parameter type information for prepared
queries. In such situations the connection assigns the parameter the
pseudotype @tt{unknown}. Conversion of Racket values to @tt{unknown}
parameters accepts strings, bytes, numbers (@racket[rational?]---no
infinities or NaN) and SQL date/time structures (@racket[sql-date?],
@racket[sql-time?], and @racket[sql-timestamp?]).


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
