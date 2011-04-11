#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt")

@title[#:tag "introduction"]{Introduction}

The following annotated program demonstrates how to connect to a
database and perform simple queries. Some of the SQL syntax is
PostgreSQL-specific, such as the syntax of query parameters.

@my-interaction[
[#, @(my-require-form)
 (void)]
]

First we create a connection. Replace @racket[_user], @racket[_db],
and @racket[_password] below with the appropriate values for your
configuration (see @secref{creating-connections} for other connection examples):

@my-interaction[
[(define pgc
   (postgresql-connect #:user _user
                       #:database _db
                       #:password _password))
 (void)]
[pgc
 (new connection%)]
]

Use @racket[query-exec] method to execute a SQL statement for effect.

@my-interaction[
[(query-exec pgc
  "create temporary table the_numbers (n integer, description varchar(80))")
 (void)]
[(query-exec pgc
  "insert into the_numbers values (0, 'nothing')")
 (void)]
[(query-exec pgc
  "insert into the_numbers values (1, 'the loneliest number')")
 (void)]
[(query-exec pgc
  "insert into the_numbers values (2, 'company')")
 (void)]
]

The @racket[query] function is a more general way to execute a
statement. It returns a structure encapsulating information about the
statement's execution. (But some of that information varies from
system to system and is subject to change.)

@my-interaction[
[(query pgc
  "insert into the_numbers values (3, 'a crowd')")
 (simple-result '((command insert 0 1)))]
[(query pgc
  "select n, description from the_numbers where n % 2 = 0")
 (recordset
  (list
   '((name . "n") (typeid . 23))
   '((name . "description") (typeid . 1043)))
  '(#(0 "nothing") #(2 "company")))]
]

When the query is known to return a recordset and when the field
descriptions are not needed, it is more convenient to use the
@racket[query-rows] function.

@my-interaction[
[(query-rows pgc
  "select n, description from the_numbers where n % 2 = 0")
 '(#(0 "nothing") #(2 "company"))]
]

Use @racket[query-row] for queries that are known to return a
recordset of exactly one row.

@my-interaction[
[(query-row pgc "select * from the_numbers where n = 0")
 (vector 0 "nothing")]
]

Similarly, use @racket[query-list] for queries that produce a
recordset of exactly one column.

@my-interaction[
[(query-list pgc "select description from the_numbers order by n")
 (list "nothing" "the loneliest number" "company" "a crowd")]
]

When a query is known to return a single value (one row containing one
column), use @racket[query-value].

@my-interaction[
[(query-value pgc "select count(*) from the_numbers")
 4]
[(query-value pgc "select description from the_numbers where n = 5")
 (error 'query-value
        "query returned zero rows: ~s"
        "select description from the_numbers where n = 5")]
]

When a query may return zero or one rows, as the last example, use
@racket[query-maybe-row] or @racket[query-maybe-value] instead.

@my-interaction[
[(query-maybe-value pgc
  "select description from the_numbers where n = 5")
 #f]
]

Errors in queries are usually non-fatal.

@my-interaction[
[(begin (with-handlers [(exn:fail?
                         (lambda (e) (printf "~a~n" (exn-message e))))]
          (query-value pgc "select NoSuchField from NoSuchTable"))
        (query-value pgc "select 'okay to proceed!'"))
 (begin (display "query-value: relation \"nosuchtable\" does not exist (SQL code 42P01)")
        "okay to proceed!")]
]

Queries may contain parameters. The easiest way to execute a
parameterize query is to provide the parameters ``inline'' after the
SQL statement in the query function call.

@my-interaction[
[(query-value pgc
  "select description from the_numbers where n = $1" 2)
 "company"]
[(query-list pgc
  "select n from the_numbers where n > $1 and n < $2" 0 3)
 (list 1 2)]
]

Alternatively, a parameterized query may be prepared in advance and
executed later. @tech{Prepared statements} can be executed multiple
times with different parameter values.

@my-interaction[
[(define get-less-than-pst
   (prepare pgc "select n from the_numbers where n < $1"))
 (void)]
[(query-list pgc get-less-than-pst 1)
 (list 0)]
[(query-list pgc (bind-prepared-statement get-less-than-pst 2))
 (list 0 1)]
]

A prepared statement is tied to the connection used to create it;
attempting to use it with another connection results in an
error. Unfortunately, in some scenarios, such as web servlets, the
lifetimes of connections are short or difficult to track, making
prepared statements inconvenient. In such cases, a better tool is the
@tech{statement generator}, which prepares statements on demand and
caches them for future use with the same connection.

@my-interaction[
[(define get-less-than-pst
   (statement-generator "select n from the_numbers where n < $1"))
 (void)]
[(code:line (query-list pgc1 get-less-than-pst 1) (code:comment "prepares statement for pgc1"))
 (list 0)]
[(code:line (query-list pgc2 get-less-than-pst 2) (code:comment "prepares statement for pgc2"))
 (list 0 1)]
[(code:line (query-list pgc1 get-less-than-pst 3) (code:comment "uses existing prep. stmt."))
 (list 0 1 2)]
]

When a connection's work is done, it should be disconnected.

@my-interaction[
[(disconnect pgc)
 (void)]
]
