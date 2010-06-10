#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.ss")
@(require (for-label scheme/base)
          (for-label "../main.ss"))

@title{Annotated example}

The following program demonstrates how to connect to a PostgreSQL
backend and perform simple queries.

@schemeinput[
(require #, @(schememodname/this-package))
]

Replace these values with the appropriate values for your
configuration (see @scheme[postgresql-connect] for other connection
examples):

@schemeinput[
  (define psql-c
    (postgresql-connect #:user "ryan"
                        #:database "ryan"
                        #:password (getenv "DBPASSWORD")))
]
@(my-interaction
  [psql-c (new connection%)])

We'll do all our work with a temporary table. It will be dropped as
soon as we disconnect.

Use @scheme[query-exec] method when you want to execute a SQL
statement that doesn't return a recordset.

@schemeinput[
(query-exec psql-c
  "create temporary table the_numbers (n integer, description varchar(80))")
]
@schemeinput[
(query-exec psql-c
  "insert into the_numbers values (0, 'nothing')")
]

You can use @scheme[query-exec] to perform several queries at once.

@schemeinput[
(query-exec psql-c
  "insert into the_numbers (n, description) values (1, 'the loneliest number')"
  "insert into the_numbers values (2, 'the loneliest number since the number 1')")
]

The @scheme[query] procedure gives you the most information, but it's
the least pleasant to use.

@(my-interaction
   [(query psql-c
      "select n, description from the_numbers where n % 2 = 0")
    (make-Recordset
     (list (make-FieldInfo "n") (make-FieldInfo "description"))
     (list
      (vector 2 "the loneliest number since the number 1")
      (vector 0 "nothing")))])

If you know a query returns exactly one row, you can use
@scheme[query-row] to get just that row.

@(my-interaction
  [(query-row psql-c "select * from the_numbers where n = 0")
   (vector 0 "nothing")])

If you know that a query returns exactly one column, you can use
@scheme[query-list] to get just the list of values.

@(my-interaction
  [(query-list psql-c "select description from the_numbers order by n")
   (list "nothing"
         "the loneliest number"
         "the loneliest number since the number 1")])

If you know that a query returns just a single value (one row, 
one column), then you get use @scheme[query-value].

@(my-interaction
  [(query-value psql-c "select count(*) from the_numbers")
   3]
  [(query-value psql-c "select now()")
   (make-sql-timestamp 2008 4 1 12 34 56 0 #f)])

If you aren't sure whether a row exists, you can use
@scheme[query-maybe-row] or @scheme[query-maybe-value].

@(my-interaction
  [(query-maybe-row psql-c "select * from the_numbers where n = 1")
   (vector 1 "the loneliest number")]
  [(query-maybe-row psql-c "select * from the_numbers where n = 5")
   #f])

In the second example above, the @scheme[query-row] method would have
raised an error.

@(my-interaction
  [(query-maybe-value psql-c
     "select description from the_numbers where n = 5")
   #f])

The @scheme[query-value] method would have raised an error here, too.

Errors in queries are generally non-fatal.

@(my-interaction
  [(begin (with-handlers [(exn:fail?
                           (lambda (e) (printf "~a~n" (exn-message e))))]
            (query-value psql-c "select NoSuchField from NoSuchTable"))
          (query-value psql-c "select 'okay to proceed!'"))
   (begin (display "field 'NoSuchField' does not exist")
          "okay to proceed!")])

You can use the higher-order query methods to process results.

@schemeinput[
  (query-map psql-c "select n1.n, n2.n from the_numbers n1, the_numbers n2"
    (lambda (x y) (list x y)))]
@schemeblock[
   (code:comment "the cartesian product of {0,1,2} with itself")
]

@schemeinput[
  (query-mapfilter psql-c
    "select n1.n, n2.n from the_numbers n1, the_numbers n2"
    list
    (lambda (x y) (= 2 (+ x y))))]
@schemeblock[
   (code:comment "all ordered pairs from {0,1,2} that sum to 2")]

@(my-interaction
  [(query-fold psql-c "select n from the_numbers" + 0)
   3])

You can create parameterized queries and apply them to values later.

@schemeinput[
  (define all-less-than
    (prepare-query-list psql-c
      "select n from the_numbers where n < $1"))
]
@schemeinput[
  (define next-largest
    (prepare-query-maybe-value psql-c
      "select n from the_numbers where n < $1 order by n desc limit 1"))
]

@(my-interaction
  [(all-less-than 4)
   (list 0 1 2)]
  [(all-less-than 1)
   (list 0)]
  [(next-largest 4)
   2]
  [(next-largest 0)
   #f])

There's another way to do that, of course:

@schemeinput[
(define next-largest2
  (prepare-query-value psql-c
    "select max(n) from the_numbers where n < $1"))
]
@(my-interaction
  [(next-largest2 4)
   2])

But if there are no numbers less than the one given, @tt{max} returns NULL.

@(my-interaction
  [(next-largest2 0)
   sql-null])

If you like, you can declare cursors to fetch data incrementally.
Usually, you must be inside of a transaction to create a cursor.

@schemeinput[
(query-exec psql-c
  "begin transaction"
  "declare MC cursor for select * from the_numbers order by n"
  "move forward 1 in MC")
]
@(my-interaction
  [(query-row psql-c "fetch 1 in MC")
   (vector 1 "the loneliest number")])
@schemeinput[
(query-exec psql-c
  "close MC"
  "commit transaction")
]

You should disconnect when you're done to close the communication ports.

@schemeinput[
(disconnect psql-c)
]
