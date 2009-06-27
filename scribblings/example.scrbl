#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          (planet cce/scheme:3)
          "config.ss")
@(require (for-label scheme/base)
          (for-label "../main.ss"))

@(define-syntax-rule (ameth meth) (method connection:admin<%> meth))
@(define-syntax-rule (qmeth meth) (method connection:query<%> meth))
@(define-syntax-rule (pmeth meth) (method connection:query/prepare<%> meth))

@title{Annotated example}

The following program demonstrates how to connect to a PostgreSQL
backend and perform simple queries.

@schemeinput[
(require #, @(schememodname/this-package)
         scheme/class)
]

Replace these values with the appropriate values for your 
configuration:
@schemeinput[
  (define cx
    (postgresql-connect #:user "ryan"
                        #:database "ryan"
                        #:password (getenv "DBPASSWORD")))
]
@(my-interaction
  [cx (new connection%)])

We'll do all our work with a temporary table. It will be dropped as
soon as we disconnect.

Use the @method[connection:query<%> exec] method when you want to execute a SQL
statement that doesn't return a recordset.

@schemeinput[
(send cx #, @qmeth[exec]
  "create temporary table the_numbers (n integer, description varchar(80))")
]
@schemeinput[
(send cx #, @qmeth[exec]
  "insert into the_numbers values (0, 'nothing')")
]

You can use @method[connection:query<%> exec] to perform several queries at once.

@schemeinput[
(send cx #, @qmeth[exec]
  "insert into the_numbers (n, description) values (1, 'unity')"
  "insert into the_numbers values (2, 'the loneliest number since the number 1')")
]

The @method[connection:query<%> query] method gives you the most information, but
it's the least pleasant to use.

@(my-interaction
   [(send cx #, @qmeth[query]
      "select n, description from the_numbers where n % 2 = 0")
    (make-Recordset
     (list (make-FieldInfo "n") (make-FieldInfo "description"))
     (list
      (vector 2 "the loneliest number since the number 1")
      (vector 0 "nothing")))])

If you know a query returns exactly one row, you can use
@method[connection:query<%> query-row] to get just that row.

@(my-interaction
  [(send cx #, @qmeth[query-row] "select * from the_numbers where n = 0")
   (vector 0 "nothing")])

If you know that a query returns exactly one column, you can use
@method[connection:query<%> query-list] to get just the list of values.

@(my-interaction
  [(send cx #, @qmeth[query-list] "select description from the_numbers order by n")
   (list "nothing" "unity" "the loneliest number since the number 1")])

If you know that a query returns just a single value (one row, 
one column), then you get use @method[connection:query<%> query-value].

@(my-interaction
  [(send cx #, @qmeth[query-value] "select count(*) from the_numbers")
   3]
  [(send cx #, @qmeth[query-value] "select now()")
   (make-sql-timestamp 2008 4 1 12 34 56 0 #f)])

If you aren't sure whether a row exists, you can use @method[connection:query<%>
query-maybe-row] or @method[connection:query<%> query-maybe-value].

@(my-interaction
  [(send cx #, @qmeth[query-maybe-row] "select * from the_numbers where n = 1")
   (vector 1 "unity")]
  [(send cx #, @qmeth[query-maybe-row] "select * from the_numbers where n = 5")
   #f])

In the second example above, the @method[connection:query<%> query-row] method
would have raised an error.

@(my-interaction
  [(send cx #, @qmeth[query-maybe-value]
     "select description from the_numbers where n = 5")
   #f])

The @method[connection:query<%> query-value] method would have raised an error
here, too.

Errors in queries are generally non-fatal.

@(my-interaction
  [(begin (with-handlers [(exn:fail?
                           (lambda (e) (printf "~a~n" (exn-message e))))]
            (send cx #, @qmeth[query-value] "select NoSuchField from NoSuchTable"))
          (send cx #, @qmeth[query-value] "select 'okay to proceed!'"))
   (begin (display "field 'NoSuchField' does not exist")
          "okay to proceed!")])

You can use the higher-order query methods to process results.

@schemeinput[
  (send cx #, @qmeth[map] "select n1.n, n2.n from the_numbers n1, the_numbers n2"
         (lambda (x y) (list x y)))]
@schemeblock[
   (code:comment "the cartesian product of {0,1,2} with itself")
]

@schemeinput[
  (send cx #, @qmeth[mapfilter]
    "select n1.n, n2.n from the_numbers n1, the_numbers n2"
    list
    (lambda (x y) (= 2 (+ x y))))]
@schemeblock[
   (code:comment "all ordered pairs from {0,1,2} that sum to 2")]

@(my-interaction
  [(send cx #, @qmeth[fold] "select n from the_numbers" + 0)
   3])

You can create parameterized queries and apply them to values later.

@schemeinput[
  (define all-less-than
    (send cx #, @pmeth[prepare-query-list]
      "select n from the_numbers where n < $1"))
]
@schemeinput[
  (define next-largest
    (send cx #, @pmeth[prepare-query-maybe-value]
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
  (send cx #, @pmeth[prepare-query-value]
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
(send cx #, @qmeth[exec]
  "begin transaction"
  "declare MC cursor for select * from the_numbers order by n"
  "move forward 1 in MC")
]
@(my-interaction
  [(send cx #, @qmeth[query-row] "fetch 1 in MC")
   (vector 1 "unity")])
@schemeinput[
(send cx #, @qmeth[exec]
  "close MC"
  "commit transaction")
]

You should disconnect when you're done to close the communication ports.

@schemeinput[
(send cx #, @ameth[disconnect])
]
