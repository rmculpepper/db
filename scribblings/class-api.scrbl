#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.ss"
          (for-label scheme/base scheme/class scheme/contract)
          (for-label "../main.ss"))

@(define-syntax-rule (qmeth meth) (method connection:query<%> meth))
@(define-syntax-rule (pmeth meth) (method connection:query/prepare<%> meth))

@title{Class-based APIs}
@(declare-exporting/this-package (main) ())

Connections are @scheme[scheme/class] objects, and the connection
procedures described in @secref{query-api} are implemented as
methods. This section describes the object-based connection API.

Connection methods are divided into three interfaces:
@scheme[connection:admin<%>], @scheme[connection:query<%>], and
@scheme[connection:query/prepare<%>]. The connections for PostgreSQL
and MySQL servers support all three interfaces.

@section{Administrative interface}

@definterface[connection:admin<%> ()]{

This interface contains a connection's administrative
methods. Corresponds exactly to @scheme[connection:admin?].

@defmethod[(disconnect)
           void?]{
Method for @scheme[disconnect].
}

@defmethod[(connected?)
           boolean?]{
Method for @scheme[connected?].
}

@defmethod[(get-system)
           (is-a/c dbsystem<%>)]{
Method for @scheme[get-system].
}
}

@definterface[dbsystem<%> ()]{

This interface provides access to information about particular
database systems, their SQL dialects, and details about the database
package's implementation of their protocols.

@defmethod[(get-name)
           symbol?]{
Returns a symbol identifying the database system, such as
@scheme['postgresql] or @scheme['mysql].
}

@defmethod[(get-known-types)
           (listof symbol?)]{
Returns a list of the type names and aliases understood by connections
to this kind of database.
}
}

@section{Query interface}

@definterface[connection:query<%> ()]{

This interface contains a connection's main query methods. Corresponds
exactly to @scheme[connection:query?].

@defmethod[(query [stmt (unsyntax @techlink{Statement})])
           (unsyntax @techlink{QueryResult})]{
Method for @scheme[query].
}

@defmethod[(query-multiple [stmts (listof (unsyntax @techlink{Statement}))])
           (listof (unsyntax @techlink{QueryResult}))]{
Method for @scheme[query-multiple].
}

@defmethod[(exec [stmt (unsyntax @techlink{Statement})] ...)
           void?]{
Method for @scheme[query-exec]. Note the method name is simply
@qmeth[exec].
}

@defmethod[(query-rows [stmt (unsyntax @techlink{Statement})])
           (listof (vectorof _field))]{
Method for @scheme[query-rows].
}

@defmethod[(query-list [stmt (unsyntax @techlink{Statement})])
           (listof _field)]{
Method for @scheme[query-list].
}

@defmethod[(query-row [stmt (unsyntax @techlink{Statement})])
           (vectorof _field)]{
Method for @scheme[query-row].
}

@defmethod[(query-maybe-row [stmt (unsyntax @techlink{Statement})])
           (or/c (vectorof _field) false/c)]{
Method for @scheme[query-maybe-row].
}

@defmethod[(query-value [stmt (unsyntax @techlink{Statement})])
           _field]{
Method for @scheme[query-value].
}

@defmethod[(query-maybe-value [stmt (unsyntax @techlink{Statement})])
           (or/c _field false/c)]{
Method for @scheme[query-maybe-value].
}

@defmethod[(map [stmt (unsyntax @techlink{Statement})]
                [proc (_field _... -> _alpha)])
           (listof _alpha)]{
Method for @scheme[query-map]. Note the method name is simply
@qmeth[map].
}

@defmethod[(for-each [stmt (unsyntax @techlink{Statement})]
                     [proc (_field _... -> any)])
           void?]{
Method for @scheme[query-for-each]. Note the method name is simply
@qmeth[for-each].
}

@defmethod[(mapfilter [stmt (unsyntax @techlink{Statement})]
                      [map-proc (_field _... -> _alpha)]
                      [filter-proc (_field _... -> boolean?)])
           (listof _alpha)]{
Method for @scheme[mapfilter]. Note the method name is simply 
@qmeth[mapfilter].
}

@defmethod[(fold [stmt (unsyntax @techlink{Statement})]
                 [proc (_alpha _field _... -> _alpha)]
                 [init _alpha])
           _alpha]{

Method for @scheme[query-fold]. Note the method name is simply
@qmeth[fold].
}
}

@section{Prepared query interface}

@definterface[connection:query/prepare<%> (connection:query<%>)]{

@defmethod[(prepare [prep string?])
           (unsyntax @techlink{PreparedStatement})]{
Method for @scheme[prepare].
}

@defmethod[(prepare-multiple [preps (listof string?)])
           (listof (unsyntax @techlink{PreparedStatement}))]{
Method @scheme[prepare-multiple].
}

@defmethod[(bind-prepared-statement
            [pst (unsyntax @techlink{PreparedStatement})]
            [params (listof any/c)])
           (unsyntax @techlink{Statement})]{
Method for @scheme[bind-prepared-statement].
}

@defmethod[(prepare-exec [prep string?])
           (_param _... -> void?)]{
Method for @scheme[prepare-exec].
}

@defmethod[(prepare-query-rows [prep string?])
           (_param _... -> (listof (vectorof _field)))]{
Method for @scheme[prepare-query-rows].
}

@defmethod[(prepare-query-list [prep string?])
           (_param _... -> (listof _field))]{
Method for @scheme[prepare-query-list].
}

@defmethod[(prepare-query-row [prep string?])
           (_param _... -> (vectorof _field))]{
Method for @scheme[prepare-query-row].
}

@defmethod[(prepare-query-maybe-row [prep string?])
           (_param _... -> (or/c (vectorof _field) false?))]{
Method for @scheme[prepare-query-maybe-row].
}

@defmethod[(prepare-query-value [prep string?])
           (_param _... -> _field)]{
Method for @scheme[prepare-query-value].
}

@defmethod[(prepare-query-maybe-value [prep string?])
           (_param _... -> (or/c _field false?))]{
Method for @scheme[prepare-query-maybe-value].
}

@defmethod[(prepare-map [prep string?] [proc (_field _... -> _alpha)])
           (_param _... -> (listof _alpha))]{
Method for @scheme[prepare-map].
}

@defmethod[(prepare-for-each [prep string?] [proc (_field _... -> void?)])
           (_param _... -> void?)]{
Method for @scheme[prepare-for-each].
}

@defmethod[(prepare-mapfilter [prep string?]
                              [map-proc (_field _... -> _alpha)]
                              [filter-proc (_field _... -> boolean?)])
           (_param _... -> (listof _alpha))]{
Method for @scheme[prepare-mapfilter].
}

@defmethod[(prepare-fold [prep string?]
                         [proc (_alpha _field _... -> _alpha)]
                         [init _alpha])
           (_param _... -> _alpha)]{
Method for @scheme[prepare-fold].
}
}
