#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scribble/struct
          scheme/sandbox
          "config.ss")

@(require (for-label scheme/base)
          (for-label "../generic/main.ss")
          (for-label "../mysql/main.ss"))

@(define the-eval (make-base-eval))
@(interaction-eval #:eval the-eval
                   (require scheme/class
                            "generic/main.ss"
                            "mysql/main.ss"
                            "mysql/connection.ss"))
@(define-syntax-rule (examples/results [example result] ...)
   (examples #:eval the-eval (eval:alts example result) ...))
@(define-syntax-rule (my-interaction [example result] ...)
   (interaction #:eval the-eval (eval:alts example result) ...))

@title{Connecting to a MySQL server}

@;@defmodule/this-package["mysql/main.ss"]
@defmodule/this-package["mysql/main"]

Use the following procedure to create a connection:

@defproc[(connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port number? 3306]
                  [#:socket socket (or/c path? string? bytes? false/c) #f]
                  [#:password password (or/c string? false/c) #f]
                  [#:ssl ssl (symbols 'yes 'optional 'no) 'no])
         (and/c (is-a/c connection<%>)
                (is-a/c connection:query<%>)
                (is-a/c connection:query/prepare<%>))]{

  Creates a connection to a MySQL server. The meaning of the keyword
  arguments is similar to those of the @scheme[postgresql:connect]
  procedure.

  The default port for MySQL databases is 3306.

  @(examples/results
    [(connect #:server "db.mysite.com"
              #:port 3306
              #:database "webappdb"
              #:user "webapp"
              #:password "ultra5ecret")
     (new connection%)]
    [(connect #:user "me"
              #:database "me"
              #:password "icecream")
     (new connection%)]
    [(connect @code:comment{Typical socket path on some MySQL configurations}
              #:socket "/var/run/mysqld/mysqld.sock"
              #:user "me"
              #:database "me")
     (new connection%)])
}
