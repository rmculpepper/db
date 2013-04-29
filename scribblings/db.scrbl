#lang scribble/doc
@(require scribble/manual
          scribble/struct
          planet/scribble
          planet/version)

@(define (my-package-version)
   (format "~a.~a" (this-package-version-maj) (this-package-version-min)))

@title[#:version (my-package-version)]{DB (PLaneT)}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule/this-package[main]

The @racketmodname[db] library (@other-doc['(lib
"db/scribblings/db.scrbl")]) is now part of the Racket distribution.

This PLaneT package exists for backwards compatibility, but it just
reprovides the exports of the @racketmodname[db] library.
