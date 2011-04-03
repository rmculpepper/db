;; Copyright 2009-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require (prefix-in postgresql- "postgresql/main.rkt"))
(provide postgresql-connect
         postgresql-guess-socket-path)
