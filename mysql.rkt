;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (prefix-in mysql- "private/mysql/main.rkt"))
(provide mysql-connect
         mysql-guess-socket-path)
