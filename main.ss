;; Copyright 2009 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require (prefix-in postgresql: "postgresql.ss")
         (prefix-in mysql: "mysql.ss"))
(provide postgresql:connect
         mysql:connect)

