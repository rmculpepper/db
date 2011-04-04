;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(provide guess-socket-path/paths)

(define (guess-socket-path/paths function paths)
  (or (for/or ([path (in-list paths)])
        (and (file-exists? path) path))
      (error function
             "automatic socket path search failed")))
