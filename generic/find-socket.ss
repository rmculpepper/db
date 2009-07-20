;; Copyright 2009 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(provide guess-socket-path/paths)

(define (guess-socket-path/paths function paths)
  (or (for/or ([path paths])
        (and (file-exists? path) path))
      (error function
             "automatic socket path search failed")))
