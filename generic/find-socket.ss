
#lang scheme/base
(provide guess-socket-path/paths)

(define (guess-socket-path/paths function paths)
  (or (for/or ([path paths])
        (and (file-exists? path) path))
      (error function
             "automatic socket path search failed")))
