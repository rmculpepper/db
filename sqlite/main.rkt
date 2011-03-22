;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         racket/contract
         "../generic/main.rkt"
         "connection.rkt"
         "ffi.rkt")
(provide/contract
 [connect
  (-> #:database (or/c string? path? bytes? 'memory 'temporary)
      any/c)])
(provide dbsystem)

(define (connect #:database path-or-sym)
  (let ([path
         (cond [(symbol? path-or-sym)
                (case path-or-sym
                  ;; Private, temporary in-memory
                  [(memory) #":memory:"]
                  ;; Private, temporary on-disk
                  [(temporary) #""])]
               [(relative-path? path-or-sym)
                (path->bytes (build-path (current-directory) path-or-sym))]
               [else
                (path->bytes path-or-sym)])])
    (let-values ([(db open-status)
                  (sqlite3_open path)])
      (handle-status 'sqlite3-connect open-status)
      (new connection% (db db)))))
