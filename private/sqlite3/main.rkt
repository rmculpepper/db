;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require racket/class
         racket/contract
         "../generic/main.rkt"
         "../generic/check-access.rkt"
         "connection.rkt"
         "ffi.rkt")

;; FIXME: Contracts duplicated at db/main.rkt
(provide/contract
 [connect
  (-> #:database (or/c string? path? bytes? 'memory 'temporary)
      any/c)])
(provide dbsystem)

(define (connect #:database path-or-sym
                 #:mode [mode 'read/write])
  (let ([path
         (cond [(symbol? path-or-sym)
                (case path-or-sym
                  ;; Private, temporary in-memory
                  [(memory) #":memory:"]
                  ;; Private, temporary on-disk
                  [(temporary) #""])]
               [(or (path? path-or-sym) (string? path-or-sym))
                (let ([path (cleanse-path (path->complete-path path-or-sym))])
                  (scheme_security_check_file "sqlite3-connect"
                                              path
                                              (+ SCHEME_GUARD_FILE_READ
                                                 (case mode
                                                   ((read-only) 0)
                                                   (else SCHEME_GUARD_FILE_WRITE))))
                  path)])])
    (let-values ([(db open-status)
                  (sqlite3_open_v2 path
                                   (case mode
                                     ((read-only) SQLITE_OPEN_READONLY)
                                     ((read/write) SQLITE_OPEN_READWRITE)
                                     ((read/write/create)
                                      (+ SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE))))])
      (handle-status 'sqlite3-connect open-status)
      (new connection% (db db)))))