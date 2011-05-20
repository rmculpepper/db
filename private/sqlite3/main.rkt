;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/contract
         "../generic/check-access.rkt"
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide sqlite3-connect
         (rename-out [dbsystem sqlite3-dbsystem]))

(define (sqlite3-connect #:database path-or-sym
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
                  (path->bytes path))])])
    (let-values ([(db open-status)
                  (sqlite3_open_v2 path
                                   (case mode
                                     ((read-only) SQLITE_OPEN_READONLY)
                                     ((read/write) SQLITE_OPEN_READWRITE)
                                     ((create)
                                      (+ SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE))))])
      (handle-status 'sqlite3-connect open-status db)
      (new connection% (db db)))))
