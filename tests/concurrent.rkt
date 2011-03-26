;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/unit
(require rackunit
         "../generic/functions.rkt"
         "../generic/signatures.rkt"
         "config.rkt")
(import database^ config^)
(export test^)

(define (sql pg my)
  (case (dbsystem-name dbsystem)
    ((postgresql) pg)
    ((mysql sqlite3) my)
    (else (error 'sql "unknown database system: ~e" dbsystem))))

(define (test-concurrency workers)
  (test-case (format "lots of threads (~s)" workers)
    (call-with-connection
     (lambda (c)
       (query-exec c "create temporary table play_numbers (n integer)")
       (for-each thread-wait
                 (map thread
                      (map (mk-worker c 100) (build-list workers add1))))))))

(define (((mk-worker c iterations) tid))
  (define insert
    (prepare-query-exec c
                        (sql "insert into play_numbers (n) values ($1)"
                             "insert into play_numbers (n) values (?)")))
  (define (add-to-max n)
    (insert (+ n (query-value c "select max(n) from play_numbers"))))
  (for-each insert (build-list iterations add1))
  (for-each add-to-max (build-list iterations add1))
  (printf "~s: ~s\n"
          tid
          (query-value c "select max(n) from play_numbers")))

(define test
  (test-suite "Concurrency"
    ;; Tests whether connections are properly locked.
    (test-concurrency 1)
    (test-concurrency 2)
    (test-concurrency 20)))
