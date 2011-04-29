;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/unit
(require racket/class
         rackunit
         "../base.rkt"
         "../util/connect.rkt"
         "config.rkt")
(import database^ config^)
(export test^)

(define NOISY? #f)

(define (sql str)
  (case (dbsystem-name dbsystem)
    ((postgresql) str)
    ((mysql sqlite3 odbc) (regexp-replace* #rx"\\$[0-9]" str "?"))
    (else (error 'sql "unsupported dbsystem: ~e" (dbsystem-name dbsystem)))))

(define (test-concurrency workers)
  (test-case (format "lots of threads (~s)" workers)
    (call-with-connection
     (lambda (c)
       (query-exec c "create temporary table play_numbers (n integer)")
       ;; transaction speeds up test by a factor of 6 on postgresql
       (query-exec c "begin")
       (for-each thread-wait
                 (map thread
                      (map (mk-worker c 100) (build-list workers add1))))))))

(define (((mk-worker c iterations) tid))
  (define insert-pst
    (prepare c (sql "insert into play_numbers (n) values ($1)")))
  (define (insert x) (query-exec c insert-pst x))
  (define (add-to-max n)
    (let* ([m (query-value c "select max(n) from play_numbers")]
           [m (if (equal? dbdb "test-sl") (string->number m) m)])
      (insert (+ n m))))
  (for-each insert (build-list iterations add1))
  (for-each add-to-max (build-list iterations add1))
  (when NOISY?
    (printf "~s: ~s\n"
            tid
            (query-value c "select max(n) from play_numbers"))))

(define (kill-safe-test proxy?)
  (test-case (format "kill-safe test~a" (if proxy? " (proxy)" ""))
    (call-with-connection
     (lambda (c0)
       (let ([c (if proxy?
                    (kill-safe-connection c0)
                    c0)])
         (query-exec c "delete from the_numbers")
         (for ([i (in-range 1000)])
           (query-exec c (sql "insert into the_numbers (n) values ($1)") i))
         (define (do-interactions)
           (for ([i (in-range 10)])
             (query-list c "select n from the_numbers")))
         (define threads (make-hasheq))

         (for ([i (in-range 20)])
           (let ([t (thread do-interactions)])
             (hash-set! threads (thread do-interactions) #t)
             (kill-thread t)))
         (for ([t (in-hash-keys threads)])
           (sync t)))))))

(define test
  (test-suite "Concurrency"
    ;; Tests whether connections are properly locked.
    (test-concurrency 1)
    (test-concurrency 2)
    (test-concurrency 20)
    (kill-safe-test #t)))