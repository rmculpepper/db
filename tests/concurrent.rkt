;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require rackunit
         racket/class
         racket/unit
         "../generic/functions.rkt"
         "../generic/signatures.rkt"
         "config.rkt")
(provide concurrent-test@)

(define-unit concurrent-test@
  (import database^ config^)
  (export test^)

  (define (sql pg my)
    (case (dbsystem-name dbsystem)
      ((postgresql) pg)
      ((mysql sqlite3) my)
      (else (error 'sql "unknown database system: ~e" dbsystem))))

  (define (make-slow-output-port out pause? limit)
    (make-output-port 'slow-port
                      out
                      (lambda (buf start end ok-buffer? ok-break?)
                        (when pause? (sleep 0.01))
                        (let ([end (min end (+ start limit))])
                          (write-bytes-avail buf out start end)))
                      (lambda () (close-output-port out))))

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
      (test-concurrency 20)

      (test-case "threads with small-chunk ports"
        ;; Tests whether connections work just as well if the connections
        ;; dribble bytes slowly.
        ;; Not sure this test is useful.
        (parameterize ((testing-connection-mixin
                        (lambda (%)
                          (class %
                            (define/override (attach-to-ports in out)
                              (super attach-to-ports
                                     in 
                                     (make-slow-output-port out #f 1)))
                            (super-new)))))
          (call-with-connection
           (lambda (c)
             (query-exec c "create temporary table play_numbers (n integer)")
             (for-each thread-wait
                       (map thread
                            (map (mk-worker c 5) (build-list 4 add1)))))))))))
