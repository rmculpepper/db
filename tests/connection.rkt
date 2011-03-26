;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/unit
(require (for-syntax racket/base)
         rackunit
         "config.rkt"
         "../generic/main.rkt")
(import config^)
(export test^)

(define test
  (test-suite "managing connections"
    (test-case "connection?"
      (call-with-connection
       (lambda (c)
         (check-true (connection? c)))))
    (test-case "connected, disconnect"
      (call-with-connection
       (lambda (c)
         (check-true (connected? c))
         (disconnect c)
         (check-false (connected? c)))))
    (test-case "double disconnect okay"
      (call-with-connection
       (lambda (c)
         (disconnect c)
         (disconnect c))))
    (test-case "dbsystem"
      (call-with-connection
       (lambda (c)
         (let ([sys (connection-dbsystem c)])
           (check-true (dbsystem? sys))
           (check-pred symbol? (dbsystem-name sys))))))))
