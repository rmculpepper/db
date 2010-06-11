;; Copyright 2000-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require (for-syntax racket/base)
         rackunit
         racket/class
         racket/unit
         "config.rkt"
         "../generic/main.rkt")
(provide query-test@)

(define (t? x) (and x #t))

(define-unit query-test@
  (import config^)
  (export test^)

  (define (query-tests use-prepared?)
    (define-syntax (sendq stx)
      (syntax-case stx ()
        [(sendq obj method . args)
         (with-syntax ([prepared
                        (datum->syntax #'method
                                       (string->symbol
                                        (format "~a-~a" "prepare" (syntax-e #'method))))])
           #'(if use-prepared?
                 ((prepared obj . args))
                 (method obj . args)))]))
    (test-suite (string-append "high-level" (if use-prepared? " (prepared)" ""))
      (test-case "query-list"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-list "select N from the_numbers"))]
             (check-pred list? q)
             (check-true (set-equal? q (map car test-data)))))))
      (test-case "query-row"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-row
                          "select N, description from the_numbers where N = 0"))]
             (check-pred vector? q)
             (check-equal? q 
                           (list->vector (assq 0 test-data)))))))
      (test-case "query-maybe-row - row"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-maybe-row
                          "select N, description from the_numbers where N = 0"))]
             (check-pred vector? q)
             (check-equal? q 
                           (list->vector (assq 0 test-data)))))))
      (test-case "query-maybe-row - none"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-maybe-row
                          "select N, description from the_numbers where N < 0"))]
             (check-equal? q #f)))))
      (test-case "query-value"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-value 
                          "select N from the_numbers where N < 6 and N > 4"))]
             (check-equal? q 5)))))
      (test-case "query-maybe-value - value"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-maybe-value
                          "select N from the_numbers where N < 6 and N > 4"))]
             (check-equal? q 5)))))
      (test-case "query-maybe-value - none"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-maybe-value
                          "select N from the_numbers where N > 1000"))]
             (check-equal? q #f)))))
      (test-case "query-map"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-map 
                          "select N, description from the_numbers where N < 2"
                          list))]
             (check-true 
              (set-equal? q
                          (filter (lambda (p) (< (car p) 2)) test-data)))))))
      (test-case "query-for-each"
        (call-with-connection
         (lambda (c)
           (define a null)
           (let ([q (sendq c query-for-each
                          "select N, description from the_numbers where N < 2"
                          (lambda (N description)
                            (set! a (cons (list N description) a))))])
             (check-true 
              (set-equal? a
                          (filter (lambda (p) (< (car p) 2)) test-data)))))))
      (test-case "query-mapfilter - odds"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-mapfilter 
                          "select N, description from the_numbers"
                          list
                          (lambda (n d) (odd? n))))]
             (check-true 
              (set-equal? q
                          (filter (lambda (p) (odd? (car p))) test-data)))))))
      (test-case "query-fold - sum"
        (call-with-connection 
         (lambda (c)
           (let [(q (sendq c query-fold "select N from the_numbers" + 0))]
             (check-equal? q
                           (foldl + 0 (map car test-data)))))))
      (test-case "query-fold/query-list - sum"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-fold "select N from the_numbers" + 0))
                 (q2 (sendq c query-list "select N from the_numbers"))]
             (check-equal? q (foldl + 0 q2))))))
      (test-case "query-fold - max"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-fold
                          "select N from the_numbers where N > 0 order by N"
                          max -1000))]
             (check-equal? q (foldl max -1000 (map car test-data)))))))
      (test-case "query-exec - insert"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-exec 
                          "insert into the_numbers values(-1, 'mysterious')"))]
             (check-equal? 
              (sendq c query-value
                    "select description from the_numbers where N = -1")
              "mysterious")))))
      (test-case "query-exec - delete"
        (call-with-connection
         (lambda (c)
           (let [(q (sendq c query-exec "delete from the_numbers where N <> 0"))]
             (check-equal? (sendq c query-value "select count(*) from the_numbers")
                           1)
             (check-equal? (sendq c query-list "select N from the_numbers")
                           (list 0))))))))


  (define test
    (test-suite "query API"
      (test-suite "low-level"
        (test-case "query-multiple"
          (call-with-connection
           (lambda (c)
             (let [(q (query-multiple c
                                      (list "select N from the_numbers"
                                            "select 5 as N")))]
               (check-pred list? q)
               (check-equal? 2 (length q))
               (check-true (andmap Recordset? q))
               (check-true (andmap (lambda (r) (list? (Recordset-data r))) q))
               (check-true (andmap (lambda (r) (list? (Recordset-info r))) q))
               (check-true (andmap (lambda (v) (= (vector-length v) 1))
                                   (Recordset-data (car q))))
               (check-true (list? (Recordset-info (cadr q))))
               (check-equal? (length (Recordset-info (cadr q))) 1)
               ;; (postgresql returns "N", mysql returns "N")
               #| (check-equal? (Recordset-info (cadr q))
                                (list (make-FieldInfo "N"))) |#
               (check-equal? (Recordset-data (cadr q))
                             (list (vector 5)))
               (check-true 
                (set-equal? (map car test-data)
                            (map (lambda (v) (vector-ref v 0) )
                                 (Recordset-data (car q)))))))))
        (test-case "query - select"
          (call-with-connection
           (lambda (c)
             (let [(q (query c "select N from the_numbers"))]
               (check-pred Recordset? q)
               (check-true (set-equal? (map car test-data)
                                       (map (lambda (v) (vector-ref v 0))
                                            (Recordset-data q))))))))
        (test-case "query - update"
          (call-with-connection
           (lambda (c)
             (let [(q (query c "update the_numbers set N = -1 where N = 1"))]
               (check-pred SimpleResult? q))))))

      (query-tests #f)
      (query-tests #t)

      (test-suite "misc correctness"
        (test-case "noninterference of nested queries"
          (call-with-connection
           (lambda (c)
             (define q
               (query-map c
                          "select N from the_numbers where N > 0 and N < 3 order by N"
                          (lambda (a)
                            (query-value c 
                                  (format "select description from the_numbers where N = ~s" a)))))
             (define q2 
               (query-list c 
                           "select description from the_numbers where N > 0 and N < 3 order by N"))
             (check-equal? q q2))))
        (test-case "continuation safety"
          (call-with-connection
           (lambda (c)
             (let* [(search-id 1)
                    (k1 #f)
                    (k2 #f)
                    (todo (list 
                           (lambda ()
                             (set! search-id 4)
                             (k1 #t))
                           (lambda ()
                             (set! search-id 2)
                             (k2 #t))
                           (lambda ()
                             (set! search-id 6)
                             (k1 #t))
                           (lambda ()
                             (set! search-id 5)
                             (k2 #t))))
                    (q 
                     (let/cc return
                       (query-for-each c "select N from the_numbers order by N asc"
                             (lambda (id)
                               (let/cc k
                                 (set! k2 k1)
                                 (printf "saw ~s~n" id)
                                 (when (= id search-id)
                                   (set! k1 k)
                                   (printf "found ~s~n~n" id)
                                   (return id)))))
                       (error 'search-failed "couldn't find ~s" search-id)))]
               (unless (null? todo)
                 (let [(t (car todo))]
                   (set! todo (cdr todo))
                   (t)))))))
        
        ;; Added 18 May 2003: Corrected a bug which incorrectly interleaved
        ;; nulls with returned fields.
        (test-case "nulls arrive in correct order"
          (call-with-connection
           (lambda (c)
             (check-equal? (query-row c "select null, 1, null")
                           (vector sql-null 1 sql-null))
             (check-equal? (query-row c "select 1, null")
                           (vector 1 sql-null))
             (check-equal? (query-row c "select null, 1")
                           (vector sql-null 1))
             (check-equal?
              (query-row c 
                    "select 1, 2, 3, 4, null, 6, null, 8, 9, 10, 11, 12, null, 14, 15, null, null, 18, 19, 20, null, null, null, null, null, null, 27, 28, 29, 30, null, 32, 33, null, 35")
              (vector 1 2 3 4 sql-null 6 sql-null 8 9 10 11 12 sql-null 14 15 sql-null sql-null 18 19 20 sql-null sql-null sql-null sql-null sql-null sql-null 27 28 29 30 sql-null 32 33 sql-null 35))))))
      
      ;; ERRORS
      (test-suite "Errors"
        (test-suite "low-level"
          (test-case "query-multiple - not a statement list"
            (with-connection c
              (check-exn exn:fail? (lambda () (query-multiple c 5)))
              (check-exn exn:fail? (lambda () (query-multiple c "select 5")))
              (check-exn exn:fail? (lambda () (query-multiple c (list 5))))))
          (test-case "query - not a statement"
            (with-connection c
              (check-exn exn:fail? (lambda () (query c 5)))))
          (test-case "query - multiple statements in string"
            (with-connection c
              (check-exn exn:fail? (lambda () (query c "select 3; select 4;")))))
          (test-case "query - unowned prepared stmt"
            (with-connection c1 
              (with-connection c
                (printf "connections: ~s, ~s\n" c1 c)
                (let ([pst (prepare c1 "select 5")])
                  (printf "prepared stmt: ~s\n" pst)
                  (check-exn exn:fail? (lambda () (bind-prepared-statement c pst null)))
                  (let ([stmt (bind-prepared-statement c1 pst null)])
                    (check-exn exn:fail? (lambda () (query c stmt))))))))
          (test-case "query errors - nonfatal"
            (with-connection c
              (check-exn exn:fail? (lambda () (query-value c "select nonsuch")))
              (check-equal? (query-value c "select 17") 17)))
          )))))
