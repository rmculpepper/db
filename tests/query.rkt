;; Copyright 2000-2011 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/unit
(require (for-syntax racket/base
                     "../generic/unstable-syntax.rkt")
         rackunit
         "config.rkt"
         "../generic/signatures.rkt"
         "../generic/main.rkt")
(import database^ config^)
(export test^)

(define-syntax-rule (with-connection c . body)
  (call-with-connection (lambda (c) . body)))

(define-syntax-rule (check-exn-fail expr)
  (check-exn exn:fail? (lambda () expr)))

(define (sql str)
  (case (dbsystem-name dbsystem)
    ((postgresql) str)
    ((mysql sqlite3) (regexp-replace* #rx"\\$[0-9]" str "?"))))

;; prep-mode:
;;   'string = query w/ string
;;   'prepare = query w/ prepared
;;   'bind = query w/ prepared+bound
;;   'function = prepared-query functions
(define-syntax (Q* stx)
  (syntax-case stx ()
    [(Q prep-mode function obj stmt arg ...)
     (with-syntax ([prepared (format-id #'function "prepare-~a" #'function)])
       #'(Q** prep-mode function prepared obj (sql stmt) (list arg ...)))]))
(define (Q** prep-mode function prepared obj stmt args)
  (case prep-mode
    ((string) (apply function obj stmt args))
    ((prepare) (apply function obj (prepare obj stmt) args))
    ((bind) (function obj (bind-prepared-statement (prepare obj stmt) args)))
    ((function) (apply (prepared obj stmt) args))
    (else 'Q* "bad prep-mode: ~e" prep-mode)))

(define (simple-tests prep-mode)

  (define-syntax-rule (Q obj function stmt arg ...)
    (Q* prep-mode function obj stmt arg ...))

  (test-suite (format "simple (~a)" prep-mode)

    (test-case "query-exec"
      (with-connection c
        (check-pred void? (Q c query-exec "insert into the_numbers values(-1, 'mysterious')"))
        (check-equal? (Q c query-value "select descr from the_numbers where N = -1")
                      "mysterious"))
      (with-connection c
        (check-pred void? (Q c query-exec "delete from the_numbers where N <> $1" 0))
        (check-equal? (Q c query-value "select count(*) from the_numbers")
                      1)
        (check-equal? (Q c query-list "select N from the_numbers")
                      (list 0))))

    (test-case "query-rows"
      (with-connection c
        (check set-equal?
               (Q c query-rows "select N, descr from the_numbers where N < 2")
               '(#(0 "nothing") #(1 "unity")))
        (check set-equal?
               (Q c query-rows "select N, descr from the_numbers where N < $1" 2)
               '(#(0 "nothing") #(1 "unity")))
        (check-exn-fail
         (Q c query-rows "insert into the_numbers values (13, 'baker')"))))

    (test-case "query-list"
      (with-connection c
        (check set-equal?
               (Q c query-list "select N from the_numbers")
               (map car test-data))
        (check set-equal?
               (Q c query-list "select N from the_numbers where N < $1" 2)
               '(0 1))
        (check set-equal?
               (Q c query-list "select N from the_numbers where N > $1 and N < $2" 1 4)
               '(2))))

    (test-case "query-row"
      (with-connection c
        (check-equal? (Q c query-row "select N, descr from the_numbers where N = 0")
                      '#(0 "nothing"))
        (check-equal? (Q c query-row "select N, descr from the_numbers where N = $1" 0)
                      '#(0 "nothing"))
        (check-exn-fail (Q c query-row "select N, descr from the_numbers where N = 100"))))

    (test-case "query-maybe-row"
      (with-connection c
        (check-equal? (Q c query-maybe-row "select N, descr from the_numbers where N = 0")
                      '#(0 "nothing"))
        (check-equal? (Q c query-maybe-row "select N, descr from the_numbers where N = $1" 0)
                      '#(0 "nothing"))
        (check-equal? (Q c query-maybe-row "select N, descr from the_numbers where N = 100")
                      #f)))

    (test-case "query-value"
      (with-connection c
        (check-equal? (Q c query-value "select N from the_numbers where N < 6 and N > 4")
                      5)
        (check-equal? (Q c query-value "select N from the_numbers where N < $1 and N > $2" 6 4)
                      5)
        (check-exn-fail (Q c query-value "select N from the_numbers where N > 100"))
        (check-exn-fail (Q c query-value "select N from the_numbers"))))

    (test-case "query-maybe-value"
      (with-connection c
        (check-equal? (Q c query-maybe-value "select N from the_numbers where N < 6 and N > 4")
                      5)
        (check-equal? (Q c query-maybe-value
                         "select N from the_numbers where N < $1 and N > $2" 6 4)
                      5)
        (check-equal? (Q c query-maybe-value "select N from the_numbers where N > 100")
                      #f)
        (check-exn-fail (Q c query-maybe-value "select N from the_numbers"))))))

(define (fold-tests)
  (test-suite "query-fold"
    (test-case "query-fold - sum"
      (with-connection c
        (let [(q (query-fold c "select N from the_numbers" + 0))]
          (check-equal? q (foldl + 0 (map car test-data))))))
    (test-case "query-fold/query-list - sum"
      (with-connection c
        (let [(q (query-fold c "select N from the_numbers" + 0))
              (q2 (query-list c "select N from the_numbers"))]
          (check-equal? q (foldl + 0 q2)))))
    (test-case "query-fold - max"
      (with-connection c
        (let [(q (query-fold c "select N from the_numbers where N > 0 order by N"
                             max -1000))]
          (check-equal? q (foldl max -1000 (map car test-data))))))))

(define low-level-tests
  (test-suite "low-level"
    (test-case "query-multiple"
      (with-connection c
        (let [(q (query-multiple c (list "select N from the_numbers" "select 5 as N")))]
          (check-pred list? q)
          (check-equal? 2 (length q))
          (check-true (andmap recordset? q))
          (check-true (andmap (lambda (r) (list? (recordset-data r))) q))
          (check-true (andmap (lambda (r) (list? (recordset-info r))) q))
          (check-true (andmap (lambda (v) (= (vector-length v) 1))
                              (recordset-data (car q))))
          (check-true (list? (recordset-info (cadr q))))
          (check-equal? (length (recordset-info (cadr q))) 1)
          (check-equal? (recordset-data (cadr q))
                        (list (vector 5)))
          (check-true 
           (set-equal? (map car test-data)
                       (map (lambda (v) (vector-ref v 0) )
                            (recordset-data (car q))))))))
    (test-case "query - select"
      (with-connection c
        (let [(q (query c "select N from the_numbers"))]
          (check-pred recordset? q)
          (check-true (set-equal? (map vector (map car test-data))
                                  (recordset-data q))))))
    (test-case "query - update"
      (with-connection c
        (let [(q (query c "update the_numbers set N = -1 where N = 1"))]
          (check-pred simple-result? q))))))

(define misc-tests
  (test-suite "misc correctness"
    (test-case "noninterference of nested queries"
      (with-connection c
        (define q
          (for/list ([a (query-list c
                           "select N from the_numbers where N > 0 and N < 3 order by N")])
            (query-value c 
             (format "select descr from the_numbers where N = ~s" a))))
        (define q2 
          (query-list c 
             "select descr from the_numbers where N > 0 and N < 3 order by N"))
        (check-equal? q q2)))
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
                   (for-each
                    (lambda (id)
                      (let/cc k
                        (set! k2 k1)
                        (printf "saw ~s~n" id)
                        (when (= id search-id)
                          (set! k1 k)
                          (printf "found ~s~n~n" id)
                          (return id))))
                    (query-list c "select N from the_numbers order by N asc"))
                   (error 'search-failed "couldn't find ~s" search-id)))]
           (unless (null? todo)
             (let [(t (car todo))]
               (set! todo (cdr todo))
               (t)))))))

    ;; Added 18 May 2003: Corrected a bug which incorrectly interleaved
    ;; nulls with returned fields.
    (test-case "nulls arrive in correct order"
      (with-connection c
        (check-equal? (query-row c "select null, 1, null")
                      (vector sql-null 1 sql-null))
        (check-equal? (query-row c "select 1, null")
                      (vector 1 sql-null))
        (check-equal? (query-row c "select null, 1")
                      (vector sql-null 1))
        (check-equal?
         (query-row c (string-append
                       "select 1, 2, 3, 4, null, 6, null, 8, 9, 10, 11, 12, null, "
                       "14, 15, null, null, 18, 19, 20, null, null, null, null, null, "
                       "null, 27, 28, 29, 30, null, 32, 33, null, 35"))
         (vector 1 2 3 4 sql-null 6 sql-null 8 9 10 11 12 sql-null 14 15 sql-null
                 sql-null 18 19 20 sql-null sql-null sql-null sql-null sql-null
                 sql-null 27 28 29 30 sql-null 32 33 sql-null 35))))))

(define error-tests
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
              (let ([stmt (bind-prepared-statement pst null)])
                (check-exn exn:fail? (lambda () (query c stmt))))))))
      (test-case "query errors - nonfatal"
        (with-connection c
          (check-exn exn:fail? (lambda () (query-value c "select nonsuch")))
          (check-equal? (query-value c "select 17") 17))))))


(define test
  (test-suite "query API"
    (simple-tests 'string)
    (simple-tests 'prepare)
    (simple-tests 'bind)
    (simple-tests 'function)
    (fold-tests)
    low-level-tests
    misc-tests
    error-tests))