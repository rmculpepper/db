#lang racket
(require "../mysql/main.ss")

(define c
  (connect #:socket "/var/run/mysqld/mysqld.sock"
           #:user "ryan"
           #:database "ryan"
           #:password "iwry;"))

(send c query "select * from the_letters")
(send c query "insert into the_letters values ('d', 4)")
(send c query "select count(*) from the_letters")
(send c query-list "select letter from the_letters")
(send c query-row "select * from the_letters where letter = 'a'")
(send c exec "delete from the_letters where letter = 'd'")

(define pst
  (send c prepare "select * from the_letters where letter > ?"))
(define stmt (send c bind-prepared-statement pst (list "b")))
(send c query stmt)
