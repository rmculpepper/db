#lang racket/base
(require racket/contract)

#|
inet, cidr = family:byte bits:byte is_cidr:byte addrlen:byte addr:be-integer
  is_cidr is ignored

box = x1 y1 x2 y2 (all float8)
circle = x y rad (all float8)
line = not yet implemented
lseg = x1 y1 x2 y2 (all float8)
path = closed?:byte #points:int4 (x y : float8)*
point = x y (all float8)
polygon = #points:int4 (x y : float8)*
|#

(struct sql-point (x y)
        #:transparent
        #:guard (lambda (x y _n)
                  (values (exact->inexact x)
                          (exact->inexact y))))

(struct sql-box (ne sw)
        #:transparent
        #:guard (lambda (ne sw _n)
                  (let ([x1 (sql-point-x ne)]
                        [x2 (sql-point-x sw)]
                        [y1 (sql-point-y ne)]
                        [y2 (sql-point-y sw)])
                    (values (sql-point (max x1 x2) (max y1 y2))
                            (sql-point (min x1 x2) (min y1 y2))))))

(struct sql-circle (center radius)
        #:transparent
        #:guard (lambda (center radius _n)
                  (values center (exact->inexact radius))))

(struct sql-lseg (p1 p2)
        #:transparent)

(struct sql-path (closed? points)
        #:transparent
        #:guard (lambda (closed? points _n)
                  (values (and closed? #t)
                          points)))

(struct sql-polygon (points)
        #:transparent)

(provide/contract
 [struct sql-point ([x real?] [y real?])]
 [struct sql-box ([ne sql-point?] [sw sql-point?])]
 [struct sql-circle ([center sql-point?] [radius (and/c real? (not/c negative?))])]
 [struct sql-lseg ([p1 sql-point?] [p2 sql-point?])]
 [struct sql-path ([closed? any/c] [points (listof sql-point?)])]
 [struct sql-polygon ([points (listof sql-point?)])])
