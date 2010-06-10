;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(require scribble/struct
         scribble/basic)
(provide & // tabbing)

(define-struct tabbing-marker (kind) #:transparent)

(define & (make-tabbing-marker '&))
(define // (make-tabbing-marker '//))

(define (tabbing #:spacing [spacing 4] . pres)
  (define (loop pres cell row rows sep)
    (cond [(null? pres)
           (let* ([row (cons (reverse cell) row)]
                  [rows (cons (reverse row) rows)])
             (reverse rows))]
          [(eq? (car pres) &)
           (loop (cdr pres) null (list* sep (reverse cell) row) rows sep)]
          [(eq? (car pres) //)
           (let* ([row (cons (reverse cell) row)])
             (loop (cdr pres) null null (cons (reverse row) rows) null))]
          [else
           (loop (cdr pres) (cons (car pres) cell) row rows sep)]))
  (define rows (loop pres null null null (list (hspace spacing))))
  (make-table #f (map layout-row rows)))

(define (layout-row row)
  (map layout-cell row))

(define (layout-cell cell)
  (make-flow (list (make-paragraph cell))))
