;; Copyright 2009-2010 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang racket/base
(provide format-symbol
         format-id)

;; Symbol Formatting

(define (format-symbol fmt . args)
  (define (convert x) (->atom x 'format-symbol))
  (check-restricted-format-string 'format-symbol fmt)
  (let ([args (map convert args)])
    (string->symbol (apply format fmt args))))

(define (format-id lctx
                   #:source [src #f]
                   #:props [props #f]
                   #:cert [cert #f]
                   fmt . args)
  (define (convert x) (->atom x 'format-id))
  (check-restricted-format-string 'format-id fmt)
  (let* ([args (map convert args)]
         [str (apply format fmt args)]
         [sym (string->symbol str)])
    (datum->syntax lctx sym src props cert)))
