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

(define (restricted-format-string? fmt)
  (regexp-match? #rx"^(?:[^~]|~[aAn~%])*$" fmt))

(define (check-restricted-format-string who fmt)
  (unless (restricted-format-string? fmt)
    (raise-type-error who
                      "format string using only ~a placeholders"
                      fmt)))

(define (->atom x err)
  (cond [(string? x) x]
        [(symbol? x) x]
        [(identifier? x) (syntax-e x)]
        [(keyword? x) (keyword->string x)]
        [(number? x) x]
	[(char? x) x]
        [else (raise-type-error err
                                "string, symbol, identifier, keyword, character, or number"
                                x)]))
