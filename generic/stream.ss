;; Copyright 2007-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

#lang scheme/base
(require scheme/promise)

(provide stream:current+next
         stream:current
         stream:next
         stream:done?
         stream:new
         stream:force-to-end
         stream->list)

;; STREAMS (Message Generators)

(define-struct stream (lock unlock promise done?))

(define (stream:new lock unlock generator end?)
  (make-stream
   lock
   unlock
   (delay
     (let ([next-message (generator)])
       (values next-message
               (if (or (eof-object? next-message)
                       (end? next-message))
                   (make-stream #f #f #f #t)
                   (stream:new lock unlock generator end?)))))
   #f))

(define (stream:current+next mg)
  (dynamic-wind
   (stream-lock mg)
   (lambda () (stream:current+next/nosync mg))
   (stream-unlock mg)))

(define (stream:current+next/nosync mg)
  (force (stream-promise mg)))

(define (stream:current mg)
  (let-values [((current next) (stream:current+next mg))]
    current))

(define (stream:next mg)
  (let-values [((current next) (stream:current+next mg))]
    next))

(define (stream:done? mg)
  (or (not mg)
      (stream-done? mg)))

(define (stream:force-to-end mg)
  (unless (stream:done? mg)
    (stream:force-to-end (stream:next mg))))

(define (stream->list mg)
  (if (stream:done? mg)
      null
      (let-values ([(r mg) (stream:current+next mg)])
        (cons r (stream->list mg)))))
