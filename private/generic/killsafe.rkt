;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "interfaces.rkt")
(provide kill-safe-connection%)

;; Kill-safe wrapper

;; Note: wrapper protects against kill-thread, but not from
;; custodian-shutdown of ports, etc.

(define kill-safe-connection%
  (class* object% (connection<%>)
    (init connection)

    (define req-channel (make-channel))

    (define safe-thread
      (thread/suspend-to-kill
       (lambda ()
         (let loop ()
           (let* ([req (channel-get req-channel)]
                  [proc (car req)]
                  [return-box (cadr req)]
                  [return-sema (caddr req)])
             (set-box! return-box
                       (with-handlers ([(lambda (e) #t)
                                        (lambda (e) (cons 'raise e))])
                         (cons 'values
                               (call-with-values (lambda () (proc connection)) list))))
             (semaphore-post return-sema)
             (loop))))))

    (define (call proc)
      (thread-resume safe-thread)
      (let ([return-box (box #f)]
            [return-sema (make-semaphore 0)])
        (channel-put req-channel (list proc return-box return-sema))
        (semaphore-wait return-sema)
        (let ([result (unbox return-box)])
          (case (car result)
            ((values)
             (apply values (cdr result)))
            ((raise)
             (raise (cdr result)))))))

    (define/public (connected?)
      (call (lambda (obj) (send obj connected?))))

    (define/public (disconnect)
      (call (lambda (obj) (send obj disconnect))))

    (define/public (get-dbsystem)
      (call (lambda (obj) (send obj get-dbsystem))))

    (define/public (query fsym stmt collector)
      (call (lambda (obj) (send obj query fsym stmt collector))))

    (define/public (prepare fsym stmt)
      (call (lambda (obj) (send obj prepare fsym stmt))))

    (define/public (free-statement stmt)
      (call (lambda (obj) (send obj free-statement stmt))))

    (super-new)))
