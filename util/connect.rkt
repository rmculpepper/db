;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/class
         "../private/generic/interfaces.rkt"
         (only-in "../private/generic/functions.rkt" connection?))

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

    (define/public (prepare fsym stmt close-on-exec?)
      (call (lambda (obj) (send obj prepare fsym stmt close-on-exec?))))

    (define/public (free-statement stmt)
      (call (lambda (obj) (send obj free-statement stmt))))

    (super-new)))

;; ----

;; Connection generator

(define connection-generator%
  (class* object% (connection<%> no-cache-prepare<%>)
    (init-private generate
                  get-key
                  fresh-alarm)
    (super-new)

    (define req-channel (make-channel))
    (define add-channel (make-channel))

    (define/private (get-connection create?)
      (thread-resume manager-thread)
      (let* ([key (get-key)]
             [b (box key)]
             [sema (make-semaphore 0)])
        (channel-put req-channel (cons b sema))
        (semaphore-wait sema)
        (let ([c (unbox b)])
          (cond [(and c (send c connected?)) c]
                [create?
                 (let ([c (generate)])
                   (channel-put add-channel (cons key c))
                   c)]
                [else #f]))))

    (define/private (remove-connection)
      (thread-resume manager-thread)
      (channel-put add-channel (cons (get-key) #f)))

    (define/private (manage)
      (define connection-table (make-hasheq)) ;; key => (cons alarm-evt connection)
      (define (fresh-alarm-for key)
        (wrap-evt (fresh-alarm) (lambda (a) key)))
      (define (put! key value)
        (let* ([alarm (fresh-alarm-for key)])
          (hash-set! connection-table key (cons alarm value))))
      (define (get key) ;; also refreshes alarm
        (let* ([value (hash-ref connection-table key #f)])
          (and value
               (let ([c (cdr value)]
                     [alarm (fresh-alarm-for key)])
                 (hash-set! connection-table key (cons alarm c))
                 c))))
      (let loop ()
        (let* ([keys (hash-map connection-table (lambda (k v) k))]
               [alarms (hash-map connection-table (lambda (k v) (car v)))])
          (sync (handle-evt req-channel
                            (lambda (box+sema)
                              (let* ([b (car box+sema)]
                                     [sema (cdr box+sema)]
                                     [key (unbox b)])
                                (set-box! b (get key))
                                (semaphore-post sema))))
                (handle-evt add-channel
                            (lambda (key+val)
                              (if (cdr key+val)
                                  (put! (car key+val) (cdr key+val))
                                  (hash-remove! connection-table (car key+val)))))
                (handle-evt (choice-evt (apply choice-evt keys)
                                        (apply choice-evt alarms))
                            (lambda (key)
                              (let ([c (hash-ref connection-table key #f)])
                                (hash-remove! connection-table key)
                                (when c (cleanup (cdr c)))))))
          (loop))))

    (define/private (cleanup c)
      ;; FIXME: If c is damaged (eg, custodian killed ports), then disconnect
      ;; might hang, so we spawn thread. This is not ideal.
      (thread (lambda () (send c disconnect))))

    (define manager-thread
      (thread/suspend-to-kill (lambda () (manage))))

    ;; ----

    (define/public (connected?)
      (let ([c (get-connection #f)])
        (and c (send c connected?))))

    (define/public (disconnect)
      (let ([c (get-connection #f)])
        (when c
          (send c disconnect)
          (remove-connection)))
      (void))

    (define/public (get-dbsystem)
      (send (get-connection #t) get-dbsystem))

    (define/public (query fsym stmt collector)
      (send (get-connection #t) query fsym stmt collector))

    (define/public (prepare fsym stmt close-on-exec?)
      (unless close-on-exec?
        (error fsym "cannot prepare statement with connection-generator"))
      (send (get-connection #t) prepare fsym stmt close-on-exec?))

    (define/public (free-statement stmt)
      (error 'free-statement
             "internal error: connection-generator does not own statements"))))

;; ----

(define (kill-safe-connection connection)
  (new kill-safe-connection%
       (connection connection)))

(define (connection-generator generate #:timeout [timeout #f])
  (new connection-generator%
       (generate generate)
       (get-key (lambda () (thread-dead-evt (current-thread))))
       (fresh-alarm (if timeout
                        (lambda () (alarm-evt (+ (current-inexact-milliseconds)
                                                 (* 1000 timeout))))
                        (lambda () never-evt)))))

(provide/contract
 [kill-safe-connection
  (-> connection? connection?)]
 [connection-generator
  (->* ((-> connection?)) (#:timeout (and/c rational? positive?))
       connection?)])
