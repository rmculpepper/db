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
    (init-private connection)

    (define req-channel (make-channel))

    (define safe-thread
      (thread/suspend-to-kill
       (lambda ()
         (let loop ()
           ((channel-get req-channel))
           (loop)))))

    (define/private (call proc)
      (thread-resume safe-thread)
      (let ([result #f]
            [sema (make-semaphore 0)])
        (channel-put req-channel
                     (lambda ()
                       (set! result
                             (with-handlers ([(lambda (e) #t)
                                              (lambda (e) (cons 'raise e))])
                               (cons 'values
                                     (call-with-values
                                         (lambda () (proc connection))
                                       list))))
                       (semaphore-post sema)))
        (semaphore-wait sema)
        (case (car result)
          ((values) (apply values (cdr result)))
          ((raise)  (raise (cdr result))))))

    (define-syntax-rule (define-forward (method arg ...) ...)
      (begin
        (define/public (method arg ...)
          (call (lambda (obj) (send obj method arg ...)))) ...))

    (define-forward
      (connected?)
      (disconnect)
      (get-dbsystem)
      (query fsym stmt collector)
      (prepare fsym stmt close-on-exec?)
      (free-statement stmt)
      (transaction-status fsym)
      (start-transaction fsym isolation)
      (end-transaction fsym mode))

    (super-new)))

;; ----

;; Connection generator

(define connection-generator%
  (class* object% (connection<%> no-cache-prepare<%>)
    (init-private generate      ;; called from client thread
                  get-key       ;; called from client thread
                  timeout)
    (super-new)

    (define custodian (current-custodian))

    (define req-channel (make-channel))

    ;; == methods called in manager thread ==

    ;; key=>conn : hasheq[key => connection]
    (define key=>conn (make-hasheq))

    ;; alarms : hasheq[connection => evt] (alarm wrapped to return key)
    (define alarms (make-hasheq))

    (define/private (get key) ;; also refreshes alarm
      (let ([c (hash-ref key=>conn key #f)])
        (when c (hash-set! alarms c (fresh-alarm-for key)))
        c))

    (define/private (put! key c)
      (hash-set! key=>conn key c)
      (hash-set! alarms c (fresh-alarm-for key)))

    (define/private (fresh-alarm-for key)
      (wrap-evt (alarm-evt (+ (current-inexact-milliseconds) timeout))
                (lambda (a) key)))

    (define/private (remove! key timeout?)
      ;; timeout? = if connection open, then wait longer
      (let* ([c (hash-ref key=>conn key #f)]
             [in-trans? (with-handlers ([exn:fail? (lambda (e) #f)])
                          (and c (send c transaction-status 'connection-generator)))])
        (cond [(not c) (void)]
              [(and timeout? in-trans?)
               (hash-set! alarms c (fresh-alarm-for key timeout))]
              [else
               (hash-remove! key=>conn key)
               (hash-remove! alarms c)
               (send c disconnect)])))

    (define/private (manage)
      (sync (handle-evt req-channel (lambda (proc) (proc)))
            (let ([keys (hash-map key=>conn (lambda (k v) k))])
              (handle-evt (apply choice-evt keys)
                          ;; Assignment to key has expired: move to idle or disconnect.
                          (lambda (key)
                            (remove! key #f))))
            (let ([alarm-evts (hash-map alarms (lambda (k v) v))])
              (handle-evt (apply choice-evt alarm-evts)
                          ;; Disconnect idle connection.
                          (lambda (key)
                            (remove! key #t)))))
      (manage))

    (define manager-thread
      (thread/suspend-to-kill (lambda () (manage))))

    ;; == methods called in client thread ==

    (define/private (get-connection create?)
      (thread-resume manager-thread)
      (let* ([key (get-key)]
             [c #f]
             [sema (make-semaphore 0)])
        (channel-put req-channel
                     (lambda ()
                       (set! c (get key create?))
                       (semaphore-post sema)))
        (semaphore-wait sema)
        (cond [(and c (send c connected?)) c]
              [create?
               (let ([c* (parameterize ((current-custodian custodian))
                           (generate))])
                 (channel-put req-channel
                              (lambda ()
                                (when c (remove! key #f))
                                (put! key c*)))
                 c*)]
              [else
               (when c  ;; got disconnected connection!
                 (channel-put req-channel (remove! key #f)))
               #f])))

    ;; ----

    (define-syntax-rule (define-forward (req-con? no-con (method arg ...)) ...)
      (begin (define/public (method arg ...)
               (let ([c (get-connection req-con?)])
                 (if c
                     (send c method arg ...)
                     no-con)))
             ...))

    (define-forward
      (#f #f     (connected?))
      (#t '_     (get-dbsystem))
      (#t '_     (query fsym stmt collector))
      (#t '_     (start-transaction fsym isolation))
      (#f (void) (end-transaction fsym mode))
      (#f #f     (transaction-status fsym)))

    (define/public (disconnect)
      (let ([c (get-connection #f)]
            [key (get-key)])
        (when c
          (send c disconnect)
          (thread-resume manager-thread)
          (channel-put req-channel
                       (lambda () (remove! key #f #f)))))
      (void))

    (define/public (prepare fsym stmt close-on-exec?)
      (unless close-on-exec?
        (error fsym "cannot prepare statement with connection-generator"))
      (send (get-connection #t) prepare fsym stmt close-on-exec?))

    (define/public (free-statement stmt)
      (error 'free-statement
             "internal error: connection-generator does not own statements"))))

;; ========================================

(define (kill-safe-connection connection)
  (new kill-safe-connection%
       (connection connection)))

(define (connection-generator generate
                              #:timeout [timeout #f])
  (let ([get-key (lambda () (thread-dead-evt (current-thread)))])
    (new connection-generator%
         (generate generate)
         (get-key (lambda () (thread-dead-evt (current-thread))))
         (timeout (* 1000 (or timeout +inf.0))))))

;; ----

(provide/contract
 [kill-safe-connection
  (-> connection? connection?)]
 [connection-generator
  (->* ((-> connection?))
       (#:timeout (and/c real? positive?))
       connection?)])
