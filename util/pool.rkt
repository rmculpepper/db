;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/class
         "../private/generic/interfaces.rkt"
         (only-in "../private/generic/functions.rkt" connection?))

(define connection-pool%
  (class* object% ()
    (init-private generate              ;; called from manager thread
                  max-connections
                  max-idle-connections)
    (super-new)

    ;; max-connections is either in [1, 10000] or +inf.0,
    ;; if leave-evt is sema, then counter = (max-connections - assigned connections)
    ;; ie, includes idle connections
    (define lease-evt
      (if (= max-connections +inf.0)
          always-evt
          (make-semaphore max-connections)))

    (define req-channel (make-channel))
    (define debug? #f)

    ;; == methods called in manager thread ==

    ;; proxy=>evt : hasheq[proxy-connection => evt]
    (define proxy=>evt (make-hasheq))

    ;; idle-list : (listof raw-connection)
    (define idle-list null)

    (define/private (lease* key)
      (when debug?
        (eprintf "leasing connection (~a)\n" (if (pair? idle-list) "idle" "new")))
      (let* ([raw-c
              (cond [(pair? idle-list)
                     (begin0 (car idle-list)
                       (set! idle-list (cdr idle-list)))]
                    [else (generate)])]
             [c (new proxy-connection% (pool this) (connection raw-c))])
        (hash-set! proxy=>evt c key)
        c))

    (define/private (release* proxy raw-c)
      (when debug?
        (eprintf "releasing connection (~a)\n"
                 (if (< (length idle-list) max-idle-connections) "idle" "disconnect")))
      (hash-remove! proxy=>evt proxy)
      (when raw-c
        (with-handlers ([exn:fail? void])
          (send raw-c end-transaction 'connection-pool 'rollback))
        (cond [(< (length idle-list) max-idle-connections)
               (set! idle-list (cons raw-c idle-list))]
              [else (send raw-c disconnect)])
        (when (semaphore? lease-evt) (semaphore-post lease-evt))))

    (define/private (new-connection)
      (let ([c (generate)])
        (when (or (hash-ref proxy=>evt c #f) (memq c idle-list))
          (uerror 'connection-pool "connect function did not produce a fresh connection"))
        c))

    (define/private (manage)
      (sync (handle-evt req-channel (lambda (proc) (proc)))
            (let ([evts (hash-map proxy=>evt (lambda (k v) (wrap-evt v (lambda (e) k))))])
              (handle-evt (apply choice-evt evts)
                          (lambda (proxy)
                            (release* proxy (send proxy release-connection))))))
      (manage))

    (define manager-thread
      (thread/suspend-to-kill (lambda () (manage))))

    ;; == methods called in client thread ==

    (define/public (lease key)
      (wrap-evt lease-evt
                (lambda (_e)
                  (thread-resume manager-thread)
                  (let* ([result #f]
                         [sema (make-semaphore 0)])
                    (channel-put req-channel
                                 (lambda ()
                                   (set! result (lease* key))
                                   (semaphore-post sema)))
                    (semaphore-wait sema)
                    result))))

    (define/public (release proxy)
      (thread-resume manager-thread)
      (let ([raw-c (send proxy release-connection)])
        (channel-put req-channel (lambda () (release* proxy raw-c))))
      (void))

    (define/public (debug ?)
      (set! debug? (eq? ? #t))
      (when ?
        (eprintf "pool status: ~a assigned, ~a idle\n"
                 (hash-count proxy=>evt) (length idle-list))))))

;; --

(define proxy-connection%
  (class* locking% (connection<%>)
    (init-private connection
                  pool)
    (inherit call-with-lock)
    (super-new)

    (define-syntax-rule (define-forward defmethod (method arg ...) ...)
      (begin
        (defmethod (method arg ...)
          (call-with-lock 'method
            (lambda ()
              (let ([c connection])
                (unless c (error/not-connected 'method))
                (send c method arg ...)))))
        ...))

    (define-forward define/public
      (get-dbsystem)
      (query fsym stmt collector)
      (prepare fsym stmt close-on-exec?)
      (free-statement stmt)
      (transaction-status fsym)
      (start-transaction fsym isolation)
      (end-transaction fsym mode))

    ;; (define-forward define/override (connected?))
    (define/override (connected?) (and connection #t))

    (define/public (disconnect)
      (send pool release this))

    (define/public (release-connection)
      (begin0 connection
        (set! connection #f)))))

;; ----


(define (connection-pool connect
                         #:max-connections [max-connections +inf.0]
                         #:max-idle-connections [max-idle-connections 2])
  (new connection-pool%
       (generate connect)
       (max-connections max-connections)
       (max-idle-connections max-idle-connections)))

(define (connection-pool? x)
  (is-a? x connection-pool%))

(define (connection-pool-lease pool [key (current-thread)])
  (let* ([key
          (cond [(thread? key) (thread-dead-evt key)]
                [(custodian? key) (make-custodian-box key #t)]
                [else key])]
         [result (sync/timeout 0.1 (send pool lease key))])
    (unless result
      (uerror 'connection-pool-lease
              "cannot obtain connection; connection pool limit reached"))
    result))

;; ----

(provide/contract
 [connection-pool
  (->* ((-> connection?))
       (#:max-connections (or/c (integer-in 1 10000) +inf.0)
        #:max-idle-connections (or/c (integer-in 1 10000) +inf.0))
       connection-pool?)]
 [connection-pool?
  (-> any/c boolean?)]
 [connection-pool-lease
  (->* (connection-pool?)
       ((or/c custodian? evt?))
       connection?)])
