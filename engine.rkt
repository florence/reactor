#lang racket
(provide
 (contract-out
  [ignition!
   (-> (and/c external-reactor? reactor-safe?) #:type (or/c 'always 'on-queue) any)]
  [queue-reaction!
   (-> (and/c (and/c reactor? reactor-ignited?)
              reactor-ignited?) any)]
  [queue-emission!
   (-> (and/c reactor? reactor-ignited?)
       (or/c pure-signal? (list/c value-signal? any/c)) ... any)]))

(require "runtime.rkt" "compiler.rkt" "data.rkt")

;; maps external reactors to their control thread
(define ignition-threads (make-weak-hasheq))

;; ExternalReactor (or/c 'always 'on-queue) -> IgnitionControl
(define (ignition! er #:type type)
  (define r* (external-reactor-internal er))
  (reactor-unsafe! r #f)
  (define r (make-external-reactor r*))
  
  (define td
    (thread
     (lambda ()
       (let loop ()
         (cond
           [(or (reactor-done? r)
                (not (reactor-safe? r)))
            (void)]
           [else
            (cycle! r type)
            (loop)])))))
  
  (hash-set! ignition-threads er td))

(define (cycle! r type)
  (define messages
    (match r
      ['always (get-messages!)]
      ['on-queue (append (thread-receive) (get-messages!))]))
  (apply react! messages))

(define (get-messages!)
  (let loop ([acc empty])
    (define n (thread-try-receive))
    (if (not n) acc (loop (append n acc)))))

(define (reactor-ignited? r)
  (and (hash-ref ignition-threads r #f) #t))

;; Reactor -> Void
;; request a reaction. Idempotent if one is already queued
(define (queue-reaction! r)
  (queue-emission! r))

;; Reactor . (or/c Signal (list/c Signal Any)) -> Void
;; queue a reaction with this signal emission
(define (queue-emission! r . v)
  (thread-send (hash-ref ignition-threads r) v))



