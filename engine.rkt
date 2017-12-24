#lang racket
(provide
 (contract-out
  [ignition!
   (->i ([r (and/c external-reactor? reactor-safe?)])
        [result any/c]
        #:post (r)
        (and (not (reactor-safe? r)) (reactor-ignited? r)))]
  [shutdown!
   (->i ([r (and/c external-reactor? reactor-ignited?)])
        [result any/c]
        #:post (r)
        (not (reactor-ignited? r)))]
  [queue-emission!
   (-> (and/c external-reactor? reactor-ignited?)
       (or/c pure-signal? (list/c value-signal? any/c)) ... any)]
  [bind-signal
   (-> (and/c external-reactor? reactor-ignited?) signal? evt? evt?)]))

(require "runtime.rkt" "compiler.rkt" "data.rkt")

;; maps external reactors to their control thread
(struct ignition-control (thread reactor))
(define ignition-threads (make-weak-hasheq))

;; ExternalReactor -> Void
(define (ignition! er)
  (define r* (external-reactor-internal er))
  (reactor-unsafe! er)
  
  (define td
    (thread
     (lambda ()
       (define r (make-external-reactor r*))
       (cycle! r))))
  
  (hash-set! ignition-threads er (ignition-control td r*)))

(define (cycle! r)
  (unless (or (reactor-done? r)
              (not (reactor-safe? r)))
    (define shutdown? #f)
    (define messages
      (let loop ([acc empty])
        (match (thread-try-receive)
          [#f acc]
          ['shutdown!
           (set! shutdown? #t)
           acc]
          [e (loop (append e acc))])))
    (apply react! r messages)
    (unless shutdown? (cycle! r))))


(define (reactor-ignited? r)
  (and (hash-ref ignition-threads r #f) #t))

(define (shutdown! r)
  (match-define (ignition-control td ir) (hash-ref ignition-threads r))
  (hash-remove! ignition-threads r)
  (thread-send td 'shutdown! #f)
  (thread-wait td)
  (inject! r ir))

(define (queue-emission! r . v)
  (thread-send
   (ignition-control-thread (hash-ref ignition-threads r))
   v #f))

(define (bind-signal r s evt)
  (define ic (hash-ref ignition-threads r))
  (replace-evt
   evt
   (lambda (v)
     (cond [(not (eq? ic (hash-ref ignition-threads r #f)))
            never-evt]
           [else
            (if (pure-signal? s)
                (queue-emission! r s)
                (queue-emission! r (list s v)))]))))