#lang racket
(require reactor rackunit)


(test-begin
 (define (p1)
   (with-handlers& 
    (react! r2)
    #:after-error [void raise]))
 (define (p2)
   (loop& (void)))

 (define r1 (prime p1))
 (define r2 (prime p2))

 (check-exn (regexp (regexp-quote "loop& terminated in a single instant!"))
            (lambda ()
              (react! r1)
              (react! r1))))


(test-begin
 (define handler #f)
 (define-signal S)
 (define (p1)
   (set! handler (lambda (x) (emit& S))))
 (define (p2)
   (loop& (void)))

 (define r1 (prime p1))
 (react! r1)
 (define r2 (prime p2))

 (check-exn (regexp (regexp-quote "reactive code escaped reactor context"))
            (lambda ()
                (call-with-exception-handler
                 (lambda (x) (with-handlers ([void values]) (handler x)))
                 (lambda () (react! r2))))))