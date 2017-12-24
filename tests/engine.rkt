#lang racket
(require reactor reactor/engine)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal S)
   (react! (prime (process (emit& S))))
   (check-eq? (sync/timeout 0 S) S)))