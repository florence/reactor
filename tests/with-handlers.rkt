#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (process
       (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
         (error "hi")))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) "hi")))