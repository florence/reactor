#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal S empty #:gather append)
   (define r
     (prime
      (thunk
       (begin (par& 1 2) (emit& S 1)))))
   (react! r)
   (check-true (last? S))
   (check-equal? (last S) 1))
  

  (test-begin
   (define-signal susp)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (thunk
       (emit& O (suspend& 1 #:unless susp)))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r susp)
   (check-true (last? O))
   (check-equal? (last O) 1))
  (test-begin
   (define-signal kill)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (thunk
       (emit& O (abort& 1 #:after kill)))))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1))
  (test-begin
   (define-signal kill)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (thunk
       (emit& O (abort& 1 #:after kill)))))
   (react! r kill)
   (check-true (last? O))
   (check-equal? (last O) 1)
   (react! r)
   (check-false (last? O)))
  (test-begin
   (define-signal kill)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (thunk
       (emit& O (abort& halt& #:after kill)))))
   (react! r kill)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) (void)))
  (test-begin
   (define-signal kill 0 #:gather +)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (thunk
       (emit& O (abort& halt& #:after kill [x x])))))
   (react! r (list kill (list 1)))
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1))
  (test-begin
   (define-signal O #f #:gather +)
   (define r
     (prime
      (thunk
       (define-signal S)
       (emit& S)
       (emit& O
              (present& S 0 1)))))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 0)
   (react! r)
   (check-false (last? O)))
  (test-begin
   (define-signal O #f #:gather +)
   (define r
     (prime
      (thunk
       (define-signal S)
       (emit& O
              (present& S 0 1)))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1)))