#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal S empty #:gather append)
   (define r
     (prime
      (process
       (emit& S (begin pause& (par& 1 2))))))
   (react! r)
   (check-false (last? S))
   (react! r)
   (check-true (last? S))
   (check-equal? (last S) (list 1 2)))
  (test-begin
   (define-signal S empty #:gather append)
   (define-signal S2 empty #:gather append)
   (define r
     (prime
      (process
       (par&
        (emit& S (begin pause& (par& 1 2)))
        (emit& S2 (await& S [v (reverse v)]))))))
   (react! r)
   (check-false (last? S))
   (check-false (last? S2))
   (react! r)
   (check-true (last? S))
   (check-equal? (last S) (list 1 2))
   (check-false (last? S2))
   (react! r)
   (check-false (last? S))
   (check-true (last? S2))
   (check-equal? (last S2) (list 2 1)))

  (test-begin
   (define-signal susp)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (process
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
      (process
       (emit& O (abort& 1 #:after kill)))))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1))
  (test-begin
   (define-signal kill)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (process
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
      (process
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
      (process
       (emit& O (abort& halt& #:after kill [x x])))))
   (react! r (list kill 1))
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1))
  (test-begin
   (define-signal O #f #:gather +)
   (define r
     (prime
      (process
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
      (process
       (define-signal S)
       (emit& O
              (present& S 0 1)))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1)))