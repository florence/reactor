#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal A)
   (define-signal B)
   (define-signal R)
   (define-signal O)
   (define-process ABRO
     (par& (await& #:immediate A) (await& #:immediate B))
     (emit& O)
     halt&)
   (define r (prime ABRO))
   (react! r A)
   (check-false (last? O))
   (react! r B)
   (check-true (last? O))
   (react! r R)
   (check-false (last? O)))
  (test-begin
   (define-signal A)
   (define-signal B)
   (define-signal R)
   (define-signal O)
   (define-process ABRO
     (abort&
      (par& (await& #:immediate A) (await& #:immediate B))
      (emit& O)
      halt&
      #:after R))
   (define r (prime ABRO))
   (react! r A)
   (check-false (last? O))
   (react! r B)
   (check-true (last? O))
   (react! r R)
   (check-false (last? O)))
  (test-begin
   (define-signal A)
   (define-signal B)
   (define-signal R)
   (define-signal O)
   (define-process ABRO
     (loop&
      (abort&
       (par& (await& #:immediate A) (await& #:immediate B))
       (emit& O)
       halt&
       #:after R)))
   (define r (prime ABRO))
   (react! r A)
   (check-false (last? O))
   (react! r B)
   (check-true (last? O))
   (react! r R)
   (check-false (last? O))
   (react! r A B)
   (check-true (last? O))
   (react! r A B)
   (check-false (last? O))
   (react! r R A B)
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r A B)
   (check-true (last? O)))
  (test-begin
   (define-signal S)
   (define-signal O)
   (define-process I/O
     (suspend&
      (emit& O)
      #:unless S))
   (define r (prime I/O))
   (react! r)
   (check-false (last? O)))
  (test-begin
   (define-signal S)
   (define-signal O)
   (define-process I/O
     (suspend&
      (emit& O)
      #:unless S))
   (define r (prime I/O))
   (react! r S)
   (check-true (last? O)))
  (test-begin
   (define-signal S1)
   (define-signal S2)
   (define-signal O)
   (define-process I/O
     (suspend&
      (suspend&
       (emit& O)
       #:unless S2)
      #:unless S1))
   (define r (prime I/O))
   (react! r)
   (check-false (last? O))
   (react! r S1)
   (check-false (last? O))
   (react! r S2)
   (check-false (last? O))
   (react! r S1 S2)
   (check-true (last? O)))
  (define-process (suspend-loop O Ss)
    (let loop ([i Ss])
      (cond
        [(empty? i) (emit& O)]
        [else
         (suspend&
          (loop (rest i))
          #:unless (first i))])))
  (test-begin
   (define-signal O)
   (define-signal S1)
   (define-signal S2)
   (define r (prime (suspend-loop O (list S1 S2))))
   (react! r S1)
   (check-false (last? O))
   (react! r S2)
   (check-false (last? O))
   (react! r S1 S2)
   (check-true (last? O)))
  (test-begin
   (define-signal O)
   (define-signal S1)
   (define-signal S2)
   (define-signal S3)
   (define r (prime (suspend-loop O (list S1 S2 S3))))
   (react! r S1 S2)
   (check-false (last? O))
   (react! r S1 S2)
   (check-false (last? O))
   (react! r S1 S2)
   (check-false (last? O))
   (react! r S3)
   (check-false (last? O))
   (react! r S3)
   (check-false (last? O))
   (react! r S1 S2 S3)
   (check-true (last? O)))

  (test-begin
   (define-signal kill 0 #:gather +)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (process
       (emit& O (abort& halt& #:after kill [1 1])))))
   (react! r (list kill 1))
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1))
  (test-begin
   (define-signal kill 0 #:gather +)
   (define-signal O 0 #:gather +)
   (define r
     (prime
      (process
       (emit& O (abort& halt& #:after kill [2 2])))))
   (react! r (list kill 1))
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r (list kill 2))
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 2)))
            
