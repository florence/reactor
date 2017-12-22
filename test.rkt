#lang racket
(require "main.rkt")
(module+ test
  (require rackunit)
  (test-begin
   (define g (prime (process (void))))
   (check-false (reactor-done? g))
   (check-not-exn (lambda () (react! g)))
   (check-true (reactor-done? g))
   (check-not-exn (lambda () (react! g)))
   (check-true (reactor-done? g))
   (check-not-exn (lambda () (react! g))))
  (test-begin
   (define r (prime (process halt&)))
   (check-false (reactor-done? r))
   (react! r)
   (check-false (reactor-done? r))
   (react! r)
   (check-false (reactor-done? r))
   (react! r)
   (check-false (reactor-done? r))
   (react! r)
   (check-false (reactor-done? r)))
  
  (test-begin
   (define done #f)
   (define p1
     (process
      pause&
      (set! done #t)))
   (define grp (prime p1))
   (react! grp)
   (check-false done)
   (react! grp)
   (check-true done))
  
  (test-begin
   (define present #f)
   (define absent #f)
   (define p1
     (process
      (signal S
        (present& S (set! present #t) (set! absent #t)))))
   (define grp (prime p1))
   (react! grp)
   (check-false present)
   (check-false absent)
   (react! grp)
   (check-false present)
   (check-true absent))

  (test-begin
   (define found #f)
   (define g1
     (prime
      (process
       (signal S
         (par&
          (begin pause& (emit& S))
          (begin (await& #:immediate S) (set! found #t)))))))
   (react! g1)
   (check-false found)
   (react! g1)
   (check-true found))
  (test-begin
   (define found #f)
   (define g1
     (prime
      (process
       (define-signal S)
       (par&
        (begin pause& (emit& S))
        (begin (await& #:immediate S) (set! found #t))))))
   (react! g1)
   (check-false found)
   (react! g1)
   (check-true found))
  (test-begin
   (define-signal O1)
   (define-signal O2)
   (define r
     (prime
      (process
       (par& (emit& O1) (emit& O2)))))
   (react! r)
   (check-true (last? O1))
   (check-true (last? O2)))
  (test-begin
   (define-signal O1)
   (define-signal O2)
   (define r
     (prime
      (process
       (par& (begin pause& (emit& O1))
             (begin pause& (emit& O2))))))
   (react! r)
   (check-false (last? O1))
   (check-false (last? O2))
   (react! r)
   (check-true (last? O1))
   (check-true (last? O2)))
  (test-begin
   (define-signal I)
   (define-signal O)
   (define r
     (prime
      (process
       (await& I)
       (emit& O))))
   (react! r)
   (check-false (last? O))
   (react! r I)
   (check-true (last? O)))

  (test-begin
   (define res #f)
   (define r
     (prime
      (process
       (define-signal S 0 #:gather +)
       (par&
        (emit& S 1)
        (await&
         S
         [n (set! res n)])))))
   (react! r)
   (check-false res)
   (react! r)
   (check-equal? res 1))
  (test-begin
   (define res #f)
   (define r
     (prime
      (process
       (define-signal S 0 #:gather +)
       (par&
        (emit& S 1)
        (emit& S 2)
        (await&
         S
         [n (set! res n)])))))
   (react! r)
   (check-false res)
   (react! r)
   (check-equal? res 3))
  (test-begin
   (define res #f)
   (define-process (add& S)
     (emit& S (add1 (last S)))
     pause&
     (run& (add& S)))
   (define r
     (prime
      (process
       (signal ([S 0 #:gather +])
         (par&
          (run& (add& S))
          (await&
           S
           [(? (lambda (x) (> x 3))) (set! res #t)]))))))
   (react! r)
   (check-false res)
   (react! r)
   (check-false res)
   (react! r)
   (check-false res)
   (react! r)
   (check-false res)
   (react! r)
   (check-true res))


  (test-begin
   (define seen #f)
   (define-process (await-when)
     (define-signal act)
     (define-signal S)
     (par&
      (suspend&
       (await& #:immediate S) (set! seen #t)
       #:unless act)
      (begin (emit& act) pause&           pause& (emit& act))
      (begin             pause& (emit& S) pause& (emit& S))))
   (define r (prime (await-when)))
   (react! r)
   (check-false seen)
   (react! r)
   (check-false seen)
   (react! r)
   (check-true seen))
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
   (check-true (last? O))))
            
