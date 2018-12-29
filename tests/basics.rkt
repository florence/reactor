#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define g (prime (thunk (void))))
   (check-false (reactor-done? g))
   (check-not-exn (lambda () (react! g)))
   (check-true (reactor-done? g))
   (check-not-exn (lambda () (react! g)))
   (check-true (reactor-done? g))
   (check-not-exn (lambda () (react! g))))
  (test-begin
   (define r (prime (thunk halt&)))
   (check-false (reactor-done? r))
   (check-false (reactor-suspended? r))
   (react! r)
   (check-false (reactor-done? r))
   (check-true (reactor-suspended? r))
   (react! r)
   (check-false (reactor-done? r))
   (check-true (reactor-suspended? r))
   (react! r)
   (check-false (reactor-done? r))
   (check-true (reactor-suspended? r))
   (react! r)
   (check-false (reactor-done? r))
   (check-true (reactor-suspended? r)))
  
  (test-begin
   (define done #f)
   (define p1
     (thunk
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
     (thunk
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
      (thunk
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
      (thunk
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
      (thunk
       (par& (emit& O1) (emit& O2)))))
   (react! r)
   (check-true (last? O1))
   (check-true (last? O2)))
  (test-begin
   (define-signal O1)
   (define-signal O2)
   (define r
     (prime
      (thunk
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
      (thunk
       (await& I)
       (emit& O))))
   (react! r I)
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r I)
   (check-true (last? O)))

  (test-begin
   (define res #f)
   (define r
     (prime
      (thunk
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
      (thunk
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
   (define (add& S)
     (emit& S (add1 (last S)))
     pause&
     (add& S))
   (define r
     (prime
      (thunk
       (signal ([S 0 #:gather +])
         (par&
          (add& S)
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
   (define (await-when)
     (define-signal act)
     (define-signal S)
     (par&
      (suspend&
       (await& #:immediate S) (set! seen #t)
       #:unless act)
      (begin (emit& act) pause&           pause& (emit& act))
      (begin             pause& (emit& S) pause& (emit& S))))
   (define r (prime await-when))
   (react! r)
   (check-false seen)
   (react! r)
   (check-false seen)
   (react! r)
   (check-true seen))

  (test-begin
   (define-signal b)
   (define r (prime (thunk (emit& b 1))))
   (check-exn #rx"Expected value carrying signal" (lambda () (react! r)))
   (check-false (reactor-safe? r)))
  (test-begin
   (define-signal b 0 #:gather +)
   (define r (prime (thunk (emit& b))))
   (check-exn #rx"Expected pure signal" (lambda () (react! r)))
   (check-false (reactor-safe? r)))
  (test-begin
   (define f void)
   (define r
     (prime (thunk (set! f (lambda () pause&)))))
   (react! r)
   (check-exn #rx"escaped reactor context" f))
  (test-begin
   (define f void)
   (define-signal S)
   (define r
     (prime (thunk (set! f (lambda () (present& S 1 2))))))
   (react! r)
   (check-exn #rx"escaped reactor context" f))


  (test-begin
   (define-signal S 0 #:gather +)
   (define/contract (test S)
     (reactive-> (signal/c number?) any)
     (emit& S 'a))
   (check-exn
    #rx"test: broke its own contract"
    (lambda () (react! (prime test S)))))
  (test-begin
   (define-signal S 0 #:gather +)
   (define/contract (test S)
     (reactive-> (signal/c number?) any)
     (emit& S 2))
   (check-not-exn
    (lambda () (react! (prime test S)))))
  (test-begin
   (define-signal I)
   (define-signal O)
   (define r
     (prime
      (thunk
       (present& I (void) (begin (await& I) (emit& O))))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r I)
   (check-true (last? O)))

  (test-begin
   (define-signal I)
   (define-signal O)
   (define r
     (prime
      (thunk
       (await& #:count 3 I)
       (emit& O))))
   (react! r)
   (check-false (last? O))
   (react! r I)
   (check-false (last? O))
   (react! r I)
   (check-false (last? O))
   (react! r I)
   (check-true (last? O)))
  (test-begin
   (define-signal I)
   (define-signal O)
   (define r
     (prime
      (thunk
       (await& #:immediate I)
       (emit& O))))
   (react! r I)
   (check-true (last? O)))
  (test-begin
   (define-signal I)
   (define-signal O)
   (define r
     (prime
      (thunk
       (await& #:immediate #:count 3 I)
       (emit& O))))
   (react! r I)
   (check-false (last? O))
   (react! r I)
   (check-false (last? O))
   (react! r I)
   (check-true (last? O)))

  (test-begin
   (define r
     (prime
      (thunk
       (loop& (void)))))
   (check-exn
    #rx"loop& terminated in a single instant!"
    (lambda () (react! r))))
  
  (test-begin
   (define r
     (prime
      (thunk
       (loop& pause&))))
   (check-not-exn (lambda () (react! r)))
   (check-not-exn (lambda () (react! r)))
   (check-not-exn (lambda () (react! r))))
  (test-begin
   (define-signal O)
   (define r
     (prime (thunk (par&) (emit& O))))
   (react! r)
   (check-true (last? O)))

  (test-begin
   (define r
     (prime (thunk (loop& (par&)))))
   (check-exn
    #rx"loop& terminated in a single instant!"
    (lambda () (react! r))))
  (test-begin
   (define r
     (prime (thunk (loop& (par& 1 2)))))
   (check-exn
    #rx"loop& terminated in a single instant!"
    (lambda () (react! r))))

  (test-begin
   (define-signal S1)
   (define-signal S2)
   (define-signal O)
   (define r
     (prime (thunk (loop& (await& (or S1 S2)) (emit& O)))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r S1)
   (check-true (last? O))
   (react! r)
   (check-false (last? O))
   (react! r S2)
   (check-true (last? O))
   (react! r)
   (check-false (last? O))
   (check-not-exn (lambda () (react! r S1 S2)))
   (check-true (last? O))))