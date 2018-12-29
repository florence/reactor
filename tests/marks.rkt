#lang racket
(require (except-in reactor branch leaf) (only-in reactor/ct hidden-thread-key hide-thread? branch leaf))
(module+ test
  (require rackunit)
  
  (test-begin
   (define r
     (prime
      (thunk
       (with-continuation-mark 1 2 pause&))))
   (react! r)
   (define l (reactor-continuation-marks r))
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (leaf (list 2))))
  
  (test-begin
   (define r
     (prime
      (thunk
       (with-continuation-mark 1 1
         (par&
          (with-continuation-mark 1 2 pause&)
          (with-continuation-mark 1 3 pause&))))))
   (react! r)
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (branch (list 1) (list (leaf (list 2)) (leaf (list 3))))))
  (test-begin
   (define r
     (prime
      (thunk
       (par& pause& pause&))))
   (react! r)
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (branch empty (list (leaf empty) (leaf empty)))))

 
  (test-begin
   (define-signal R)
   (define r
     (prime
      (thunk
       (abort&
        (par& 1 2)
        halt&
        #:after R))))
   (react! r)
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (leaf empty)))

  (test-begin
   (define-signal S)
   (define-signal S1)
   (define r
     (prime
      (thunk
       (present& S
                 (void)
                 (with-continuation-mark 1 1 (await& S1))))))
   (react! r)
   (react! r)
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (leaf (list 1))))
  
  (test-begin
   (define r
     (prime
      (thunk
       (par& pause&
             (with-continuation-mark hidden-thread-key #t pause&)))))
   (react! r)
   (check-equal? (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
                 (leaf empty)))
  
  (test-begin
   ;; NOTE this test is about making sure
   ;; hidden threads stay hidden. If the implementation of loop stops relying on
   ;; internal threads this test becomes meaningless
   (define r
     (prime
      (thunk
       (loop& pause&))))
   (react! r)
   (check-equal? (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
                 (leaf empty)))
  (test-begin
   (define-signal S)
   (define r
     (prime
      (thunk
       (with-continuation-mark 1 1
         (suspend& (with-continuation-mark 1 2 halt&) #:unless S)))))
   (react! r S)
   (check-equal? (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
                 (leaf (list 1 2))))

  (test-begin
   (define r
     (prime
      (thunk
       (with-continuation-mark 1 1
         (par& (with-continuation-mark 1 2 pause&)
               (with-continuation-mark 1 2 halt&))))))
   (react! r)
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (branch (list 1) (list (leaf (list 2)) (leaf (list 2)))))
   (react! r)
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (leaf (list 2)))
   (react! r)
   (check-equal?
    (continuation-mark-set-tree->tree (reactor-continuation-marks r) 1)
    (leaf (list 2)))))
