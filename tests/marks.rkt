#lang racket
(require reactor (only-in reactor/data hide-thread?))
(module+ test
  (require rackunit)
  
  (test-begin
   (define r
     (prime
      (process
       (with-continuation-mark 1 2 pause&))))
   (react! r)
   (define l (reactor-continuation-marks r))
   (check-equal? (rest l) empty)
   (check-equal? (continuation-mark-set->list (first l) 1) (list 2)))
  
  (test-begin
   (define r
     (prime
      (process
       (with-continuation-mark 1 1
         (par&
          (with-continuation-mark 1 2 pause&)
          (with-continuation-mark 1 3 pause&))))))
   (react! r)
   (define l (reactor-continuation-marks r))
   (check-equal? (rest (rest l)) empty)
   (check-equal?
    (list->set
     (map (lambda (x) (continuation-mark-set->list x 1))
          l))
    (set (list 2 1) (list 3 1))))

 
  (test-begin
   (define-signal R)
   (define r
     (prime
      (process
       (abort&
        (par& 1 2)
        halt&
        #:after R))))
   (react! r)
   (check-equal? (length (reactor-continuation-marks r))
                 1))

  (test-begin
   (define-signal S)
   (define-signal S1)
   (define r
     (prime
      (process
       (present& S
                 (void)
                 (with-continuation-mark 1 1 (await& S1))))))
   (react! r)
   (react! r)
   (define l (reactor-continuation-marks r))
   (check-equal? (rest l) empty)
   (check-equal? (continuation-mark-set->list (first l) 1)
                 (list 1)))
  (test-begin
   (define r
     (prime
      (process
       (par& pause&
             (parameterize ([hide-thread? #t]) pause&)))))
   (react! r)
   (check-equal? (length (reactor-continuation-marks r))
                 1))
  (test-begin
   ;; NOTE this test is about making sure
   ;; hidden threads stay hidden. If the implementation of loop stops relying on
   ;; internal threads this test becomes meaningless
   (define r
     (prime
      (process
       (loop& pause&))))
   (react! r)
   (check-equal? (length (reactor-continuation-marks r))
                 1))

  (test-begin
   (define r
     (prime
      (process
       (with-continuation-mark 1 1
         (par& (with-continuation-mark 1 2 pause&)
               (with-continuation-mark 1 2 halt&))))))
   (react! r)
   (check-equal?
    (map (lambda (x) (continuation-mark-set->list x 1))
         (reactor-continuation-marks r))
    (list (list 2 1) (list 2 1)))
   (react! r)
   (check-equal?
    (map (lambda (x) (continuation-mark-set->list x 1))
         (reactor-continuation-marks r))
    (list (list 2)))))
