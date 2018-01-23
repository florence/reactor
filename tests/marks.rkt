#lang racket
(require reactor)
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
                 1)))
      