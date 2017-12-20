#lang racket
(require "main.rkt" rackunit)
(test-begin
 (define g (start& (process& (void))))
 (check-false (reactor-done? g))
 (check-not-exn (lambda () (react& g)))
 (check-true (reactor-done? g))
 (check-not-exn (lambda () (react& g)))
 (check-true (reactor-done? g))
 (check-not-exn (lambda () (react& g))))
  
(test-begin
 (define done #f)
 (define p1
   (process&
    pause&
    (set! done #t)))
 (define grp (start& p1))
 (react& grp)
 (check-false done)
 (react& grp)
 (check-true done))
  
(test-begin
 (define present #f)
 (define absent #f)
 (define p1
   (process&
    (signal& S
      (present& S (set! present #t) (set! absent #t)))))
 (define grp (start& p1))
 (react& grp)
 (check-false present)
 (check-false absent)
 (react& grp)
 (check-false present)
 (check-true absent))

(test-begin
 (define found #f)
 (define g1
   (start&
    (process&
     (signal& S
       (par&
        (begin pause& (emit& S))
        (begin (await& #:immediate S) (set! found #t)))))))
 (react& g1)
 (check-false found)
 (react& g1)
 (check-true found))

(test-begin
 (define res #f)
 (define r
   (start&
    (process&
     (signal& ([S 0 #:gather +])
       (par&
        (emit& S 1)
        (emit& S 2)
        (await&
         S
         [n (set! res n)]))))))
 (react& r)
 (check-false res)
 (react& r)
 (check-equal? res 3))
(test-begin
 (define res #f)
 (define-process& (add& S)
   (emit& S (add1 (last& S)))
   pause&
   (run& (add& S)))
 (define r
   (start&
    (process&
     (signal& ([S 0 #:gather +])
       (par&
        (run& (add& S))
        (await&
         S
         [(? (lambda (x) (> x 3))) (set! res #t)]))))))
 (react& r)
 (check-false res)
 (react& r)
 (check-false res)
 (react& r)
 (check-false res)
 (react& r)
 (check-false res)
 (react& r)
 (check-true res))


(test-begin
 (define seen #f)
 (define-process& (await-when)
   (signal& (act S)
     (par&
      (suspend&
       (await& #:immediate S) (set! seen #t)
       #:unless act)
      (begin (emit& act) pause&           pause& (emit& act))
      (begin             pause& (emit& S) pause& (emit& S)))))
 (define r (start& (await-when)))
 (react& r)
 (check-false seen)
 (react& r)
 (check-false seen)
 (react& r)
 (check-true seen))
       
