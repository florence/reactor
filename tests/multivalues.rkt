#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal S 1 2)
   (define r (prime (process (emit& S 3 3))))
   (react! r)
   (check-equal?
    (call-with-values (lambda () (last S)) list)
    (list 3 3)))
  (test-begin
   (define-signal S 1 2)
   (define r (prime (process (void))))
   (react! r (list S (list 3 3)))
   (check-equal?
    (call-with-values (lambda () (last S)) list)
    (list 3 3)))
  (test-begin
   (define-signal S 1 2)
   (define r (prime (process  (emit& S 3 3) (emit& S 3 3))))
   (check-exn
    (regexp (regexp-quote "attempted to emit a value signal with no gather function twice: S '(3 3) '(3 3)"))
    (lambda () (react! r))))
  (test-begin
   (define-signal S 1 2 #:gather (lambda (a b c d) (values (+ a c) (+ b d))))
   (define r (prime (process  (emit& S 3 3) (emit& S 3 3))))
   (react! r)
   (check-equal?
    (call-with-values (lambda () (last S)) list)
    (list 6 6)))
  (test-begin
   (define-signal S 1 2)
   (define r (prime (process (emit& S 3 3)
                             (await*& S
                               [(3 3) (emit& S #t #t)]))))
   (react! r)
   (react! r)
   (check-equal?
    (call-with-values (lambda () (last S)) list)
    (list #t #t))))