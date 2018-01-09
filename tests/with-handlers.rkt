#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (process
       (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
         (error "hi")))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) "hi"))

  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (process
       (emit&
        O
        (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
          1)))))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1))

  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (process
       (emit&
        O
        (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
          pause& 1)))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) 1))

  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (process
       (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
         pause& (error "hi")))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) "hi"))

  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (process
       (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
         (par& (emit& O "hi") (error "bye"))))))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) "hi")
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) "bye"))



  (test-begin
   (define-signal O #f #:gather values)
   (define-signal run)
   (define r
     (prime
      (process
       (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
         (suspend&
          (par& (emit& O "hi") (error "bye"))
          #:unless run)))))
   (react! r)
   (check-false (last? O))
   (react! r run)
   (check-true (last? O))
   (check-equal? (last O) "hi")
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) "bye"))

  (test-begin
   (define-signal O #f #:gather values)
   (define-signal run)
   (define r
     (prime
      (process
       (suspend&
        (with-handlers& ([void (lambda (e) (emit& O (exn-message e)))])
          (par& (emit& O "hi") (error "bye")))
        #:unless run))))
   (react! r)
   (check-false (last? O))
   (react! r run)
   (check-true (last? O))
   (check-equal? (last O) "hi")
   (react! r)
   (check-false (last? O))
   (react! r run)
   (check-true (last? O))
   (check-equal? (last O) "bye")))