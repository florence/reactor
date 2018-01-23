#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (process
       (with-handlers& 
        (error "hi")
        #:after-error [void (lambda (e) (emit& O (exn-message e)))]))))
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
        (with-handlers& 
         1
         #:after-error
         [void (lambda (e) (emit& O (exn-message e)))])))))
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
        (with-handlers& 
         pause& 1
         #:after-error [void (lambda (e) (emit& O (exn-message e)))])))))
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
       (with-handlers& 
        pause& (error "hi")
        #:after-error [void (lambda (e) (emit& O (exn-message e)))]))))
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
       (with-handlers& 
        (par& (emit& O "hi") (error "bye"))
        #:after-error [void (lambda (e) (emit& O (exn-message e)))]))))
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
       (with-handlers& 
        (suspend&
         (par& (emit& O "hi") (error "bye"))
         #:unless run)
        #:after-error [void (lambda (e) (emit& O (exn-message e)))]))))
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
        (with-handlers& 
         (par& (emit& O "hi") (error "bye"))
         #:after-error [void (lambda (e) (emit& O (exn-message e)))])
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