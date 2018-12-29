#lang racket
(require reactor)
(module+ test
  (require rackunit)
  (test-begin
   (define-signal O #f #:gather values)
   (define r
     (prime
      (thunk
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
      (thunk
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
      (thunk
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
      (thunk
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
      (thunk
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
      (thunk
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
      (thunk
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
   (check-equal? (last O) "bye"))

  
  (test-begin
   (define-signal disable)
   (define-signal enable)
   (define-signal enable-run)
   (define-signal disable-run)
   (define-signal inputs-changed)
   (define-signal inputs-changed-to "")
   (define-signal error)
   (define (input-handling)
     (par&
      (loop& (await& #:immediate inputs-changed) (emit& disable-run) pause&)
      (loop&
       (await& #:immediate inputs-changed-to)
       (emit& disable)
       (await& inputs-changed-to
         [str
          (with-handlers&
           (read (open-input-string str))
           (emit& enable-run)
           #:after-error [exn:fail? (lambda (x) (emit& error))])])
       (emit& enable))))

   (define r (prime input-handling))

   (react! r inputs-changed)
   (check-true (last? disable-run))
   (react! r (list inputs-changed-to (list "(")))
   (check-true (last? disable))
   (react! r)
   (react! r)
   (check-true (last? error))
   (check-true (last? enable)))

  (test-begin
   (define-signal S "")
   (define-signal O "")
   (define r
     (prime
      (thunk
       (par&
        (await& S
          [v
           (with-handlers&
            (error 'hi)
            #:after-error [void (lambda (_) (emit& O ""))])])
        (loop&
         (await& O
           [v (void)]))))))
   (react! r (list S (list "")))
   (check-false (reactor-done? r))
   (check-false (reactor-suspended? r))
   (react! r)
   (check-false (reactor-done? r))
   (check-false (reactor-suspended? r)))

  (test-begin
   (define-signal O (seteq) #:gather set-union)
   (define r
     (prime
      (thunk
       (with-handlers&
        (par& (raise "a") (raise "b"))
        #:after-error [(lambda (_) #t) (lambda (e) (emit& O (set e)))]))))
   (react! r)
   (check-false (last? O))
   (react! r)
   (check-true (last? O))
   (check-equal? (last O) (set "a" "b"))))
   
   