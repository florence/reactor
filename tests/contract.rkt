#lang racket
(require rackunit reactor)

(test-begin
 (define-signal S 0
   #:contract (signal/c integer?))
 (define r
   (prime (thunk (emit& S "a"))))
 (check-exn
  (regexp (regexp-quote "S: contract violation\n  expected: integer?\n  given: \"a\"\n  in: The 1st value emitted to\n      (signal/c integer?)"))
  (lambda () (react! r))))

(test-begin
 (define-signal S 0
   #:contract (signal/c integer?)
   #:gather (lambda _ "x"))
 (define r
   (prime (thunk (emit& S 1) (emit& S 1))))
 (check-exn
  (regexp
   (regexp-quote "S: broke its own contract\n  promised: integer?\n  produced: \"x\"\n  in: the range of\n      The gather function of\n      (signal/c integer?)"))
  (lambda () (react! r))))

(test-begin
 (define-signal S0 0
   #:gather (lambda _ "x"))
 (define/contract S (signal/c integer?) S0)
 (define r 
   (prime (thunk (emit& S 1) (emit& S 1))))
 (check-not-exn (lambda () (react! r)))
 (check-exn
  (regexp (regexp-quote "S: broke its own contract\n  promised: integer?\n  produced: \"x\"\n  in: The 1st value contained in\n      (signal/c integer?)\n"))
  (lambda () (last S))))
(test-begin
 (check-exn
  (regexp (regexp-quote "S: broke its own contract\n  promised: string?\n  produced: 0\n  in: The 1st default of argument of\n      (signal/c string?)"))
  (lambda () (define-signal S 0 #:contract (signal/c string?)) (void))))
(test-begin
 (check-exn
  (regexp (regexp-quote "S: broke its own contract\n  promised A signal carrying 1 values produced A signal named S, which carries 2 values with defaults: (0 1)\n  in: (signal/c integer?"))
  (lambda () (define-signal S 0 1 #:contract (signal/c integer?)) (void))))

(test-begin
 (check-exn
  (regexp (regexp-quote "S: broke its own contract\n  promised: (signal/c)\n  produced: #<value-signal>\n  in: (signal/c)"))
  (lambda () (define-signal S 1 #:contract (signal/c)) (void))))

(test-begin
 (check-exn
  (regexp (regexp-quote "S1: broke its own contract;\n promised value-signal? produced #<pure-signal>\n  in: (signal/c integer?)"))
  (lambda () (define-signal S) (define/contract S1 (signal/c integer?) S) (void))))