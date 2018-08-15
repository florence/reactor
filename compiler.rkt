#lang racket
(provide
 run&
 process*
 define-process
 signal*
 define-signal
 par&
 emit&
 pause&
 await&
 suspend&
 loop&
 halt&
 abort&
 present&
 with-handlers&
 current-control-tree)
(require "data.rkt" "runtime.rkt" "ct.rkt" "control.rkt")
(require (for-syntax syntax/parse racket/stxparam-exptime racket/syntax)
         racket/control
         racket/hash racket/stxparam)
                                                           

(define-syntax-parameter in-process? #f)
(define (current-control-tree)
  (unless (current-reactor) (raise-process-escape-error))
  (control-tree-parent (current-rthread)))

(define-syntax define-syntax/in-process
  (syntax-parser
    [(_ name:id f)
     #'(define-syntax name
         (lambda (stx)
           (unless (syntax-parameter-value #'in-process?)
             (raise-syntax-error 'name "used outside of process" stx))
           (f stx)))]))

(define-syntax/in-process run&
  (syntax-parser
    [(_ e:expr)
     #`(runf e (current-control-tree))]))
(define-syntax process*
  (syntax-parser
    [(_ e ...)
     #'(make-process
        (lambda (tree)
          (lambda ()
            (syntax-parameterize ([in-process? #t])
              e ...))))]))
(define-syntax define-process
  (syntax-parser
    [(_ name:id body ...)
     #'(define name (process* body ...))]
    [(_ (name:id args:id ...) body:expr ...)
     #'(define (name args ...)
         (process* body ...))]))

(define-syntax/in-process present&
  (syntax-parser
    [(present& S p q)
     #'(presentf (current-control-tree) S (lambda () p) (lambda () q))]))

(define ((raise-signal-error s) a b)
  (error 'emit& "attempted to emit a value signal with no gather function twice: ~a ~v ~v"
         s a b))
(define-syntax define-signal
  (syntax-parser
    [(_ S:id)
     #'(define S (make-pure-signal 'S))]
    [(_ S:id default:expr)
     #'(define-signal S default #:gather (raise-signal-error 'S))]
    [(_ S:id default:expr #:gather gather:expr)
     #'(define S
         (make-value-signal 'S default empty gather))]))
(define-syntax signal*
  (syntax-parser
    [(signal S:id e) #'(signal (S) e)]
    [(signal (S:id ...) e ...)
     #'(let ()
         (define-signal S) ...
         e ...)]
    [(signal ([S:id default:expr #:gather gather:expr] ...) e ...)
     #'(let ()
         (define-signal S default #:gather gather) ...
         e ...)]))

(define-syntax/in-process pause&
  (syntax-parser
    [_:id #'(pausef (current-control-tree))]))

(define-syntax/in-process emit&
  (syntax-parser
    [(_ . a) #'(emitf . a)]))

(define-syntax/in-process suspend&
  (syntax-parser
    [(suspend& e:expr ... #:unless S)
     #`(%%
        k
        (let ([nt (make-suspend-unless k #f S)])
          (with-extended-control
           tk
           nt
           (let ([t (continue-at (lambda () e ...) tk)])
             (set-suspend-unless-child! nt t)
             (reparent! t nt)
             (activate-suspends! nt)))))]))

(define-syntax/in-process abort&
  (syntax-parser
    [(abort& e:expr ... #:after S)
     #'(%%
        k
        (letrec ([nt (make-preempt-when
                      k #f S
                      (lambda () (continue-at void (control-tree-k nt))))])
          (with-extended-control
           tk
           nt
           (let ([t (continue-at
                     (lambda () e ...)
                     tk)])
             (set-preempt-when-child! nt t)
             (reparent! t nt)
             (activate! t)))))]
    [(abort& e:expr ... #:after S [pattern body ...] ...)
     #'(%%
        k
        (letrec ([nt (make-preempt-when
                      k #f S
                      (lambda ()
                        (match (last S)
                          [pattern (continue-at (lambda () body ...)
                                                (control-tree-k nt))] ...
                          [_ #f])))])
          (with-extended-control
           tk
           nt
           (let ([t (continue-at
                     (lambda () e ...)
                     tk)])
             (set-preempt-when-child! nt t)
             (reparent! t nt)
             (activate! t)))))]))

(define-syntax/in-process par&
  (syntax-parser
    [(par&) #'(void)]
    [(par& p ...)
     #:with k (generate-temporary)
     #:with (t ...) (generate-temporaries #'(p ...))
     #'(%%
        k
        (let ([nt (make-par k empty)])
          (replace-child! (current-control-tree) (current-rthread) nt)
          (call/prompt
           (lambda ()
             (%% tk
                 (let ([t (continue-at (lambda () p) tk)] ...)
                   (set-par-children! nt (list t ...))
                   (reparent! t nt) ...
                   (activate! t) ...) (switch!)))
           reactive-tag)))]))

(define-syntax with-extended-control
  (syntax-parser
    [(_ k nt body ...)
     #`(with-extended-controlf
        (lambda () (%% k body ... (switch!)))
        nt)]))

;; (-> any) ControlTree
;; run `body`with the control tree extended by `new-tree`
(define (with-extended-controlf body new-tree)
  (replace-child! (current-control-tree) (current-rthread) new-tree)
  (call/prompt body reactive-tag))

(define-syntax/in-process await&
  (syntax-parser
    [(await& #:immediate S:id)
     #'(run& (await-immediate S))]
    [(await& S)
     #'(run& (await S))]
    [(await& #:immediate #:count n S)
     #'(let ([k n]
             [s S])
         (unless (exact-positive-integer? n)
           (error 'await& "expected exact postive integer, got ~v" n))
         (let loop ([c k])
           (cond
             [(= c 1)
              (await& #:immediate s)]
             [else
              (await& #:immediate s)
              pause&
              (loop (sub1 c))])))]
    [(await& #:count n S)
     #'(let ([k n]
             [s S])
         (unless (exact-positive-integer? n)
           (error 'await& "expected exact postive integer, got ~v" n))
         (let loop ([c k])
           (unless (zero? c)
             (await& s)
             (loop (sub1 c)))))]
    [(await& S [pat:expr body:expr ...] ...)
     #'(let ()
         (define f
           (lambda (v)
             (match v
               [pat body ...] ...
               [_ (run& (await-value S f))])))
         (run& (await-value S f)))]))

(define-syntax/in-process loop&
  (syntax-parser
    [(loop& p ...)
     #`(let loop ()
         (define-signal loop-check)
         (par& (begin p ... (emit& loop-check))
               (hide
                (lambda ()
                  (present& loop-check
                            #,(syntax/loc this-syntax
                                (error 'loop& "loop& terminated in a single instant!"))
                            (void)))))
         (loop))]))

(define-syntax/in-process halt&
  (syntax-parser
    [halt:id #'(signal* S (suspend& (void) #:unless S))]))

(define-syntax/in-process with-handlers&
  (syntax-parser
    [(with-handlers& body:expr ... #:after-error [p:expr h:expr] ...)
     (define/with-syntax (v ...)
       (for/list ([_ (in-list (syntax->list #'(p ...)))] [i (in-naturals)])
         i))
     #'(signal* ([S empty #:gather append])
                (abort&
                 (with-handler-pred
                  (lambda (exn)
                    (cond
                      [(p exn) v] ...
                      [else #f]))
                  S
                  (lambda () body ...)
                  current-control-tree)
                 #:after S
                 [vals
                  (let loop ([a vals])
                    (match a
                      [(list) empty]
                      [(cons (list n exn) r)
                       (par&
                        (cond
                          [(= n v) (h exn)] ...)
                        (loop r))]))]))]))
                     

;; Signal -> Process
;; block until the signal is present (including in this instant)
(define-process (await-immediate S)
  (suspend& (void) #:unless S))

(define-process (await S)
  (begin pause& (run& (await-immediate S))))

;; ValueSignal (Any -> Any) -> Process
;; Await a value for the signal S, and give it to `f`
;; when ready
(define-process (await-value S f)
  (await& #:immediate S)
  pause&
  (f (last S)))