#lang racket
(provide
 signal*
 define-signal
 par&
 emit&
 pause&
 await&
 await*&
 suspend&
 loop&
 halt&
 abort&
 present&
 with-handlers&
 current-control-tree)
(require reactor/data reactor/runtime reactor/ct reactor/control)
(require (for-syntax syntax/parse racket/stxparam-exptime racket/syntax seq-no-order
                     racket/sequence)
         racket/control
         racket/hash racket/stxparam)
                                                           

(define (current-control-tree)
  (unless (current-reactor) (raise-process-escape-error))
  (control-tree-parent (current-rthread)))

(define (check-in-reactor!)
  (void (current-reactor)))

(define-syntax present&
  (syntax-parser
    [(present& S p q)
     #'(presentf (current-control-tree) S (lambda () p) (lambda () q))]))

(define ((raise-signal-error s n) . a)
  (define-values (l r) (split-at a n))
  (error 'emit& "attempted to emit a value signal with no gather function twice: ~a ~v ~v"
         s l r))

(define-syntax define-signal
  (syntax-parser
    [(_ S:id)
     #'(define S (make-pure-signal 'S))]
    [(_ S:id (~seq-no-order (~seq default:expr ...+)
                            (~optional (~seq #:gather gather:expr)
                                       #:defaults ([gather #`(raise-signal-error 'S n)]))
                            (~optional (~seq #:contract /c)
                                       #:defaults ([/c #'any/c]))))
     (quasisyntax/loc this-syntax
       (define/contract S (syntax-parameterize ([gather-contract #t]) /c)
         (make-value-signal 'S (list default ...) empty
                            (let ([n #,(length (syntax-e #'(default ...)))])
                              gather))))]))

(define-syntax signal*
  (syntax-parser
    [(signal S:id e) #'(signal (S) e)]
    [(signal (S:id ...) e ...)
     #'(let ()
         (define-signal S) ...
         e ...)]
    [(signal ([S:id default:expr ... #:gather gather:expr] ...) e ...)
     #'(let ()
         (define-signal S default ... #:gather gather) ...
         e ...)]))

(define-syntax pause&
  (syntax-parser
    [_:id #'(pausef (current-control-tree))]))

(define-syntax emit&
  (syntax-parser
    [(_ . a) #'(emitf . a)]))

(define-syntax suspend&
  (syntax-parser
    #:literals (or)
    [(suspend& e:expr ... #:unless (or S ...+))
     #`(%%
        k
        (let ([nt (make-suspend-unless k #f (flatten (list S ...)))])
          (with-extended-control
           tk
           nt
           (let ([t (continue-at (lambda () e ...) tk)])
             (set-suspend-unless-child! nt t)
             (reparent! t nt)
             (activate-suspends! nt)))))]
    [(suspend& e:expr ... #:unless S:expr)
     #'(suspend& e ... #:unless (or S))]))

(define-syntax abort&
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

(define-syntax par&
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

(define-syntax await&
  (syntax-parser
    #:literals (or)
    [(await& #:immediate (or S:expr ...+))
     #'(await-immediate (list S ...))]
    [(await& #:immediate S:expr)
     #'(await& #:immediate (or S))]
    [(await& (or S:expr ...+))
     #'(await (list S ...))]
    [(await& S:expr)
     #'(await& (or S))]
    [(await& #:immediate #:count n S ...+)
     #'(let ([k n]
             [s (list S ...)])
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
    [(await& #:count n S ...+)
     #'(let ([k n]
             [s (list S ...)])
         (unless (exact-positive-integer? n)
           (error 'await& "expected exact postive integer, got ~v" n))
         (let loop ([c k])
           (unless (zero? c)
             (await& s)
             (loop (sub1 c)))))]
    [(await& S [pat:expr body:expr ...] ...)
     #'(await*& S [(pat) body ...] ...)]))

(define-syntax await*&
  (syntax-parser
    [(_ S [(pat:expr ...) body:expr ...] ...)
     #:with under (for/list ([_ (in-syntax (car (syntax-e #'((pat ...) ...))))]) #'_)
     #'(let ()
         (define f
           (lambda (v)
             (match/values (apply values v)
                           [(pat ...) body ...] ...
                           [under (await-value S f)])))
         (await-value S f))]))

(define-syntax loop&
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

(define-syntax halt&
  (syntax-parser
    [halt:id #'(signal* S (suspend& (void) #:unless S))]))

(define-syntax with-handlers&
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
(define (await-immediate S)
  (suspend& (void) #:unless S))

(define (await S)
  (begin pause& (await-immediate S)))

;; ValueSignal (Any -> Any) -> Process
;; Await a value for the signal S, and give it to `f`
;; when ready
(define (await-value S f)
  (await& #:immediate S)
  pause&
  (f (value-signal-value S)))

     
     