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
 with-handlers&)
(require "data.rkt" "runtime.rkt")
(require (for-syntax syntax/parse racket/stxparam-exptime racket/syntax) racket/hash racket/stxparam)
                                                           

(define-syntax-parameter in-process? #f)
(define current-control-tree (make-parameter #f))

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
              (parameterize ([current-control-tree tree])
                (syntax-parameterize ([in-process? #t])
                  e ...)))))]))
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

(define-syntax define-signal
  (syntax-parser
    [(_ S:id)
     #'(define S (make-pure-signal #f #f (make-signal-evt)))]
    [(_ S:id default:expr #:gather gather:expr)
     #'(define S
         (make-value-signal #f #f (make-signal-evt) default empty gather))]))
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
(define-syntax/in-process par&
  (syntax-parser
    [(par& p ...)
     #' (parf (current-control-tree) (list (lambda () p) ...))]))
(define-syntax/in-process pause&
  (syntax-parser
    [_:id #'(pausef (current-control-tree))]))

(define-syntax/in-process emit&
  (syntax-parser
    [(_ . a) #'(emitf . a)]))

(define-syntax/in-process suspend&
  (syntax-parser
    [(suspend& e:expr ... #:unless S)
     #`(%% k
           (let ([nt (make-suspend-unless empty empty S)])
             (with-extended-control
              (lambda ()
                (run-next! nt (continue-at (lambda () e ...) k nt))
                (activate-suspends! nt)
                (switch!))
              nt)))]))
(define-syntax/in-process abort&
  (syntax-parser
    [(abort& e:expr ... #:after S)
     #'(%%
        k
        (let ([ct (current-control-tree)])
          (with-extended-control
           (lambda () e ...)
           (make-preempt-when
            empty empty S
            (lambda () (continue-at void k ct))))))]
    [(abort& e:expr ... #:after S [pattern body ...] ...)
     #'(%%
        k
        (let ([ct (current-control-tree)])
          (with-extended-control
           (lambda () e ...)
           (make-preempt-when
            empty empty S
            (extend-with-parameterization
             (lambda ()
               (match (last S)
                 [pattern (continue-at (lambda () body ...) k ct)] ...
                 [_ #f])))))))]))

;; (-> any) ControlTree
;; run `body`with the control tree extended by `new-tree`
(define (with-extended-control body new-tree)
  (add-new-control-tree! (current-control-tree) new-tree)
  (parameterize ([current-control-tree new-tree])
    (call-with-control-safety body (current-control-tree))))

(define-syntax/in-process await&
  (syntax-parser
    [(await& #:immediate S:id)
     #'(run& (await-immediate S))]
    [(await& S)
     #'(run& (await S))]
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
     #'(let loop ()
         p ...
         (loop))]))

(define-syntax/in-process halt&
  (syntax-parser
    [halt:id #'(signal* S (suspend& (void) #:unless S))]))

(define-syntax/in-process with-handlers&
  (syntax-parser
    [(with-handlers& ([p:expr h:expr] ...) body:expr ...)
     (define/with-syntax (v ...)
       (for/list ([_ (in-list (syntax->list #'(p ...)))] [i (in-naturals)])
         i))
     #'(signal* ([S #f #:gather values])
         (abort&
          (with-handler-pred
           (lambda (exn)
             (cond
               [(p exn) v] ...))
           S
           (lambda () body ...)
           (current-control-tree))
          #:after S
          [(list v exn) (h exn)] ...))]))
                     

;; Signal -> Process
;; block until the signal is present (including in this instant)
(define-process (await-immediate S)
  (suspend& (void) #:unless S))

(define-process (await S)
  (begin (run& (await-immediate S)) pause&))

;; ValueSignal (Any -> Any) -> Process
;; Await a value for the signal S, and give it to `f`
;; when ready
(define-process (await-value S f)
  (suspend& (void) #:unless S)
  pause&
  (f (last S)))