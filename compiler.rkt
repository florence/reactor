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
 present&)
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
     #'(define S (make-pure-signal #f #f))]
    [(_ S:id default:expr #:gather gather:expr)
     #'(define S (make-value-signal #f #f default empty gather))]))
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
             (extend-control
              (let ([f (lambda () e ...)])
                (lambda ()
                  (cond
                    [(signal-status S) (f)]
                    [else
                     (run-next!
                      nt
                      (extend-with-parameterization
                       (lambda () (f) (k (void)))))
                     (set-reactor-susps!
                      (current-reactor)
                      (cons nt (reactor-susps (current-reactor))))
                     (switch!)])))
              S nt)))]))
(define-syntax/in-process abort&
  (syntax-parser
    [(suspend& e:expr ... #:after S)
     #'(%% k
           (extend-control (lambda () e ...)
                           S
                           (make-preempt-when empty empty S (lambda () (k (void))))))]))

(define (extend-control body S new-tree)
  (add-new-control-tree! (current-control-tree) new-tree)
  (parameterize ([current-control-tree new-tree])
    (body)))

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
    [halt:id #'(loop& pause&)]))



;; Signal -> Process
;; block until the signal is present (including in this instant)
(define-process (await-immediate S)
  (present& S (void) (run& (await-immediate S))))

(define-process (await S)
  (begin pause& (run& (await-immediate S))))

;; ValueSignal (Any -> Any) -> Process
;; Await a value for the signal S, and give it to `f`
;; when ready
(define-process (await-value S f)
  (present& S
            (begin
              pause&
              (f (last S)))
            (run& (await-value S f))))