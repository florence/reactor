#lang racket
(provide
 ;; data
 signal?
 pure-signal?
 value-signal?
 reactor?
 process?
 reactor-done?
 ;; running
 start&
 run&
 react&
 ;; process creation
 process&
 define-process&
 ;; forms
 present&
 signal&
 par&
 emit&
 pause&
 await&
 last&)
(require (for-syntax syntax/parse racket/stxparam-exptime racket/syntax) racket/hash racket/stxparam)
(module+ test (require rackunit))

(define current-reactor (make-parameter #f))

;; A Reactor is a
;;  (reactor RThread (Listof Thread) (hasheqof Signal Blocked) (listof RThread) (Listof Signal))
(struct reactor (os active blocked paused signals) #:mutable #:authentic)
;; `os` is the continuation for the OS loop
;; `active` are a list of runnable threads
;; `blocked` maps signals to blocked thread pairs
;; `paused` is the list of threads to activate next instant
;; `signals` is a list of all signals in the program, that have been emitted
;;   they may be reset inbetween instants

;; a RThread is a (-> Any), and is the continuation of a thread

;; a process is a (make-process RThread)
(struct process (thunk) #:constructor-name make-process #:authentic)
;; `thunk` is the initial thread for this process
;; processes simply box unrun threads

;; a blocked is a (make-blocked RThread RThread)
(struct blocked (present absent) #:constructor-name make-blocked)
;; a blocked represents a thread awaiting a signals value.
;; it will run `present` if the signal is present or `absent` if it is now.

;; a signal is a one of
;; (make-value-signal boolean A (listof A) (A A -> A))
;; (make-pure-signal boolean)

(struct signal (status) #:mutable #:authentic)

(struct pure-signal signal () #:mutable #:authentic
  #:constructor-name make-pure-signal)
;; `status` is false if the signal has not been emitted (is unknown), and true if it has

(struct value-signal signal (value collection gather) #:mutable #:authentic
  #:constructor-name make-value-signal)
;; `status` is as in `pure-signal`
;; `value` is the value of the signal for this instant. It is equal to the result of
;;     `gather` in the last instant if the signal was emitted, or the value of the previous instant
;;         otherwise
;; `collection` is the list of values it has been emitted with this instant
;; `gather` turns collection into the new value between instants

;                                                              
;                                                              
;                                                              
;                                                              
;                                                              
;                                                              
;                                   ;;;                        
;                                   ;;;                        
;      ;;;;  ;;;     ;; ;;; ;;;   ;;;;;;;;   ;;;;    ;;;    ;;;
;    ;;;;;;   ;;    ;;  ;;;;;;;;  ;;;;;;;;   ;;;;;;   ;;;   ;; 
;   ;;;       ;;    ;;  ;;;   ;;;   ;;;         ;;;    ;;  ;;  
;   ;;;;      ;;;   ;;  ;;;   ;;;   ;;;          ;;;   ;;;;;;  
;    ;;;;;     ;;  ;;   ;;;   ;;;   ;;;        ;;;;;    ;;;;   
;      ;;;;    ;;; ;;   ;;;   ;;;   ;;;      ;;;;;;;    ;;;;   
;        ;;;    ;; ;;   ;;;   ;;;   ;;;     ;;;  ;;;    ;;;;   
;        ;;;    ;;;;    ;;;   ;;;   ;;;;    ;;;  ;;;   ;;  ;;  
;   ;;;;;;;      ;;;    ;;;   ;;;    ;;;;;; ;;;;;;;;  ;;   ;;; 
;   ;;;;;        ;;     ;;;   ;;;     ;;;;;  ;;; ;;; ;;;    ;;;
;               ;;;                                            
;             ;;;;                                             
;             ;;                                               
;                                                              
;                                                              

(define-syntax-parameter in-process? #f)

(define (run& e) (runf e))
(define-syntax process&
  (syntax-parser
    [(_ e ...)
     #'(make-process (lambda () e ...))]))
(define-syntax define-process&
  (syntax-parser
    [(_ (name:id args:id ...) body:expr ...)
     #'(define (name args ...)
         (process& body ...))]))

(define-syntax present&
  (syntax-parser
    [(present& S p q)
     #'(presentf S (lambda () p) (lambda () q))]))

(define-syntax signal&
  (syntax-parser
    [(signal& S:id e) #'(signal& (S) e)]
    [(signal& (S:id ...) e ...)
     #'(let ([S (make-pure-signal #f)] ...)
         e ...)]
    [(signal& ([S:id default:expr #:gather gather:expr] ...) e ...)
     #'(let ([S (make-value-signal #f default empty gather)] ...)
         e ...)]))
(define-syntax par&
  (syntax-parser
    [(par& p ...)
     #' (parf (list (lambda () p) ...))]))
(define-syntax pause&
  (syntax-parser
    [_:id #'(pausef)]))

(define emit&
  (case-lambda
    [(S) (emit-pure S)]
    [(S v) (emit-value S v)]))

(define-syntax await&
  (syntax-parser
    [(await& #:immediate S:id)
     #'(run& (await-immediate S))]
    [(await& S [pat:expr body:expr ...] ...)
     #'(let ()
         (define f
           (lambda (v)
             (match v
               [pat body ...] ...
               [_ (run& (await-value S f))])))
         (run& (await-value S f)))]))



;                                                                        
;                                                                        
;                                                                        
;                                                                        
;                                              ;;;                       
;                                              ;;;                       
;                                   ;;;        ;;;                       
;                                   ;;;                                  
;   ;;;;; ;;; ;;;  ;;;  ;;; ;;;   ;;;;;;;;  ;;;;;;   ;;;;; ;;;     ;;;;  
;   ;;;;;;;;; ;;;  ;;;  ;;;;;;;;  ;;;;;;;;  ;;;;;;   ;;;;;;;;;;   ;;;;;; 
;     ;;;;;   ;;;  ;;;  ;;;   ;;;   ;;;        ;;;   ;;; ;; ;;;  ;;   ;; 
;     ;;;     ;;;  ;;;  ;;;   ;;;   ;;;        ;;;   ;;; ;; ;;;  ;;   ;;;
;     ;;;     ;;;  ;;;  ;;;   ;;;   ;;;        ;;;   ;;; ;; ;;; ;;;;;;;;;
;     ;;;     ;;;  ;;;  ;;;   ;;;   ;;;        ;;;   ;;; ;; ;;; ;;;;;;;;;
;     ;;;     ;;;  ;;;  ;;;   ;;;   ;;;        ;;;   ;;; ;; ;;; ;;;      
;     ;;;     ;;;  ;;;  ;;;   ;;;   ;;;;       ;;;   ;;; ;; ;;;  ;;;     
;   ;;;;;;;;   ;;;;;;;  ;;;   ;;;    ;;;;;; ;;;;;;;;;;;; ;; ;;;   ;;;;;; 
;   ;;;;;;;;    ;; ;;;  ;;;   ;;;     ;;;;; ;;;;;;;;;;;; ;; ;;;     ;;;; 
;                                                                        
;                                                                        
;                                                                        
;                                                                        
;


;;;;;; OS

(define reactive-tag  (make-continuation-prompt-tag 'react))

;; Process -> Reactor
(define (start& proc)
  (make-reactor (process-thunk proc)))

;; Reactor -> Any
;; run a reaction on this reactor
(define (react& grp)
  (call-with-continuation-prompt
   (lambda () (os-loop! grp))
   reactive-tag))

;; (-> Any) -> Reactor
;; make a reactor containing only the given thread, which is active
(define (make-reactor proc)
  (reactor void (list proc) (make-hasheq) empty empty))

;; Reactor -> Any
;; main scheduler loop. Should be called with a `reactive-tag`
(define (os-loop! g)
  (match-define (reactor os active blocked paused signals) g)
  (cond
    [(reactor-done? g)
     (define active
       (for*/list ([(_ procs) (in-hash blocked)]
                   [p (in-list procs)])
         (blocked-absent p)))
     (set-reactor-os! g void)
     (set-reactor-active! g (append active paused))
     (set-reactor-blocked! g (make-hasheq))
     (set-reactor-paused! g empty)
     (set-reactor-signals! g empty)
     (for ([S signals])
       (reset-signal! S))]
    [else
     (define next (first active))
     (set-reactor-active! g (rest active))
     (call/cc
      (lambda (k)
        (set-reactor-os! g (lambda () (k (void))))
        (parameterize ([current-reactor g])
          (next)))
      reactive-tag)
     (os-loop! g)]))

;; Signal -> Void
;; cleanup signal for next instant
(define (reset-signal! S)
  (when (and (value-signal? S) (signal-status S))
    (define c (value-signal-collection S))
    (set-value-signal-value!
     S
     (foldl (value-signal-gather S) (first c) (rest c)))
    (set-value-signal-collection! S empty))
  (set-signal-status! S #f))

;; -> Any
;; switch back to the scheduler
(define (switch!)
  ((reactor-os (current-reactor))))

;; Signal -> Void
;; registers a signal emission with the OS for cleanup after the instant.
;; also unblocks any processes waiting on S
(define (register-signal-emission! S)
  (set-reactor-signals!
   (current-reactor)
   (cons S (reactor-signals (current-reactor))))
  (unblock! S))

;; Reactor -> Boolean
;; does this reactor have active threads?
(define (reactor-done? g) (empty? (reactor-active g)))

;; RThread -> Void
;; register this threads as active
(define (activate! thrd)
  (set-reactor-active!
   (current-reactor)
   (cons thrd (reactor-active (current-reactor)))))
;; RThread -> Void
;; register this thread for the next instant
(define (run-next! thrd)
  (set-reactor-paused!
   (current-reactor)
   (cons thrd (reactor-paused (current-reactor)))))

;;;;;; running

;; ValueSignal -> Any
(define last& value-signal-value)

;; Process -> Any
(define (runf proc) ((process-thunk proc)))

;; PureSignal -> Void
;; emit the signal
(define (emit-pure S)
  (unless (pure-signal? S)
    (error 'emit& "Expected pure signal, given ~s" S))
  (set-signal-status! S #t)
  (register-signal-emission! S))

;; ValueSignal Any -> Void
;; emit the given value on the signal
(define (emit-value S v)
  (unless (value-signal? S)
    (error 'emit& "Expected value carrying signal, given ~s and ~s" S v))
  (set-signal-status! S #t)
  (set-value-signal-collection!
   S
   (cons v (value-signal-collection S)))
  (register-signal-emission! S))

;; Signal -> Void
;; Unblock every thread waiting on S using its `present` continuation
(define (unblock! S)
  (define grp (current-reactor))
  (define blocked (reactor-blocked grp))
  (define unblocked (hash-ref blocked S empty))
  (hash-remove! blocked S)
  (set-reactor-active!
   grp
   (append (reactor-active grp)
           (map blocked-present unblocked))))

;; -> Void
;; suspend the current thread until the next instant
(define (pausef)
  (call/cc
   (lambda (k)
     (define grp (current-reactor))
     (run-next! (lambda () (k (void))))
     (switch!))
   reactive-tag))

;; Signal (-> Any) (-> Any) -> Any
;; dispatch on signal status, or block if not ready yet
(define (presentf S p q)
  (cond
    [(signal-status S) (p)]
    [else
     (define blocked (reactor-blocked (current-reactor)))
     (call/cc
      (lambda (k)
        (hash-set! blocked
                   S
                   (cons (make-blocked (lambda () (p) (k (void)))
                                       (lambda () (q) (k (void))))
                         (hash-ref blocked S empty)))
        (switch!))
      reactive-tag)]))

;; Signal -> Void
;; block until the signal is present (including in this instant)
(define-process& (await-immediate S)
  (present& S (void) (run& (await-immediate S))))

;; ValueSignal (Any -> Any) -> Any
;; Await a value for the signal S, and give it to `f`
;; when ready
(define-process& (await-value S f)
  (present& S
            (begin
              pause&
              (f (last& S)))
            (run& (await-value S f))))

;; (Listof RThread) -> Any
;; run all threads, blocking the current thread until all have completed
(define (parf threads)
  (define counter (length threads))
  ;; TODO optimization mentioned in RML paper:
  ;; pass boxed counter around, so that when
  ;; a thread is dynamically allocated it can
  ;; reuse/join the outer thread group
  (call/cc
   (lambda (k)
     (for/list ([t (in-list threads)])
       (activate!
        (lambda ()
          (t)
          (set! counter (- counter 1))
          (when (zero? counter)
            (activate! (lambda () (k (void)))))
          (switch!))))
     (switch!))
   reactive-tag))