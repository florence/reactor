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
 start
 react
 ;; process creation
 (rename-out [process* process])
 define-process
 ;; forms
 run&
 present&
 (rename-out [signal* signal])
 define-signal
 par&
 emit&
 pause&
 await&
 last
 last?
 suspend&
 abort&
 loop&
 halt&)
(require (for-syntax syntax/parse racket/stxparam-exptime racket/syntax) racket/hash racket/stxparam)
(module+ test (require rackunit))


;                                          
;                                          
;                                          
;                                          
;                                          
;                                          
;   ;;;;;;                ;;;              
;   ;;;;;;;               ;;;              
;   ;;;  ;;;   ;;;;     ;;;;;;;;   ;;;;    
;   ;;;   ;;;  ;;;;;;   ;;;;;;;;   ;;;;;;  
;   ;;;   ;;;     ;;;     ;;;         ;;;  
;   ;;;   ;;;      ;;;    ;;;          ;;; 
;   ;;;   ;;;    ;;;;;    ;;;        ;;;;; 
;   ;;;   ;;;  ;;;;;;;    ;;;      ;;;;;;; 
;   ;;;   ;;  ;;;  ;;;    ;;;     ;;;  ;;; 
;   ;;;  ;;;  ;;;  ;;;    ;;;;    ;;;  ;;; 
;   ;;;;;;;   ;;;;;;;;     ;;;;;; ;;;;;;;; 
;   ;;;;;      ;;; ;;;      ;;;;;  ;;; ;;; 
;                                          
;                                          
;                                          
;                                          
;                                          

;; A Reactor is a
;;  (reactor RThread (Listof Thread) (hasheqof Signal Blocked) ControlTree (hasheqof S SuspendUnless) (Listof Signal))
(struct reactor (os active blocked ct susps signals) #:mutable #:authentic)
;; `os` is the continuation for the OS loop
;; `active` are a list of runnable threads
;; `blocked` maps signals to blocked thread pairs
;; `ct` the control three for the entire program
;; `susps` is active suspensions, keys on their blocking signal
;; `signals` is a list of all signals in the program, that have been emitted
;;   they may be reset inbetween instants

;; a RThread is a (-> Any), and is the continuation of a thread

;; a process is a (make-process (ControlTree -> RThread))
(struct process (thunk) #:constructor-name make-process #:authentic)
;; `thunk` constructs the initial thread for this process, given its control tree

;; a blocked is a (make-blocked ControlTree RThread RThread)
(struct blocked (ct present absent) #:constructor-name make-blocked)
;; a blocked represents a thread awaiting a signals value.
;; it will run `present` if the signal is present or add `absent` to the control tree if it's not.

;; a signal is a one of
;; (make-value-signal boolean A (listof A) (A A -> A))
;; (make-pure-signal boolean)

(struct signal (status last?) #:mutable #:authentic)

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

;; a ControlTree is one of:
;;   (make-top (listof ControlTree) (Listof RThread))
;;   (make-suspend-unless (listof ControlTree) Signal (Listof RThread))
;;   (make-preempt-when (listof ControlTree) Signal (Listof RThread))

(struct control-tree (children next) #:mutable #:transparent)

(struct top control-tree () #:mutable
  #:constructor-name make-top)
;; a node with no specific control
(struct suspend-unless control-tree (signal) #:mutable
  #:constructor-name make-suspend-unless)
;; control becomes active when `signal` is emitted
(struct preempt-when control-tree (signal kont) #:mutable
  #:constructor-name make-preempt-when)
;; tree is removed at the end of instant when the signal is present

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
(define-syntax-parameter current-control-tree
  (lambda (stx) (raise-syntax-error #f "control tree escaped process context" stx)))

(define-syntax run&
  (syntax-parser
    [(_ e:expr)
     #`(runf e current-control-tree)]))
(define-syntax process*
  (syntax-parser
    [(_ e ...)
     #'(make-process
        (lambda (tree)
          (syntax-parameterize ([current-control-tree (make-rename-transformer #'tree)])
            (lambda ()
              e ...))))]))
(define-syntax define-process
  (syntax-parser
    [(_ name:id body ...)
     #'(define name (process* body ...))]
    [(_ (name:id args:id ...) body:expr ...)
     #'(define (name args ...)
         (process* body ...))]))

(define-syntax present&
  (syntax-parser
    [(present& S p q)
     #'(presentf current-control-tree S (lambda () p) (lambda () q))]))

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
(define-syntax par&
  (syntax-parser
    [(par& p ...)
     #' (parf current-control-tree (list (lambda () p) ...))]))
(define-syntax pause&
  (syntax-parser
    [_:id #'(pausef current-control-tree)]))

(define emit&
  (case-lambda
    [(S) (emit-pure S)]
    [(S v) (emit-value S v)]))

(define-syntax suspend&
  (syntax-parser
    [(suspend& e:expr ... #:unless S)
     #`(%% k
           (let ([nt (make-suspend-unless empty empty S)])
             (extend-control
              (let ([f (lambda () e ...)])
                (if (signal-status S)
                    (f)
                    (begin
                      (run-next! nt #,(syntax/loc this-syntax (lambda () (f) (k (void)))))
                      (set-reactor-susps!
                       (current-reactor)
                       (cons nt (reactor-susps (current-reactor))))
                      (switch!))))
              S nt)))]))
(define-syntax abort&
  (syntax-parser
    [(suspend& e:expr ... #:after S)
     #'(%% k
           (extend-control (begin e ...)
                           S
                           (make-preempt-when empty empty S (lambda () (k (void))))))]))

(define-syntax extend-control
  (syntax-parser
    [(_ body S controller)
     #'(let ([new-tree controller])
         (add-new-control-tree! current-control-tree new-tree)
         (syntax-parameterize ([current-control-tree
                                (make-rename-transformer #'new-tree)])
           body))]))

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

(define-syntax loop&
  (syntax-parser
    [(loop& p ...)
     #'(let loop ()
         p ...
         (loop))]))

(define-syntax halt&
  (syntax-parser
    [halt:id #'(loop& pause&)]))

;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;   ;;;;;;;                                                                                  
;   ;;;;;;;;                                                                                 
;   ;;;  ;;;  ;;;;; ;;;   ;;;;        ;;;;     ;;;;      ;;;;      ;;;;      ;;;;      ;;;;  
;   ;;;   ;;; ;;;;;;;;;  ;;;;;;;    ;;;;;;    ;;;;;;   ;;;;;;    ;;;;;;     ;;;;;;   ;;;;;;  
;   ;;;   ;;;   ;;;;;   ;;;  ;;;   ;;;       ;;   ;;  ;;;       ;;;        ;;   ;;  ;;;      
;   ;;;   ;;;   ;;;    ;;;    ;;; ;;;        ;;   ;;; ;;;;      ;;;;       ;;   ;;; ;;;;     
;   ;;;  ;;;    ;;;    ;;;    ;;; ;;;       ;;;;;;;;;  ;;;;;     ;;;;;    ;;;;;;;;;  ;;;;;   
;   ;;;;;;;     ;;;    ;;;    ;;; ;;;       ;;;;;;;;;    ;;;;      ;;;;   ;;;;;;;;;    ;;;;  
;   ;;;;;       ;;;    ;;;    ;;; ;;;       ;;;            ;;;       ;;;  ;;;            ;;; 
;   ;;;         ;;;     ;;;  ;;;   ;;;       ;;;           ;;;       ;;;   ;;;           ;;; 
;   ;;;       ;;;;;;;;  ;;;;;;;     ;;;;;;    ;;;;;;  ;;;;;;;   ;;;;;;;     ;;;;;;  ;;;;;;;  
;   ;;;       ;;;;;;;;    ;;;;       ;;;;;      ;;;;  ;;;;;     ;;;;;         ;;;;  ;;;;;    
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            



;; Signal -> Process
;; block until the signal is present (including in this instant)
(define-process (await-immediate S)
  (present& S (void) (run& (await-immediate S))))

;; ValueSignal (Any -> Any) -> Process
;; Await a value for the signal S, and give it to `f`
;; when ready
(define-process (await-value S f)
  (present& S
            (begin
              pause&
              (f (last S)))
            (run& (await-value S f))))


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

(define current-reactor (make-parameter #f))
(define reactive-tag  (make-continuation-prompt-tag 'react))
(define-syntax %%
  (syntax-parser
    [(_ k:id body ...)
     #'(call/cc (lambda (k) body ...) reactive-tag)]))

;; Process -> Reactor
(define (start proc)
  (make-reactor (process-thunk proc)))

;; Reactor (Listof (or PureSignal (List ValueSignal Any))) -> Any
;; run a reaction on this reactor
(define (react grp . signals)
  (parameterize ([current-reactor grp])
    (for ([i (in-list signals)])
      (match i
        [(list a b) (emit-value a b)]
        [a (emit-pure a)])))
  (call-with-continuation-prompt
   (lambda () (sched! grp))
   reactive-tag))

;; (-> Any) -> Reactor
;; make a reactor containing only the given thread, which is active
(define (make-reactor proc)
  (define top-tree (make-top empty empty))
  (reactor void (list (proc top-tree)) (make-hasheq) top-tree (make-hasheq) empty))

;; Reactor -> Any
;; main scheduler loop. Should be called with a `reactive-tag`
(define (sched! g)
  (match-define (reactor os active blocked ct susps signals) g)
  (cond
    [(reactor-done? g)
     (for* ([(_ procs) (in-hash blocked)]
            [b (in-list procs)])
       (run-next! (blocked-ct b) (blocked-absent b)))
     (define active (get-next-active! ct))
     (define new-susps
       (let ([new-susps (make-hasheq)])
         (add-suspends! new-susps (get-top-level-susps ct))
         new-susps))
     (set-reactor-os! g void)
     (set-reactor-active! g active)
     (set-reactor-blocked! g (make-hasheq))
     (set-reactor-signals! g empty)
     (set-reactor-susps! g new-susps)
     (for ([S (in-list (remove-duplicates signals eq?))])
       (reset-signal! g S))]
    [else
     (define next (first active))
     (set-reactor-active! g (rest active))
     (%%
      k
      (set-reactor-os! g (lambda () (k (void))))
      (parameterize ([current-reactor g])
        (next)))
     (sched! g)]))



;; ControlTree -> (listof RThread)
;; get any threads that should start active in the next instant
;; EFFECT: removes them from the control tree
(define (get-next-active! ct)
  (define (get-next-active/filter! ct)
    (define (rec cts)
      (for/fold ([threads empty] [children empty])
                ([ct (in-list cts)])
        (define-values (t ct2) (get-next-active/filter! ct))
        (values (append t threads) (if ct2 (cons ct2 children) children))))
    (match ct
      [(top children threads)
       (define-values (t child) (rec children))
       (set-control-tree-next! ct empty)
       (set-control-tree-children! ct child)
       (values (append threads t) ct)]
      [(preempt-when children threads signal k)
       (define-values (t child) (rec children))
       (set-control-tree-next! ct empty)
       (set-control-tree-children! ct child)
       (if (signal-status signal)
           (values (cons k t) #f)
           (values (append threads t)
                   (if (and (empty? threads) (empty? child)) #f ct)))]
      [(suspend-unless children threads signal)
       #:when (signal-status signal)
       (define-values (t child) (rec children))
       (set-control-tree-next! ct (append t threads))
       (set-control-tree-children! ct child)
       (values empty (if (and (empty? threads) (empty? child)) #f ct))]
      [(suspend-unless _ _ _) (values empty ct)]))
  ;; ---- IN ----
  (define-values (threads _) (get-next-active/filter! ct))
  ;; _ must be ct here
  threads)

;; ControlTree -> (Listof SuspendUnless)
(define (get-top-level-susps ct)
  (cond
    [(suspend-unless? ct)
     (list ct)]
    [else
     (append-map get-top-level-susps (control-tree-children ct))]))

;; ControlTree -> (Listof RThread)
(define (get-top-level-threads ct)
  (cond
    [(and (suspend-unless? ct)
          (not (signal-status (suspend-unless-signal ct))))
     (list)]
    [else
     (append
      (control-tree-next ct)
      (append-map get-top-level-threads (control-tree-children ct)))]))

;; Reactor Signal -> Void
;; cleanup signal for next instant
(define (reset-signal! g S)
  (when (and (value-signal? S) (signal-status S))
    (define c (value-signal-collection S))
    (set-value-signal-value!
     S
     (foldl (value-signal-gather S) (first c) (rest c)))
    (set-value-signal-collection! S empty))
  (set-signal-last?! S (signal-status S))
  ;; re-add signal to emission list, so its last value is correctly changed
  (when (signal-status S)
    (set-reactor-signals! g (cons S (reactor-signals g))))
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
;; ControlTree RThread -> Void
;; register this thread for the next instant
(define (run-next! ct thrd)
  (set-control-tree-next!
   ct
   (cons thrd (control-tree-next ct))))

(define (add-new-control-tree! cct nt)
  (set-control-tree-children!
   cct
   (cons nt (control-tree-children cct))))

;;;;;; running

;; ValueSignal -> Any
(define last value-signal-value)
(define last? signal-last?)

;; Process -> Any
(define (runf proc control-tree) (((process-thunk proc) control-tree)))

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
;; and awake every suspend-unless
(define (unblock! S)
  (unblock-threads! S)
  (unblock-suspends! S))

(define (unblock-threads! S)
  (define grp (current-reactor))
  (define blocked (reactor-blocked grp))
  (define unblocked (hash-ref blocked S empty))
  (hash-remove! blocked S)
  (set-reactor-active!
   grp
   (append (reactor-active grp)
           (map blocked-present unblocked))))

(define (unblock-suspends! S)
  (define susps (reactor-susps (current-reactor)))
  (define trees (hash-ref susps S empty))
  (hash-remove! susps S)
  (for ([sp (in-list trees)])
    (for-each activate! (control-tree-next sp))
    (set-control-tree-next! sp empty)
    (define new-sp (append-map get-top-level-susps (control-tree-children sp)))
    (define new-threads (append-map get-top-level-threads (control-tree-children sp)))
    (for-each activate! new-threads)
    (add-suspends! susps new-sp)))

(define (add-suspends! susps new-sp)
  (for ([sp (in-list new-sp)])
    (define S (suspend-unless-signal sp))
    (hash-set! susps
               S
               (cons sp (hash-ref susps S empty)))))

;; ControlTree -> Void
;; suspend the current thread until the next instant
(define (pausef ct)
  (%%
   k
   (run-next! ct (lambda () (k (void))))
   (switch!)))

;; ControlTree Signal (-> Any) (-> Any) -> Any
;; dispatch on signal status, or block if not ready yet
(define (presentf ct S p q)
  (cond
    [(signal-status S) (p)]
    [else
     (define blocked (reactor-blocked (current-reactor)))
     (%%
      k
      (hash-set! blocked
                 S
                 (cons (make-blocked ct
                                     (lambda () (p) (k (void)))
                                     (lambda () (q) (k (void))))
                       (hash-ref blocked S empty)))
      (switch!))]))


;; ControlTree (Listof RThread) -> Any
;; run all threads, blocking the current thread until all have completed
(define (parf ct threads)
  (define counter (length threads))
  ;; TODO optimization mentioned in RML paper:
  ;; pass boxed counter around, so that when
  ;; a thread is dynamically allocated it can
  ;; reuse/join the outer thread group
  (%% k
     (for/list ([t (in-list threads)])
       (activate!
        (lambda ()
          (t)
          (set! counter (- counter 1))
          (when (zero? counter)
            (activate! (lambda () (k (void)))))
          (switch!))))
     (switch!)))