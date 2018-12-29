#lang racket
(provide emit-pure
         emit-value
         presentf
         pausef
         emitf
         switch!
         run-next!
         activate!
         current-reactor
         last?
         last
         prime
         react!
         reactor-done?
         reactor-suspended?
         default
         %%
         activate-suspends!
         continue-at
         with-handler-pred
         reactor-continuation-marks
         reactive->)
(require reactor/data
         reactor/ct
         reactor/control
         (for-syntax syntax/parse)
         racket/control)
(module+ test (require rackunit))

(define the-current-reactor (make-parameter #f))

(define (current-reactor)
  (define r (the-current-reactor))
  (unless r
    (raise-process-escape-error))
  r)



;; Process Any ... -> Reactor
(define (prime proc . args)
  (make-reactor (lambda () (apply proc args))))

;; Reactor (Listof (or PureSignal (List ValueSignal (list Any ...)))) -> Any
;; run a reaction on this reactor
(define (react! grp . signals)
  (with-handlers ([void
                   ;; prevent `call-with-exception-handler` from running within
                   ;; the reaction itself. Necessary to prevent state leaking between
                   ;; reactors. 
                   raise])
    (reactor-unsafe! grp)
    (define (start)
      (parameterize ([the-current-reactor grp])
        (for ([i (in-list signals)])
          (match i
            [(list a b) (apply emit-value a b)]
            [a (emit-pure a)])))
      (sched! grp))
    (call-with-continuation-barrier
     (lambda ()
       (call/prompt start reactive-tag)
       (cleanup! grp))))
  
  (reactor-safe! grp))

;; (-> Any) -> Reactor
;; make a reactor containing only the given thread, which is active
(define (make-reactor proc)
  (define top-tree (make-top))
  (define kont #f)
  (define initial-thread
    (make-rthread empty-calling-continuation proc))
  (set-top-child! top-tree initial-thread)
  (reparent! initial-thread top-tree)
  (reactor (list initial-thread) (make-hasheq) top-tree (make-hasheq) empty #t))

(define-syntax reactive->
  (syntax-parser
    [(_ dom ... range)
     #'(->* (dom ...)
            #:pre/desc (if (the-current-reactor)
                           #t
                           "Called outside of a reactor")
            range)]))

;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
;                     ;;                     ;;           ;;;;                      
;      ;;;            ;;                     ;;            ;;;                      
;    ;;;;;            ;;                     ;;             ;;                      
;   ;;          ;;;;  ;; ;;;     ;;;       ;;;;  ;;   ;;    ;;       ;;;    ;;;; ;; 
;   ;;        ;;;;;;  ;;;;;;;   ;;;;;    ;;;;;;  ;;   ;;    ;;      ;;;;;   ;;;;;;; 
;    ;;;     ;;;      ;;   ;;   ;   ;;   ;;  ;;  ;;   ;;    ;;      ;   ;;    ;;;   
;      ;;;   ;;       ;;   ;;  ;;;;;;;  ;;   ;;  ;;   ;;    ;;     ;;;;;;;    ;;    
;        ;;  ;;       ;;   ;;  ;;;;;;;  ;;   ;;  ;;   ;;    ;;     ;;;;;;;    ;;    
;        ;;  ;;;      ;;   ;;  ;;       ;;   ;;  ;;   ;;    ;;     ;;         ;;    
;   ;;;;;;    ;;;;;;  ;;   ;;   ;;;;;;   ;;;;;;  ;;;;;;;  ;;;;;;    ;;;;;;  ;;;;;;  
;   ;;;;       ;;;;;  ;;   ;;     ;;;;    ;; ;;   ;;; ;;  ;;;;;;      ;;;;  ;;;;;;  
;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   


;; Reactor -> Any
;; main scheduler loop. Should be called within a `reactive-tag`.
(define (sched! g)
  (unless (ireactor-suspended? g)
    (match-define (reactor active blocked ct susps signals safe?) g)
    (define next (first active))
    (set-reactor-active! g (rest active))
    (call/prompt
     (lambda ()
       (parameterize ([the-current-reactor g])
         (run-rthread (reactor-ct g) next)))
     sched-tag
     void)
    (sched! g)))


;; Signal -> Void
;; registers a signal emission with the OS for cleanup after the instant.
;; also unblocks any processes waiting on S
(define (register-signal-emission! S)
  (set-reactor-signals!
   (current-reactor)
   (cons S (reactor-signals (current-reactor))))
  (unblock! S))

;; Reactor -> Boolean
;; does this reactor have no active threads, but have suspened threads?
(define (reactor-suspended? r)
  (and (ireactor-suspended? r)
       (not (hash-empty? (reactor-susps r)))))

;; Reactor -> Boolean
;; does this reactor have no active threads?
(define (ireactor-suspended? g)
  (empty? (reactor-active g)))

;; Reactor -> Boolean
;; does this reactor have active threads or suspensions?
(define (reactor-done? g)
  (and
   (empty? (reactor-active g))
   (hash-empty? (reactor-susps g))))

;; RThread -> Void
;; register this threads as active
(define (activate! thrd)
  (set-reactor-active!
   (current-reactor)
   (cons thrd (reactor-active (current-reactor)))))



;                                                                                                                                         
;                                                                                                                                         
;                                                                                                                                         
;                                                                                                                                         
;                                                                                    ;;;;                                                 
;   ;;;;;;                       ;;                         ;;                  ;;;   ;;;                                                 
;   ;;;;;;                       ;;                         ;;                ;;;;;    ;;                                                 
;     ;;     ;; ;;;     ;;;;   ;;;;;;;   ;;;     ;; ;;;   ;;;;;;;            ;;        ;;       ;;;     ;;;     ;; ;;;   ;;   ;;  ;; ;;   
;     ;;     ;;;;;;;   ;;;;;;  ;;;;;;;   ;;;;;   ;;;;;;;  ;;;;;;;           ;;         ;;      ;;;;;    ;;;;;   ;;;;;;;  ;;   ;;  ;;;;;;  
;     ;;     ;;   ;;  ;;         ;;          ;;  ;;   ;;    ;;              ;;         ;;      ;   ;;       ;;  ;;   ;;  ;;   ;;  ;;   ;; 
;     ;;     ;;   ;;   ;;;       ;;        ;;;;  ;;   ;;    ;;              ;;         ;;     ;;;;;;;     ;;;;  ;;   ;;  ;;   ;;  ;;   ;; 
;     ;;     ;;   ;;     ;;;     ;;      ;;;;;;  ;;   ;;    ;;              ;;         ;;     ;;;;;;;   ;;;;;;  ;;   ;;  ;;   ;;  ;;   ;; 
;     ;;     ;;   ;;       ;;    ;;     ;;   ;;  ;;   ;;    ;;               ;;        ;;     ;;       ;;   ;;  ;;   ;;  ;;   ;;  ;;  ;;  
;   ;;;;;;   ;;   ;;  ;;;;;;     ;;;;;  ;;;;;;;  ;;   ;;    ;;;;;            ;;;;;;  ;;;;;;    ;;;;;;  ;;;;;;;  ;;   ;;  ;;;;;;;  ;;;;;;  
;   ;;;;;;   ;;   ;;  ;;;;;        ;;;   ;;; ;;  ;;   ;;      ;;;              ;;;;  ;;;;;;      ;;;;   ;;; ;;  ;;   ;;   ;;; ;;  ;;;;    
;                                                                                                                                 ;;      
;                                                                                                                                 ;;      
;                                                                                                                                 ;;      
;                                                                                                                                         
;                                                                                                                                         

;; reactor -> Void
;; should not be called within a `reactive-tag`.
(define (cleanup! g)
  (match-define (reactor active blocked ct susps signals safe?) g)
  (for* ([(_ procs) (in-hash blocked)]
         [b (in-list procs)])
    (replace-child! (blocked-parent b) (blocked-blocking b) (blocked-absent b)))
  
  (reset-reactor-signals! g)
  (cleanup-control-tree! ct)
  
 

  (define new-susps
    (let ([new-susps (make-hasheq)])
      (add-suspends! new-susps (get-top-level-susps ct))
      new-susps))

  (define new-active (get-next-active ct))
  
  (set-reactor-active! g new-active)
  (set-reactor-blocked! g (make-hasheq))
  
  (set-reactor-susps! g new-susps))

;; Reactor -> Void
;; reset the signal buffer for the next instant
(define (reset-reactor-signals! g)
  (define signals (reactor-signals g))
  (set-reactor-signals! g empty)
  (for ([S (in-list (remove-duplicates signals signal=?))])
    (reset-signal! g S)))

;; Reactor Signal -> Void
;; cleanup signal for next instant
(define (reset-signal! g S)
  (when (and (value-signal? S) (signal-status S))
    (define c (value-signal-collection S))
    (set-value-signal-value!
     S
     (foldl (lambda (a b)
              (call-with-values (lambda () (apply (value-signal-gather S) (append a b))) list))
            (first c)
            (rest c)))
    (set-value-signal-collection! S empty))
  (set-signal-last?! S (signal-status S))
  (when (signal-status S)
    (ready-signal! S)
    (set-reactor-signals! g (cons S (reactor-signals g))))
  (set-signal-status! S #f))

;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                          ;;                     
;   ;;;;;;                                 ;;                     
;   ;;;;;;;                                ;;                     
;   ;;   ;;  ;;   ;;  ;; ;;;   ;; ;;;   ;;;;;    ;; ;;;      ;;;; 
;   ;;   ;;  ;;   ;;  ;;;;;;;  ;;;;;;;  ;;;;;    ;;;;;;;   ;;;;;; 
;   ;;  ;;   ;;   ;;  ;;   ;;  ;;   ;;     ;;    ;;   ;;   ;   ;; 
;   ;;;;;    ;;   ;;  ;;   ;;  ;;   ;;     ;;    ;;   ;;  ;;   ;; 
;   ;;;;;;   ;;   ;;  ;;   ;;  ;;   ;;     ;;    ;;   ;;  ;;   ;; 
;   ;;  ;;   ;;   ;;  ;;   ;;  ;;   ;;     ;;    ;;   ;;  ;;   ;; 
;   ;;   ;;  ;;;;;;;  ;;   ;;  ;;   ;;  ;;;;;;;; ;;   ;;   ;;;;;; 
;   ;;   ;;;  ;;; ;;  ;;   ;;  ;;   ;;  ;;;;;;;; ;;   ;;    ;; ;; 
;                                                              ;; 
;                                                           ;;;;  
;                                                          ;;;    
;                                                                 
;                                                                 


;; ValueSignal -> Any
(define (last S)
  (apply values (value-signal-value S)))
(define last? signal-last?)
(define (default S)
  (apply values (value-signal-default S)))

;; PureSignal -> Void
;; emit the signal
(define (emit-pure S)
  (unless (pure-signal? S)
    (error 'emit& "Expected pure signal, given ~s" S))
  (set-signal-status! S #t)
  (register-signal-emission! S))

;; ValueSignal Any -> Void
;; emit the given value on the signal
(define (emit-value S . v)
  (unless (value-signal? S)
    (error 'emit& "Expected value carrying signal, given ~s and ~s" S v))
  (set-signal-status! S #t)
  (set-value-signal-collection!
   S
   (cons v (value-signal-collection S)))
  (register-signal-emission! S))

;(define current-exn-handler (make-parameter values))
(define (with-handler-pred pred s f gct)
  ;; TODO its unclear to me how to
  ;; have a good test case for
  ;; aborting the inner part of
  ;; the continuation here instead of
  ;; just using "call-with-exception-handler"
  ;; but at least breaks are (more) correct in this version?
  (with-handlers ([void
                   (lambda (exn)
                     (cond
                       [(pred exn)
                        =>
                        (lambda (v)
                          (emit-value s (list (list v exn)))
                          (switch!))]
                       [else (raise exn)]))])
    (call-with-control-safety f gct)))

;; Signal -> Void
;; Unblock every thread waiting on S using its `present` continuation
;; and awake every suspend-unless
(define (unblock! S)
  (define name (signal-name S))
  (unblock-threads! name)
  (unblock-suspends! name))

;; SignalName -> Void
;; Effect: unblock any threads waiting on this signal
(define (unblock-threads! S)
  (define grp (current-reactor))
  (define blocked (reactor-blocked grp))
  (define unblocked (hash-ref blocked S empty))
  (hash-remove! blocked S)
  (for ([b (in-list unblocked)])
    (replace-child! (blocked-parent b) (blocked-blocking b) (blocked-present b)))
  (set-reactor-active!
   grp
   (append (reactor-active grp)
           (map blocked-present unblocked))))

;; SignalName -> Void
;; Effect: Unblock all suspensions waiting for this signal.
(define (unblock-suspends! S)
  (define susps (reactor-susps (current-reactor)))
  (define trees (hash-ref susps S empty))
  (for ([sp+S (in-list trees)])
    (define sp (first sp+S))
    (prune-suspends! susps sp (rest sp+S))
    (activate-suspends! sp)))

;; (hasheq-of Signal (cons SuspendUnless (Listof Signals)) SuspendUnless (Listof Signal) -> Void
;; remove any entries of sp marked to awaken on Ss
(define (prune-suspends! susps sp Ss)
  (for ([S (in-list Ss)])
    (hash-set! susps
               S
               (remf* (lambda (x) (eq? (first x) sp))
                      (hash-ref susps S)))))

;; ControlTree -> Void
;; awaken or activate all suspends in this tree
(define (activate-suspends! sp)
  (register-context-as-active!
   sp
   activate!
   (lambda (susp) (add-suspend! (reactor-susps (current-reactor)) susp))))
  

;; (hasheq-of Signal (cons SuspendUnless (Listof Signals)) (Listof SuspendUnless) -> Void
;; Effect: begin waiting on (activate) these suspends, given the suspension map
;; Invariant: Must be called during an instant, before signal cleanup.
(define (add-suspends! susps new-sp)
  (for ([sp (in-list new-sp)])
    (add-suspend! susps sp)))
(define (add-suspend! susps sp)
  (define Ss (map signal-name (suspend-unless-signals sp)))
  (define susper (cons sp Ss))
  (for ([S (in-list Ss)])
    (hash-set! susps
               S
               (cons susper (hash-ref susps S empty)))))

;; ControlTree -> Any
;; suspend the current thread until the next instant
(define (pausef ct)
  (%%
   k
   (run-next! ct (continue-at void k ct))
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
                 (signal-name S)
                 (cons
                  (make-blocked ct (current-rthread) (continue-at p k ct) (continue-at q k ct))
                  (hash-ref blocked (signal-name S) empty)))
      (switch!))]))


(define emitf
  (case-lambda
    [(S) (emit-pure S)]
    [(S . v) (apply emit-value S v)]))

;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                         ;;;;    
;       ;;;                      ;;                        ;;;    
;     ;;;;;                      ;;                         ;;    
;    ;;        ;;;    ;; ;;;   ;;;;;;;  ;;;; ;;    ;;;      ;;    
;   ;;        ;;;;;   ;;;;;;;  ;;;;;;;  ;;;;;;;   ;;;;;     ;;    
;   ;;       ;;   ;;  ;;   ;;    ;;       ;;;    ;;   ;;    ;;    
;   ;;       ;;   ;;  ;;   ;;    ;;       ;;     ;;   ;;    ;;    
;   ;;       ;;   ;;  ;;   ;;    ;;       ;;     ;;   ;;    ;;    
;    ;;      ;;   ;;  ;;   ;;    ;;       ;;     ;;   ;;    ;;    
;    ;;;;;;   ;;;;;   ;;   ;;    ;;;;;  ;;;;;;    ;;;;;   ;;;;;;  
;      ;;;;    ;;;    ;;   ;;      ;;;  ;;;;;;     ;;;    ;;;;;;  
;                                                                 
;                                                                 
;                                                                 
;                                                                 
;

;; ControlTree RThread -> Void
;; mark the current thread as having reached RThread's, and not schedulable until next instant.
(define (run-next! ct thread)
  (replace-child! ct (current-rthread) thread))

;; (-> A) (and continuation? ((-> A) -> Nothing)) [ControlTree] -> RThread
;; return a function that runs f, then jumps to k with the result of `(f)`,
;; all under the current parameterization
(define (continue-at f k [ignored #f])
  (make-rthread k f))

;; (-> Any) (-> ControlTree) -> Any
;; call f with the reactor exn handler
(define (call-with-control-safety f gct)
  (call-with-exception-handler
   (lambda (exn)
     (unsafe-assume-thread-will-die! gct)
     exn)
   f))

;; (-> control-tree) -> void
;; Explain to the system that the current thread is going to be removed before the end of the instant,
;; but must be in place until cleanup. It is unsafe for the thread to be schedulable ever again.
(define (unsafe-assume-thread-will-die! gct)
  ;; alternitavely, could to (void) here
  (replace-child! (gct)
                  (current-rthread)
                  (make-rthread empty-calling-continuation
                                (lambda ()
                                  (error
                                   'internal
                                   "Thread should be unresumable after marked as dead")))))

;; safe-reactor -> continuation-mark-tree
(define (reactor-continuation-marks r)
  (continuation-mark-tree (reactor-ct r)))