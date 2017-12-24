#lang racket
(provide emit-pure
         emit-value
         runf
         presentf
         parf
         pausef
         emitf
         switch!
         run-next!
         current-reactor
         add-new-control-tree!
         last?
         last
         prime
         react!
         reactor-done?
         reactor-suspended?
         %%
         activate-suspends!
         extend-with-parameterization
         continue-at)
(require "data.rkt"
         (for-syntax syntax/parse))
(module+ test (require rackunit))




;;;;;; OS

(define current-reactor (make-parameter #f))
(define reactive-tag  (make-continuation-prompt-tag 'react))
(define-syntax %%
  (syntax-parser
    [(_ k:id body ...)
     #'(call/cc (lambda (k) body ...) reactive-tag)]))

  

;; Process -> ExternalreaReactor
(define (prime proc)
  (make-external-reactor (make-reactor (process-thunk proc))))

;; Reactor (Listof (or PureSignal (List ValueSignal Any))) -> Any
;; run a reaction on this reactor
(define (react! r . signals)
  (define grp (external-reactor-internal r))
  (parameterize ([current-reactor grp])
    (for ([i (in-list signals)])
      (match i
        [(list a b) (emit-value a b)]
        [a (emit-pure a)])))
  (call-with-continuation-prompt
   (lambda () (sched! grp))
   reactive-tag)
  (cleanup! grp))

;; (-> Any) -> Reactor
;; make a reactor containing only the given thread, which is active
(define (make-reactor proc)
  (define top-tree (make-top empty empty))
  (reactor void (list (proc top-tree)) (make-hasheq) top-tree (make-hasheq) empty))

;; Reactor -> Any
;; main scheduler loop. Should be called within a `reactive-tag`.
(define (sched! g)
  (unless (ireactor-suspended? g)
    (match-define (reactor os active blocked ct susps signals) g)
    (define next (first active))
    (set-reactor-active! g (rest active))
    (%%
     k
     (set-reactor-os! g (lambda () (k (void))))
     (parameterize ([current-reactor g])
       (next)))
    (sched! g)))

;; reactor -> Void
;; should not be called within a `reactive-tag`.
(define (cleanup! g)
  (match-define (reactor os active blocked ct susps signals) g)
  (for* ([(_ procs) (in-hash blocked)]
         [b (in-list procs)])
    (run-next! (blocked-ct b) (blocked-absent b)))
  (define new-susps
    (let ([new-susps (make-hasheq)])
      (add-suspends! new-susps (get-top-level-susps ct))
      new-susps))
  

  (reset-reactor-signals! g)
  
  (define new-active (get-next-active! ct))
  
  (set-reactor-os! g void)
  (set-reactor-active! g new-active)
  (set-reactor-blocked! g (make-hasheq))
  
  (set-reactor-susps! g new-susps))



;; ControlTree -> (listof RThread)
;; get any threads that should start active in the next instant
;; EFFECT: removes them from the control tree, cleans up dead children.
;; INVARIANT: Must be called during an instant, after signals are reset for the next instant
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
      [(preempt-when (list) (list) signal g)
       (values empty #f)]
      [(preempt-when children threads signal g)
       (define-values (t child) (rec children))
       (set-control-tree-next! ct empty)
       (set-control-tree-children! ct child)
       (cond
         [(and (last? signal) (g))
          =>
          (lambda (k) (values (cons k t) #f))]
         [else
          (values (append threads t)
                  (if (and (empty? threads) (empty? child)) #f ct))])]
      [(suspend-unless children threads signal)
       #:when (last? signal)
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

;; Reactor -> Void
;; reset the signal buffer for the next instant
(define (reset-reactor-signals! g)
  (define signals (reactor-signals g))
  (set-reactor-signals! g empty)
  (for ([S (in-list (remove-duplicates signals eq?))])
    (reset-signal! g S)))

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
  (when (signal-status S)
    (ready-signal! S)
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

;; ExternalReactor -> Boolean
;; does this reactor have no active threads, but have suspened threads?
(define (reactor-suspended? g)
  (define r (external-reactor-internal g))
  (and (ireactor-suspended? r)
       (not (hash-empty? (reactor-susps r)))))

;; Reactor -> Boolean
;; does this reactor have no active threads?
(define (ireactor-suspended? g)
  (empty? (reactor-active g)))

;; ExternalReactor -> Boolean
;; does this reactor have active threads or suspensions?
(define (reactor-done? r)
  (define g (external-reactor-internal r))
  (and
   (empty? (reactor-active g))
   (hash-empty? (reactor-susps g))))

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

;; ControlTree ControlTree -> Void
;; Effect: Add `nt` to the children of `cct`
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

;; Signal -> Void
;; Effect: unblock any threads waiting on this signal
(define (unblock-threads! S)
  (define grp (current-reactor))
  (define blocked (reactor-blocked grp))
  (define unblocked (hash-ref blocked S empty))
  (hash-remove! blocked S)
  (set-reactor-active!
   grp
   (append (reactor-active grp)
           (map blocked-present unblocked))))

;; Signal -> Void
;; Effect: Unblock all suspensions waiting for this signal.
(define (unblock-suspends! S)
  (define susps (reactor-susps (current-reactor)))
  (define trees (hash-ref susps S empty))
  (hash-remove! susps S)
  (for ([sp (in-list trees)])
    (activate-suspends! sp)))

;; ControlTree -> Void
;; awaken or activate all suspends in this tree
(define (activate-suspends! sp)
  (cond
    [(or (not (suspend-unless? sp))
         (signal-status (suspend-unless-signal sp)))
     (for-each activate! (control-tree-next sp))
     (set-control-tree-next! sp empty)
     (for-each activate-suspends! (control-tree-children sp))]
    [else
     (add-suspend! (reactor-susps (current-reactor)) sp)]))
  

;; (hasheq-of Signal SuspendUnless) (Listof SuspendUnless) -> Void
;; Effect: begin waiting on (activate) these suspends, given the suspension map
;; Invariant: Must be called during an instant, before signal cleanup.
(define (add-suspends! susps new-sp)
  (for ([sp (in-list new-sp)])
    (add-suspend! susps sp)))
(define (add-suspend! susps sp)
  (define S (suspend-unless-signal sp))
  (hash-set! susps
             S
             (cons sp (hash-ref susps S empty))))

;; ControlTree -> Any
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
                 (cons (make-blocked ct (continue-at p k) (continue-at q k))
                       (hash-ref blocked S empty)))
      (switch!))]))


;; ControlTree (Listof RThread) -> (Listof Any)
;; run all threads, blocking the current thread until all have completed
(define (parf ct threads)
  (define counter (length threads))
  (define cells (for/list ([t (in-list threads)]) (box #f)))
  ;; TODO optimization mentioned in RML paper:
  ;; pass boxed counter around, so that when
  ;; a thread is dynamically allocated it can
  ;; reuse/join the outer thread group
  (%% k
      (for ([t (in-list threads)]
            [cell (in-list cells)])
        (activate!
         (continue-at
          t
          (lambda (x)
            (set! counter (- counter 1))
            (set-box! cell x)
            (when (zero? counter)
              (activate! (lambda () (k (map unbox cells)))))
            (switch!)))))
     (switch!)))

(define emitf
  (case-lambda
    [(S) (emit-pure S)]
    [(S v) (emit-value S v)]))


;; (-> Any) (Any -> Nothing) -> (-> Nothing)
;; return a function that runs f, then jumps to k with the result of `(f)`,
;; all under the current parameterization
(define (continue-at f k)
  (extend-with-parameterization (lambda () (k (f)))))


;; (any ... -> any) -> (any ... -> any)
;; gives a function like f, but that sees
;; the current parameterization.
(define (extend-with-parameterization f)
  (define pz (current-parameterization))
  (lambda a
    (call-with-parameterization
     pz
     (lambda () (apply f a)))))

(module+ test
  (define p (make-parameter #f))
  (define f
    (parameterize ([p #t])
      (extend-with-parameterization
       (lambda () (p)))))
  (check-true (f)))