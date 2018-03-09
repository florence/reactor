#lang racket
(provide (all-defined-out))

;; ExternalReactor -> Void
(define (reactor-unsafe! r)
  (set-reactor-safe?! r #f))
;; ExternalReactor -> Void
(define (reactor-safe! r)
  (set-reactor-safe?! r #t))

;; A Reactor is a
;;  (reactor RThread (Listof Thread) (hasheqof SignalName Blocked) ControlTree (hasheqof SignalName SuspendUnless) (Listof Signal) boolean)
(struct reactor (os active blocked ct susps signals safe?)
  #:mutable)
;; `os` is the continuation for the OS loop
;; `active` are a list of runnable threads
;; `blocked` maps signals to blocked thread pairs
;; `ct` the control three for the entire program
;; `susps` is active suspensions, keys on their blocking signal
;; `signals` is a list of all signals in the program, that have been emitted
;;   they may be reset inbetween instants
;; `safe?` is if the reactor is safe to react with

;; a RThread is a (make-rthread Continuation (-> Any))
(struct rthread (k f)
  #:authentic
  #:constructor-name make-rthread)

(define hide-thread? (make-parameter #f))

(struct hidden-rthread rthread ()
  #:authentic)
(define (make-hidden-rthread k f)
  (hidden-rthread k (lambda () (parameterize ([hide-thread? #t]) (f)))))

(define (run-rthread r)
  ((rthread-k r) (rthread-f r)))

;; a process is a (make-process (ControlTree -> RThread))
(struct process (thunk)
  #:constructor-name make-process)
;; `thunk` constructs the initial thread for this process, given its control tree

;; a blocked is a (make-blocked ControlTree RThread RThread)
(struct blocked (ct present absent)
  #:constructor-name make-blocked
  #:authentic)
;; a blocked represents a thread awaiting a signals value.
;; it will run `present` if the signal is present or add `absent` to the control tree if it's not.

;; a signal is a one of
;; (make-value-signal boolean (make-signal-evt) uninterned-symbol A (listof A) (A A -> A))
;; (make-pure-signal boolean uninterned-symbol (make-signal-evt))

(struct signal ([status #:mutable] [last? #:mutable] name evt)
  #:property prop:evt (lambda (S) (wrap-evt (signal-evt S) (lambda (_) S)))
  #:methods gen:equal+hash
  [(define (equal-proc left right _)
     (and (signal? left)
          (signal? right)
          (eq? (signal-name left)
               (signal-name right))))
   (define (hash-proc self rec)
     (rec (signal-name self)))
   (define (hash2-proc self rec)
     (rec (signal-name self)))])

(define (make-signal-evt) (make-semaphore 0))
(define (ready-signal! S)
  (define e (signal-evt S))
  (semaphore-try-wait? e)
  (semaphore-post e))

(define (signal=? s1 s2)
  (eq? (signal-name s1) (signal-name s2)))

(struct pure-signal signal ()
  #:mutable)
;; `status` is false if the signal has not been emitted (is unknown), and true if it has

(define (make-pure-signal name)
  (pure-signal #f #f
               (string->uninterned-symbol (symbol->string name))
               (make-signal-evt)))
                             
(struct value-signal signal (value default collection gather)
  #:mutable)
;; `status` is as in `pure-signal`
;; `value` is the value of the signal for this instant. It is equal to the result of
;;     `gather` in the last instant if the signal was emitted, or the value of the previous instant
;;         otherwise
;; `collection` is the list of values it has been emitted with this instant
;; `gather` turns collection into the new value between instants

(define (make-value-signal name default collection gather)
  (value-signal #f #f
                (string->uninterned-symbol (symbol->string name))
                (make-signal-evt)
                default default collection gather))


(define (signal/c /c)
  (struct/c value-signal any/c any/c any/c any/c /c /c any/c (-> /c /c /c)))

;; a ControlTree is one of:
;;   (make-top (listof ControlTree) (Listof RThread))
;;   (make-suspend-unless (listof ControlTree) Signal (Listof RThread))
;;   (make-preempt-when (listof ControlTree) Signal (Listof RThread) (-> (Maybe Thread)))

(struct control-tree (children next)
  #:mutable
  #:authentic)

(struct top control-tree ()
  #:mutable
  #:constructor-name make-top
  #:authentic)
;; a node with no specific control
(struct suspend-unless control-tree (signal)
  #:mutable
  #:constructor-name make-suspend-unless
  #:authentic)
;; control becomes active when `signal` is emitted
(struct preempt-when control-tree (signal kont)
  #:mutable
  #:constructor-name make-preempt-when
  #:authentic)
;; tree is removed at the end of instant when the signal is present

