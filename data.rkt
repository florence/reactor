#lang racket
(provide (all-defined-out))

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
