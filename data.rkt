#lang racket
(provide (all-defined-out))
(require syntax/parse/define)

;; ExternalReactor -> Void
(define (reactor-unsafe! r)
  (set-reactor-safe?! r #f))
;; ExternalReactor -> Void
(define (reactor-safe! r)
  (set-reactor-safe?! r #t))

;; A Reactor is a
;;  (reactor (Listof Thread) (hasheqof SignalName Blocked) ControlTree (hasheqof SignalName SuspendUnless) (Listof Signal) boolean)
(struct reactor (active blocked ct susps signals safe?)
  #:mutable
  #:transparent)
;; `os` is the continuation for the OS loop
;; `active` are a list of runnable threads
;; `blocked` maps signals to blocked thread pairs
;; `ct` the control three for the entire program
;; `susps` is active suspensions, keys on their blocking signal
;; `signals` is a list of all signals in the program, that have been emitted
;;   they may be reset inbetween instants
;; `safe?` is if the reactor is safe to react with

;; a process is a (make-process (ControlTree -> RThread))
(struct process (thunk)
  #:constructor-name make-process)
;; `thunk` constructs the initial thread for this process, given its control tree

;; a blocked is a (make-blocked ControlTree ControlTree RThread RThread)
(struct blocked (parent blocking present absent)
  #:constructor-name make-blocked
  #:authentic)
;; a blocked represents a thread awaiting a signals value.
;; the `current` value is used for looking up which thread to replace
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


(define-syntax signal/c
  (syntax-parser
    [(_) #'pure-signal?]
    [(_ /c:expr ...)
     #:with (x ...) (generate-temporaries #'(/c ...))
     #'(let ([x /c] ...)
         (struct/c value-signal any/c any/c any/c any/c (list/c x ...) (list/c x ...) any/c (-> x ... x ... x ...)))]))

