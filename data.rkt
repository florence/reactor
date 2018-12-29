#lang racket
(provide (all-defined-out))
(require syntax/parse/define racket/contract/base
         racket/stxparam
         racket/stxparam-exptime)

;; ExternalReactor -> Void
(define (reactor-unsafe! r)
  (set-reactor-safe?! r #f))
;; ExternalReactor -> Void
(define (reactor-safe! r)
  (set-reactor-safe?! r #t))

;; A Reactor is a
;;  (reactor (Listof Thread) (hasheqof SignalName Blocked) ControlTree (hasheqof SignalName (Cons SuspendUnless (Listof Signal)) (Listof Signal) boolean)
(struct reactor (active blocked ct susps signals safe?)
  #:mutable)
;; `os` is the continuation for the OS loop
;; `active` are a list of runnable threads
;; `blocked` maps signals to blocked thread pairs
;; `ct` the control three for the entire program
;; `susps` is active suspensions, keys on their blocking signal. The signals
;; within the list are all signals that suspend will trigger on.
;; `signals` is a list of all signals in the program, that have been emitted
;;   they may be reset inbetween instants
;; `safe?` is if the reactor is safe to react with

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

(define-syntax-parameter gather-contract #f)
(define-syntax-parser  signal/c
  [(signal/c contracts ...)
   #:with (n ...) (generate-temporaries #'(contracts ...))
   #`(let ([n contracts] ...)
       (signal/c/f #,(if (syntax-parameter-value #'gather-contract)
                         #'(-> n ... n ... (values n ...))
                         #'any/c)
                   (list n ...)
                   '(signal/c contracts ...)))])
(define (signal/c/f arrow contracts name)
  (cond
    [(empty? contracts)
     (flat-named-contract name pure-signal?)]
    [else
     (define cn (length contracts))
     
     (make-chaperone-contract
      #:name name
      #:first-order (lambda (val)
                      (and (signal? val)
                           (= (length contracts) (length (value-signal-default val)))
                           (for/and ([c (in-list contracts)]
                                     [v (in-list (value-signal-default val))])
                             (contract-first-order-passes? c v))
                           (contract-first-order-passes? arrow (value-signal-gather val))))
      #:late-neg-projection
      (lambda (blame)
        (define arrow+blame
          ((contract-late-neg-projection arrow)
           (blame-add-context
            blame
            "The gather function of")))
        (define emit-contracts+blame
          (for/list ([x (in-list contracts)]
                     [i (in-naturals 1)])
            ((contract-late-neg-projection
              x)
             (blame-add-context
              blame
              (format "The ~a value emitted to" (n->th i))
              #:swap? #t))))
        (define read-contracts+blame
          (for/list ([x (in-list contracts)]
                     [i (in-naturals 1)])
            ((contract-late-neg-projection
              x)
             (blame-add-context
              blame
              (format "The ~a value contained in" (n->th i))))))
        (define default-contracts+blame
          (for/list ([x (in-list contracts)]
                     [i (in-naturals 1)])
            ((contract-late-neg-projection
              x)
             (blame-add-context
              blame
              (format "The ~a default of argument of" (n->th i))))))
        (lambda (val neg)
          (unless (value-signal? val)
            (raise-blame-error blame #:missing-party neg
                               val
                               (list 'expected "value-signal?" 'given "~v") val))
          (unless (= cn (length (value-signal-default val)))
            (raise-blame-error blame #:missing-party neg
                               val
                               (list 'expected "A signal carrying ~a values"
                                     'given
                                     "A signal named ~a, which carries ~a values with defaults: ~a")
                               cn
                               (signal-name val)
                               (length (value-signal-default val))
                               (map ~v (value-signal-default val))))
                               
          (define arrow+neg (arrow+blame (value-signal-gather val) neg))
          (define new-defaults
            (map
             (lambda (v c)
               (c v neg))
             (value-signal-default val)
             default-contracts+blame))
          (chaperone-struct
           val
           value-signal-default
           (lambda (self val) new-defaults)
           value-signal-gather
           (lambda (self val) arrow+neg)
           set-value-signal-collection!
           (lambda (self field)
             (if (empty? field)
                 empty
                 (cons (map (lambda (c v) (c v neg))
                            emit-contracts+blame
                            (first field))
                       (rest field))))
           
           value-signal-value
           (lambda (self field)
             (map (lambda (c v) (c v neg))
                  read-contracts+blame
                  field))))))]))
           
(define (n->th n)
  (string-append 
   (number->string n)
   (case (modulo n 10)
     [(1) "st"]
     [(2) "nd"]
     [(3) "rd"]
     [else "th"])))         