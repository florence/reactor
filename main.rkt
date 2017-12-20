#lang racket
(provide
 start&
 run&
 process&
 define-process&
 present&
 signal&
 par&
 emit&
 pause&
 react&
 await&
 reactor-done?
 last&)
(require (for-syntax syntax/parse racket/stxparam-exptime racket/syntax) racket/hash racket/stxparam)
(module+ test (require rackunit))

(define current-reactor (make-parameter #f))
(struct process (thunk) #:constructor-name make-process #:authentic)
(struct reactor (os active blocked paused signals) #:mutable #:authentic)
(struct blocked (present absent) #:constructor-name make-blocked)

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
         (register-signal! S) ...
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

(define (start& proc)
  (make-reactor (process-thunk proc)))

(define (react& grp)
  (call-with-continuation-prompt
   (lambda () (os-loop! grp))
   reactive-tag))

(define (make-reactor proc)
  (reactor void (list proc) (make-hasheq) empty empty))

(define (os-loop! g)
  (match g
    [(reactor os (list) blocked paused signals)
     (define active
       (for*/list ([(_ procs) (in-hash blocked)]
                   [p (in-list procs)])
         (blocked-absent p)))
     (set-reactor-os! g void)
     (set-reactor-active! g (append active paused))
     (set-reactor-blocked! g (make-hasheq))
     (set-reactor-paused! g empty)
     (for ([S signals])
       (reset-signal! S))]
    [(reactor os active blocked paused signals)
     (define next (first active))
     (set-reactor-active! g (rest active))
     (call/cc
      (lambda (k)
        (set-reactor-os! g (lambda () (k (void))))
        (parameterize ([current-reactor g])
          (next)))
      reactive-tag)
     (os-loop! g)]))

(define (reset-signal! S)
  (when (and (value-signal? S) (signal-status S))
    (define c (value-signal-collection S))
    (set-value-signal-value!
     S
     (foldl (value-signal-gather S) (first c) (rest c)))
    (set-value-signal-collection! S empty))
  (set-signal-status! S #f))
  

(define (switch!)
  ((reactor-os (current-reactor))))

(define (register-signal! S)
  (set-reactor-signals!
   (current-reactor)
   (cons S (reactor-signals (current-reactor)))))

(define (reactor-done? g) (empty? (reactor-active g)))

(define (activate! thrd)
  (set-reactor-active!
   (current-reactor)
   (cons thrd (reactor-active (current-reactor)))))
(define (run-next! thrd)
  (set-reactor-paused!
   (current-reactor)
   (cons thrd (reactor-paused (current-reactor)))))

;;;;;; running

(define last& value-signal-value)
         
(define (runf proc) ((process-thunk proc)))

(define (emit-pure S)
  (unless (pure-signal? S)
    (error 'emit& "Expected pure signal, given ~s" S))
  (set-signal-status! S #t)
  (unblock! S))
(define (emit-value S v)
  (unless (value-signal? S)
    (error 'emit& "Expected value carrying signal, given ~s and ~s" S v))
  (set-signal-status! S #t)
  (set-value-signal-collection!
   S
   (cons v (value-signal-collection S)))
  (unblock! S))

(define (unblock! S)
  (define grp (current-reactor))
  (define blocked (reactor-blocked grp))
  (define unblocked (hash-ref blocked S empty))
  (hash-remove! blocked S)
  (set-reactor-active!
   grp
   (append (reactor-active grp)
           (map blocked-present unblocked))))

(define (pausef)
  (call/cc
   (lambda (k)
     (define grp (current-reactor))
     (run-next! (lambda () (k (void))))
     (switch!))
   reactive-tag))

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

(define-process& (await-immediate S)
  (present& S (void) (run& (await-immediate S))))

(define-process& (await-value S f)
  (present& S
            (begin
              pause&
              (f (last& S)))
            (run& (await-value S f))))

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