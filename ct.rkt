#lang debug racket
(provide (all-defined-out))

(require racket/generic reactor/data reactor/control racket/control
         syntax/parse/define)

(define current-rthread-key (make-continuation-mark-key 'current-rthread-key))
(define (current-rthread)
  (continuation-mark-set-first (current-continuation-marks) current-rthread-key))

(define (run-rthread ct t)
  (define n (find-execution-path ct t))
  (unless n
    (error
     'internal
     "unable to find thread execution context! Thread: ~a, Context: ~a"
     t
     ct))
  (define-values (_ v) ((build-execution-context (first n) (rest n))))
  v)

(define (cleanup-control-tree! ct)
  (preempt-threads! ct)
  (cleanup-joins! ct))

(define-generics ct
  ;; This RThread -> (maybe (non-empty-listof ControlTree))
  ;; result list will terminate in an RThread
  (find-execution-path ct thread)
  ;; This (listof ControlTree) -> (-> Any)
  (build-execution-context ct next)
  ;; This (RThread -> Void) (Signal ControlTree -> Void)
  (register-context-as-active! ct activate! activate-on-signal!)
  ;; This ControlTree ControlTree -> Void
  (replace-child! ct old new)
  ;; This -> Void
  ;; Effect: Removes unneeded join points
  ;; INVARNIANT: must be called during an instant, after threads have been preempted.
  (cleanup-joins! ct)
  ;; This -> (-> (-> Any) Any)
  (get-control-code ct)
  ;; This -> ControlTree
  ;; a control tree to replace this one with in case it must be demoted to a leaf
  ;; Effect: preempt any threads that need to be killed
  ;; INVARIANT: Must be called during an instant, after signals are reset for the next instant
  (preempt-threads! ct)
  ;; This -> (listof RThread)
  ;; get any threads that should start active in the next instant
  ;; INVARIANT: Must be called during an instant, after signals are reset for the next instant, and after
  ;;   threads have been preempted and join points cleaned up.
  (get-next-active ct)
  ;; This -> (listof ControlTree)
  (get-top-level-susps ct)
  ;; This -> ContinuationMarkTree
  (continuation-mark-tree ct)
  #:fallbacks
  [(define/generic bec build-execution-context)
   (define/generic gcc get-control-code)
   (define (get-control-code self)
     (base-get-control-code self))
   (define (build-execution-context self next)
     (unless (cons? next)
       (error 'internal "Invalid execution context!"))
     (define control (gcc self))
     (define inner (bec (first next) (rest next)))
     (lambda () (control inner)))])

;; ASSUMPTION: The implementation assumes that threads need no cleanup code
(define (base-get-control-code self)
  (lambda (f)
    (define k (control-tree-k self))
    (define parent (current-rthread))
    ;; TODO this doesn't account for thread hiding
    (define sat (self-as-thread self))
    (call-before-k self sat k f (lambda (_ v) (replace-child! parent self sat) v))))

(define-simple-macro (control-tree-delimit e ...)
  (call-with-continuation-barrier
   (lambda ()
     (call/prompt
      (lambda () e ...)
      reactive-tag))))

(define (call-before-k tree sat k f code)
  (with-continuation-mark current-rthread-key sat
    (values
     sat
     (control-tree-delimit
      (k
       (lambda ()
         (with-continuation-mark current-rthread-key tree
           (let-values ([(nt v)
                         (control-tree-delimit (f))])
            
             (code nt v)))))))))
  

(define (self-as-thread ct)
  ;; TODO thread hiding
  (make-rthread (control-tree-k ct) void))

;; a ControlTree is one of:
;;   RThread
;;   (make-top ControlTree)
;;   (make-par Continuation (listof ControlTree))
;;   (make-suspend-unless Continuation ControlTree Signal)
;;   (make-preempt-when Continuation ControlTree Signal (-> (Maybe Thread)))

;; a RThread is one of
;;   (make-rthread Continuation (-> Any))
;;   (make-hidden-rthread Continuaiton (-> Any)

;; A ContinuationTree is one of
;; (continuation-mark-leaf (Listof ContinuationMarkSet))
;; (continuation-mark-branch (Listof ContinutionMarkSets) (Listof ContinuationMarkTree))
(struct continuation-mark-leaf (sets)
  #:reflection-name 'continuation-mark-tree)
(struct continuation-mark-branch (sets branches)
  #:reflection-name 'continuation-mark-tree)

(define (continuation-mark-tree? x)
  (or (continuation-mark-leaf? x) (continuation-mark-branch? x)))

(define (continuation-mark-tree->tree cmt key)
  (match cmt
    [(continuation-mark-leaf sets)
     (append-map (lambda (set) (continuation-mark-set->list set key)) sets)]
    [(continuation-mark-branch sets branches)
     (cons
      (append-map (lambda (s) (continuation-mark-set->list s key)) sets)
      (map continuation-mark-tree->tree branches))]))
(define (continuation-mark-tree-cons-set set ct)
  (match ct
    [(continuation-mark-leaf sets)
     (continuation-mark-leaf (cons set sets))]
    [(continuation-mark-branch sets branches)
     (continuation-mark-branch (cons set sets) branches)]))
    
  


(struct control-tree (k)
  #:authentic
  #:mutable
  #:transparent)


;                             
;                             
;                             
;                             
;                             
;  ;;;;;;;;                   
;  ;;;;;;;;                   
;     ;;       ;;;    ;; ;;   
;     ;;      ;;;;;   ;;;;;;  
;     ;;     ;;   ;;  ;;   ;; 
;     ;;     ;;   ;;  ;;   ;; 
;     ;;     ;;   ;;  ;;   ;; 
;     ;;     ;;   ;;  ;;  ;;  
;     ;;      ;;;;;   ;;;;;;  
;     ;;       ;;;    ;;;;    
;                     ;;      
;                     ;;      
;                     ;;      
;                             
;                             

(struct top (child)
  #:authentic
  #:transparent
  #:mutable
  #:constructor-name make-top
  #:methods gen:ct
  [(define/generic fep find-execution-path)
   (define/generic rcaa! register-context-as-active!)
   (define/generic cj! cleanup-joins!)
   (define/generic super-get-control-code get-control-code)
   (define/generic gna get-next-active)
   (define/generic pt! preempt-threads!)
   (define/generic gtls get-top-level-susps)
   (define/generic cmt continuation-mark-tree)
   (define/generic rp! replace-child!)
   (define (find-execution-path self thread)
     (define inner (fep (top-child self) thread))
     (and inner (cons self inner)))
   (define (register-context-as-active! self activate! activate-on-signal!)
     (when (top-child self)
       (rcaa! (top-child self) activate! activate-on-signal!)))
   (define (replace-child! ct old new)
     (unless (eq? old (top-child ct))
       (error 'internal "thread lost context:\n at ~a:~a\n~a\nold ~a\n~a\nnew ~a\n~a"
              (eq-hash-code ct)
              (eq-hash-code (top-child ct))
              ct
              (eq-hash-code old)
              old
              (eq-hash-code new)
              new))
     (set-top-child! ct new))
   (define (cleanup-joins! self)
     (when (top-child self)
       (define c (top-child self)) 
       (cj! (top-child self))
       (set-top-child! self (maybe-collapse-join c))))
   (define (get-control-code self)
     (lambda (f)
       (call-before-k
        self #f empty-calling-continuation f
        (lambda (_nt _v) (set-top-child! self #f)))))
   (define (get-next-active self)
     (if (top-child self)
         (gna (top-child self))
         empty))
   (define (preempt-threads! self)
     (when (top-child self)
       (set-top-child! self (pt! (top-child self))))
     self)
   (define (get-top-level-susps self)
     (if (top-child self)
         (gtls (top-child self))
         empty))
   (define (continuation-mark-tree self)
     (if (top-child self)
         (cmt (top-child self))
         (continuation-mark-leaf empty)))])

;                                                        
;                                                        
;                                                        
;                                                        
;            ;;                                       ;; 
;  ;;;;;;;;  ;;                                       ;; 
;  ;;;;;;;;  ;;                                       ;; 
;     ;;     ;; ;;;   ;;;; ;;    ;;;     ;;;        ;;;; 
;     ;;     ;;;;;;;  ;;;;;;;   ;;;;;    ;;;;;    ;;;;;; 
;     ;;     ;;   ;;    ;;;     ;   ;;       ;;   ;;  ;; 
;     ;;     ;;   ;;    ;;     ;;;;;;;     ;;;;  ;;   ;; 
;     ;;     ;;   ;;    ;;     ;;;;;;;   ;;;;;;  ;;   ;; 
;     ;;     ;;   ;;    ;;     ;;       ;;   ;;  ;;   ;; 
;     ;;     ;;   ;;  ;;;;;;    ;;;;;;  ;;;;;;;   ;;;;;; 
;     ;;     ;;   ;;  ;;;;;;      ;;;;   ;;; ;;    ;; ;; 
;                                                        
;                                                        
;                                                        
;                                                        
;                                                        


(struct rthread control-tree (f)
  #:authentic
  #:transparent
  #:mutable
  #:constructor-name make-rthread
  #:methods gen:ct
  [(define (find-execution-path self thread)
     (and (eq? self thread) (list self)))
   (define (build-execution-context self below)
     (unless (empty? below)
       (error 'internal "incorrect execution context when running thread!"))
     (lambda ()
       (with-continuation-mark current-rthread-key self
         (values
          self
          (control-tree-delimit ((control-tree-k self) (rthread-f self)))))))
   (define (register-context-as-active! self activate! activate-on-signal!)
     (activate! self))
   (define (replace-child! ct old new)
     (error 'interal "threads have no children!"))
   (define (cleanup-joins! self) (void))
   (define (get-control-code ct) (error 'internal "thread control code generation should be unreachable!"))
   (define (get-next-active self) (list self))
   (define (preempt-threads! self) self)
   (define (get-top-level-susps self) empty)
   (define (continuation-mark-tree self)
     (continuation-marks (control-tree-k self)))])

(define hide-thread? (make-parameter #f))

(struct hidden-rthread rthread ()
  #:authentic
  #:transparent)
(define (make-hidden-rthread k f)
  (hidden-rthread k (lambda () (parameterize ([hide-thread? #t]) (f)))))


;                             
;                             
;                             
;                             
;                             
;   ;;;;;                     
;   ;;;;;;;                   
;   ;;   ;;   ;;;     ;;;; ;; 
;   ;;   ;;   ;;;;;   ;;;;;;; 
;   ;;   ;;       ;;    ;;;   
;   ;;;;;;      ;;;;    ;;    
;   ;;;;      ;;;;;;    ;;    
;   ;;       ;;   ;;    ;;    
;   ;;       ;;;;;;;  ;;;;;;  
;   ;;        ;;; ;;  ;;;;;;  
;                             
;                             
;                             
;                             
;                             


(struct par control-tree (children)
  #:mutable
  #:constructor-name make-par
  #:authentic
  #:transparent
  #:methods gen:ct
  [(define/generic fep find-execution-path)
   (define/generic rcaa! register-context-as-active!)
   (define/generic cj! cleanup-joins!)
   (define/generic super-get-control-code get-control-code)
   (define/generic gna get-next-active)
   (define/generic pt! preempt-threads!)
   (define/generic gtls get-top-level-susps)
   (define/generic cmt continuation-mark-tree)
   (define/generic rp! replace-child!)
   (define (find-execution-path self thread)
     (define next
       (for/or ([p (in-list (par-children self))])
         (fep p thread)))
     (and next (cons self next)))
   (define (register-context-as-active! self activate! activate-on-signal!)
     (for ([c (in-list (par-children self))])
       (rcaa! c activate! activate-on-signal!)))
   (define (replace-child! self old new)
     (define new-children
       (let loop ([l (par-children self)])
         (match l
           [(list)
            (error 'internal
                   "Thread leaked to incorrect context!\n context: par@~v\nchildren: ~v\n thread: ~v\nreplacement: ~v\n"
                   (eq-hash-code self)
                   (map eq-hash-code (par-children self))
                   (eq-hash-code old)
                   (eq-hash-code new))]
           [(cons a b)
            (if (eq? a old)
                (cons new b)
                (cons a (loop b)))])))
     (set-par-children! self new-children))
   (define (cleanup-joins! self)
     (define new-threads (map maybe-collapse-join (par-children self)))
     (for-each cj! new-threads)
     (set-par-children! self new-threads)) 
   (define (get-control-code self)
     (lambda (f)
       (define k (control-tree-k self))
       (define parent (current-rthread))
       (define final? (empty? (rest (par-children self))))
       ;; TODO this doesn't account for thread hiding
       (define sat (self-as-thread self))
       (call-before-k
        self sat k f
        (lambda (child _)
          (set-par-children! self (remq child (par-children self)))
          (if final?
              (rp! parent self sat)
              (switch!))))))
   (define (get-next-active self)
     (append-map gna (par-children self)))
   (define (preempt-threads! self)
     (define new (map pt! (par-children self)))
     (set-par-children! self new)
     self)
   (define (get-top-level-susps self)
     (append-map gtls (par-children self)))
   (define (continuation-mark-tree self)
     (define top (continuation-marks (control-tree-k self)))
     (define children (map cmt (par-children self)))
     (continuation-mark-branch (list top) children))])


;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                              ;; 
;      ;;;                                                     ;; 
;    ;;;;;                                                     ;; 
;   ;;       ;;   ;;    ;;;;   ;; ;;      ;;;    ;; ;;;      ;;;; 
;   ;;       ;;   ;;   ;;;;;;  ;;;;;;    ;;;;;   ;;;;;;;   ;;;;;; 
;    ;;;     ;;   ;;  ;;       ;;   ;;   ;   ;;  ;;   ;;   ;;  ;; 
;      ;;;   ;;   ;;   ;;;     ;;   ;;  ;;;;;;;  ;;   ;;  ;;   ;; 
;        ;;  ;;   ;;     ;;;   ;;   ;;  ;;;;;;;  ;;   ;;  ;;   ;; 
;        ;;  ;;   ;;       ;;  ;;  ;;   ;;       ;;   ;;  ;;   ;; 
;   ;;;;;;   ;;;;;;;  ;;;;;;   ;;;;;;    ;;;;;;  ;;   ;;   ;;;;;; 
;   ;;;;      ;;; ;;  ;;;;;    ;;;;        ;;;;  ;;   ;;    ;; ;; 
;                              ;;                                 
;                              ;;                                 
;                              ;;                                 
;                                                                 
;                                                                 


;; a node with may children
(struct suspend-unless control-tree (child signal)
  #:mutable
  #:constructor-name make-suspend-unless
  #:authentic
  #:transparent
  #:methods gen:ct
  [(define/generic fep find-execution-path)
   (define/generic rcaa! register-context-as-active!)
   (define/generic cj! cleanup-joins!)
   (define/generic super-get-control-code get-control-code)
   (define/generic gna get-next-active)
   (define/generic pt! preempt-threads!)
   (define/generic gtls get-top-level-susps)
   (define/generic cmt continuation-mark-tree)
   (define (find-execution-path self thread)
     (define next (fep (suspend-unless-child self) thread))
     (and next (cons self next)))
   (define (register-context-as-active! self activate! activate-on-signal!)
     (if (signal-status (suspend-unless-signal self))
         (rcaa! (suspend-unless-child self) activate! activate-on-signal!)
         (activate-on-signal! self)))
   (define (replace-child! self old new)
     (unless (eq? (suspend-unless-child self) old)
       (error 'internal "Thread leaked to incorrect context!"))
     (set-suspend-unless-child! self new))
   (define (cleanup-joins! self)
     (define c (suspend-unless-child self))
     (cj! c)
     (set-suspend-unless-child! self (maybe-collapse-join c)))
   (define (get-next-active self) empty)
   (define (preempt-threads! self)
     (define new (pt! (suspend-unless-child self)))
     (set-suspend-unless-child! self new)
     self)
   (define (get-top-level-susps self) (list self))
   (define (continuation-mark-tree self)
     (continuation-mark-tree-cons-set
      (continuation-marks (control-tree-k self))
      (cmt (suspend-unless-child self))))])


;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                                 
;   ;;;;;                                                   ;;    
;   ;;;;;;;                                                 ;;    
;   ;;   ;;  ;;;; ;;    ;;;      ;;;    ;;; ;;   ;; ;;    ;;;;;;; 
;   ;;   ;;  ;;;;;;;   ;;;;;    ;;;;;   ;;;;;;;  ;;;;;;   ;;;;;;; 
;   ;;   ;;    ;;;     ;   ;;   ;   ;;  ;;;; ;;  ;;   ;;    ;;    
;   ;;;;;;     ;;     ;;;;;;;  ;;;;;;;  ;;;; ;;  ;;   ;;    ;;    
;   ;;;;       ;;     ;;;;;;;  ;;;;;;;  ;;;; ;;  ;;   ;;    ;;    
;   ;;         ;;     ;;       ;;       ;;;; ;;  ;;  ;;     ;;    
;   ;;       ;;;;;;    ;;;;;;   ;;;;;;  ;;;; ;;  ;;;;;;     ;;;;; 
;   ;;       ;;;;;;      ;;;;     ;;;;  ;; ; ;;  ;;;;         ;;; 
;                                                ;;               
;                                                ;;               
;                                                ;;               
;                                                                 
;                                                                 


;; control becomes active when `signal` is emitted
(struct preempt-when control-tree (child signal kont)
  #:mutable
  #:constructor-name make-preempt-when
  #:authentic
  #:transparent
  #:methods gen:ct
  [(define/generic fep find-execution-path)
   (define/generic rcaa! register-context-as-active!)
   (define/generic cj! cleanup-joins!)
   (define/generic super-get-control-code get-control-code)
   (define/generic gna get-next-active)
   (define/generic pt! preempt-threads!)
   (define/generic gtls get-top-level-susps)
   (define/generic cmt continuation-mark-tree)
   (define (find-execution-path self thread)
     (define next (fep (preempt-when-child self) thread))
     (and next (cons self next)))
   (define (register-context-as-active! self activate! active-on-signal!)
     (rcaa! (preempt-when-child self)))
   (define (replace-child! self old new)
     (unless (eq? (preempt-when-child self) old)
       (error 'internal "Thread leaked to incorrect context!"))
     (set-preempt-when-child! self new))
   (define (cleanup-joins! self)
     (define c (preempt-when-child self))
     (cj! c)
     (set-preempt-when-child! self (maybe-collapse-join c)))
   (define (get-next-active self)
     (gna (preempt-when-child self)))
   (define (preempt-threads! self)
     (define signal (preempt-when-signal self))
     (define g (preempt-when-kont self))
     (cond [(and (signal-last? signal) (g))
            =>
            (lambda (k) k)]
           [else
            (define new (pt! (preempt-when-child self)))
            (set-preempt-when-child! self new)
            self]))
   (define (get-top-level-susps self)
     (gtls (preempt-when-child self)))
   (define (continuation-mark-tree self)
     (continuation-mark-tree-cons-set
      (continuation-marks (control-tree-k self))
      (cmt (preempt-when-child self))))])
           
;; tree is removed at the end of instant when the signal is present


(define (maybe-collapse-join ct)
  (cond
    [(and (par? ct) (empty? (rest (par-children ct))))
     (define f (first (par-children ct)))
     (set-control-tree-k! f (compose-continuations (control-tree-k ct) (control-tree-k f)))
     f]
    [else ct]))