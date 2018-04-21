#lang racket
(provide %%
         raise-process-escape-error
         reactive-tag
         empty-calling-continuation
         sched-tag
         switch!
         debug-continuation!
         compose-continuations)
(require racket/control
         reactor/data
         (for-syntax syntax/parse syntax/srcloc))
(module+ test (require rackunit))

(define reactive-tag (make-continuation-prompt-tag 'reaction))
(struct delete-tag (start-prompt-tag end-prompt-tag) #:authentic)


(define (raise-process-escape-error)
  (error "process escaped reactor context"))

;; basic continuation grabbing
(define-syntax %%
  (syntax-parser
    [(_ k:id body ...)
     #`(begin
         (unless (continuation-prompt-available? reactive-tag)
           (raise-process-escape-error))
         #,(syntax/loc this-syntax ((call/comp
                                     (lambda (k) (lambda () body ...))
                                     reactive-tag))))]))

(define empty-calling-continuation
  (call/prompt
   (lambda () (%% k k))
   reactive-tag))

(define (compose-continuations k1 k2)
  (call/prompt
   (lambda ()
     (k1 (lambda () (k2 (lambda () (%% k (abort/cc reactive-tag k)))))))
   reactive-tag
   values))

(module+ test
  (define k1 (call/prompt
              (lambda ()
                (with-continuation-mark 1 1
                  (%% k (abort/cc reactive-tag k))))
              reactive-tag values))
  (define k2 (call/prompt
              (lambda ()
                (with-continuation-mark 1 2
                  (%% k (abort/cc reactive-tag k))))
              reactive-tag values))
  (check-equal?
   (continuation-mark-set->list (continuation-marks (compose-continuations k1 k2)) 1)
   (list 2))
  (check-equal?
   (continuation-mark-set->list
    (continuation-marks (compose-continuations (compose-continuations k1 k2) (compose-continuations k1 k2)))
    1)
   (list 2)))

;; OS tags

(define sched-tag (make-continuation-prompt-tag 'sched))



;; -> Any
;; switch back to the scheduler
(define (switch!)
  (abort/cc sched-tag))

(define (debug-continuation! k)
  (call/prompt
   (lambda () (k (lambda () (error 'debug "~a" k))))))