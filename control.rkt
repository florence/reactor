#lang racket
(provide %%
         raise-process-escape-error
         reactive-tag
         empty-calling-continuation
         sched-tag
         switch!)
(require racket/control
         reactor/data
         (for-syntax syntax/parse))
(module+ test (require rackunit))

(define reactive-tag (make-continuation-prompt-tag 'reaction))
(struct delete-tag (start-prompt-tag end-prompt-tag) #:authentic)


(define (raise-process-escape-error)
  (error "process escaped reactor context"))

;; basic continuation grabbing
(define-syntax %%
  (syntax-parser
    [(_ k:id body ...)
     #'(begin
         (unless (continuation-prompt-available? reactive-tag)
           (raise-process-escape-error))
         ((call/comp (lambda (k) (lambda () body ...)) reactive-tag)))]))
(define empty-calling-continuation
  (call/prompt
   (lambda () (%% k k))
   reactive-tag))

;; OS tags

(define sched-tag (make-continuation-prompt-tag 'sched))


;; -> Any
;; switch back to the scheduler
(define (switch!)
  (abort/cc sched-tag))
