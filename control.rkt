#lang racket
(provide make-delete-tag delimit-delete-begin delimit-delete-end delete-continuation-frames!
         %%
         raise-process-escape-error
         reactive-tag)
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
         ((call/cc (lambda (k) (lambda () body ...)) reactive-tag)))]))                                                        

;; selectively deleting continuations
(define (make-delete-tag)
  (delete-tag (make-continuation-prompt-tag 'delete-tag-start)
              (make-continuation-prompt-tag 'delete-tag-end)))

(define-syntax delimit-delete-begin
  (syntax-parser
    [(_ key e ...)
     #'(call/prompt
        (lambda () e ...)
        (delete-tag-start-prompt-tag key)
        (lambda (E) (E (void))))]))

(define-syntax delimit-delete-end
  (syntax-parser
    [(_ key e ...)
     #'(call/prompt (lambda () e ...) (delete-tag-end-prompt-tag key))]))

(define (delete-continuation-frames! key)
  (call-with-composable-continuation
   (lambda (E)
     (abort/cc (delete-tag-start-prompt-tag key) E))
   (delete-tag-end-prompt-tag key)))
  
(module+ test
  (check-equal?
   (let ([b (box 'unset)]
         [d (make-delete-tag)])
     (delimit-delete-begin
      d
      (begin
        (delimit-delete-end
         d
         (begin (delete-continuation-frames! d)
                (set-box! b 'set-correctly)))
        (set-box! b 'set-incorrectly)))
     (unbox b))
   'set-correctly)
  (check-equal?
   (let ([b (box 'unset)]
         [d (make-delete-tag)])
     (begin
       (delimit-delete-begin
        d
        (begin
          (delimit-delete-end
           d
           (begin (delete-continuation-frames! d)
                  (set-box! b 'set-incorrectly-inner)))
          (set-box! b 'set-incorrectly-outer)))
       (set-box! b 'set-correctly))
     (unbox b))
   'set-correctly)

  (define (marks k)
    (continuation-mark-set->list (current-continuation-marks) k))
  (check-equal?
   (let ([d (make-delete-tag)])
     (delimit-delete-begin
      d
      (with-continuation-mark 'x 1
        (delimit-delete-end
         d
         (with-continuation-mark 'x 2
           (list (list->set (marks 'x))
                 (begin (delete-continuation-frames! d)
                        (list->set (marks 'x)))))))))
   (list (list->set (list 1 2)) (list->set (list 2))))
  (check-equal?
   (let ([d (make-delete-tag)]
         [dummy #f])
     (delimit-delete-begin
      d
      (begin
        (set! dummy 3)
        (with-continuation-mark 'x 1
          (begin
            (set! dummy 4)
            (delimit-delete-end
             d
             (with-continuation-mark 'x 2
               (list (list->set (marks 'x))
                     (begin (delete-continuation-frames! d)
                            (list->set (marks 'x))))))
            (set! dummy 5))))))
   (list (list->set (list 1 2)) (list->set (list 2))))
  (check-equal?
   (let ([d (make-delete-tag)]
         [dummy #f])
     (delimit-delete-begin
      d
      (begin
        (set! dummy 3)
        (with-continuation-mark 'x 1
          (begin
            (set! dummy 4)
            (call-with-exception-handler
             (lambda (exn) exn)
             (lambda ()
               (delimit-delete-end
                d
                (with-continuation-mark 'x 2
                  (list (list->set (marks 'x))
                        (begin (delete-continuation-frames! d)
                               (list->set (marks 'x))))))
               (set! dummy 5)))
            (set! dummy 6)
            'unreachable)))))
   (list (list->set (list 1 2)) (list->set (list 2))))

  (check-equal?
   (let ([d (make-delete-tag)])
     (with-continuation-mark 'x 1
       (delimit-delete-begin
        d
        (delimit-delete-end
         d
         (with-continuation-mark 'x 2
           (list (list->set (marks 'x))
                 (begin (delete-continuation-frames! d)
                        (list->set (marks 'x)))))))))
   (list (list->set (list 1 2)) (list->set (list 2))))
  (check-equal?
   (let ([d (make-delete-tag)]
         [dummy #f])
     (begin0
       (with-continuation-mark 'x 1
         (delimit-delete-begin
          d
          (begin
            (set! dummy 3)
            (begin
              (set! dummy 4)
              (call-with-exception-handler
               (lambda (exn) exn)
               (lambda ()
                 (delimit-delete-end
                  d
                  (with-continuation-mark 'x 2
                    (list (list->set (marks 'x))
                          (begin (delete-continuation-frames! d)
                                 (list->set (marks 'x))))))
                 (set! dummy 5)))
              (set! dummy 6)
              'unreachable))))
       (set! dummy 'end)))
   (list (list->set (list 1 2)) (list->set (list 2)))))

     


   
