#lang racket

(define basic-print
  (case-lambda
    [()       (newline)]
    [(x)      (display x)
              (newline)]
    [(x . x*) (display x) 
              (display " ") 
              (apply basic-print x*)]))

(define-syntax-rule (basic-input x)
  (let ([ans (read-line)])
    (set! x ans)))

(define-syntax-rule (basic-let x e)
  (set! x e))

(define-syntax-rule (basic-read data x ...)
  (begin
    (cond [(null? data) (basic-end)]
          [else (set! x    (car data))
                (set! data (cdr data))]) ...))

(define basic-end-handler
  (make-parameter
   (lambda () (exit))))     

(define (basic-end)
  ((basic-end-handler)))

(provide basic-let
         basic-read
         basic-print
         basic-input
         basic-end
         basic-end-handler
         (all-from-out racket))