#lang racket

(struct Program (sequence stmts) #:transparent)

(struct SLet    (var expr)          #:transparent)
(struct SEnd    ()                  #:transparent)
(struct SIf     (test then else)    #:transparent)
(struct SGoto   (dest)              #:transparent)
(struct SGosub  (dest)              #:transparent)
(struct SReturn ()                  #:transparent)
(struct SPrint  (exprs)             #:transparent)
(struct SInput  (id)                #:transparent)
(struct SFor    (id start end step) #:transparent)
(struct SNext   (id)                #:transparent)
(struct SDim    (id size)           #:transparent)
(struct SRem    ()                  #:transparent)
(struct SRead   (ids)               #:transparent)
(struct SData   (vals)              #:transparent)

(struct Var    (name) #:transparent)

(struct ERef   (var)       #:transparent)
(struct EConst (val)       #:transparent)
(struct EFun   (name args) #:transparent)
(struct EOp    (op l r)    #:transparent)

(define (first-line prg)
  (car (Program-sequence prg)))

(define (next-line prg n)
  (let ([seq (member n (Program-sequence prg))])
    (if (null? (cdr seq))
        #f
        (cadr seq))))

(define (program-statement prg n)
  (dict-ref (Program-stmts prg) n))

(provide (all-defined-out))