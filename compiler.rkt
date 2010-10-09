#lang racket

(require "ast.rkt"
         "parser.rkt")

(define (compile-file path)
  (call-with-input-file path compile-port))

(define (compile-port port)
  (read-line port)
  (let* ([prg (fix-one-armed-if
               (parse-basic-program port))]
         [blocks (build-blocks prg)]
         [vars   (find-program-vars prg)]
         [data   (find-program-data prg)])
    (let-values ([(start code) (compile-blocks prg blocks)])
      (let ([vcode (compile-vars vars)])        
        `(,@vcode
          (define data (list ,@data))
          ,@code
          (define (run)
            (let/ec end
              (parameterize ([basic-end-handler end])
                (,start end)))))))))

(define (fix-one-armed-if prg)
  (struct-copy Program prg
    [stmts (for/hash ([(n s) (in-dict (Program-stmts prg))])
             (match s
               [(SIf t c #f)
                (let ([next (next-line prg n)])
                  (unless next
                    (error 'fix-one-arm-if
                           "if statement appears to be last"))
                  (values n (SIf t c next)))]
               [_ (values n s)]))]))

(define (build-blocks prg)
  (define blocks (make-hash))
  
  (define (find-needed-block)
    (for/or ([(k v) (in-dict blocks)])
      (if (not v)
          k
          #f)))
  
  (define (process-next-block)
    (let ([n (find-needed-block)])
      (if n
          (process-stmt n null)
          blocks)))
  
  (define (process-stmt n cur-block)
    (let ([stmt (program-statement prg n)])
      (match stmt
        [(SPrint _) (block-add n cur-block)]
        [(SInput _) (block-add n cur-block)]
        [(SRead  _) (block-add n cur-block)]
        [(SLet _ _) (block-add n cur-block)]
        [(SData _)  (skip-stmt n cur-block)]
        [(SIf _ n1 n2) (new-block! n1)
                       (new-block! n2)
                       (finish-block n cur-block)]
        [(SEnd)     (finish-block n cur-block)]
        
        [(SGoto  j) (new-block! j)
                    (finish-block n cur-block)]

        [_ (error 'process-stmt
                  "write case for: ~a" stmt)])))
 
  (define (new-block! n)
    (unless (dict-ref blocks n #f)
      (dict-set! blocks n #f)))
 
  (define (skip-stmt n block)
    (let ([n (next-line prg n)])
      (if n
          (process-stmt n block)
          (process-next-block))))
  
  (define (finish-block n block)
    (let ([block (reverse (cons n block))])
      (dict-set! blocks (car block) block)
      (process-next-block)))
  
  (define (block-add n b)
    (let ([nn (next-line prg n)])
      (if nn
          (process-stmt nn (cons n b))
          (process-next-block))))
  
  (process-stmt (first-line prg) null))

(define (find-program-vars prg)
  (for/fold ([vars (hash)]) ([stmt (in-dict-values 
                                         (Program-stmts prg))])
    (find-vars stmt vars)))

(define (find-vars* s* vars)
  (for/fold ([vars vars]) ([s s*]) (find-vars s vars)))

(define (find-vars stmt vars)
  (match stmt
    [(SInput v)   (find-vars v vars)]
    [(SPrint e*)  (find-vars* e* vars)]
    [(SIf t _ _)  (find-vars t vars)]
    [(SRead v*)   (find-vars* v* vars)]
    [(SLet v e)   (find-vars e (dict-update vars v values 0))]
    [(SGoto _)    vars]
    [(SData _)    vars]
    [(SEnd)       vars]
    [(EOp _ l r)  (find-vars l (find-vars r vars))]
    [(ERef v)     (find-vars v vars)]
    [(EConst _)   vars]
    [(Var _)      (dict-update vars stmt values 0)]
    [_ (error 'find-vars
              "write case for: ~a" stmt)]))

(define (find-program-data prg)
  (for/fold ([data null]) ([i (in-list (Program-sequence prg))])
    (match (program-statement prg i)
      [(SData (list (EConst n*) ...))
       (append data n*)]
      [_ data])))

(define (compile-vars vars)
  (for/list ([(v dim) (in-dict vars)])
    `(define ,(Var-name v)
       ,(if (zero? dim)
            '(void)
            `(make-vector ,dim (void))))))

(define (compile-blocks prg blocks)
  (define block->name
    (for/hash ([n (in-dict-keys blocks)])
      (values n (gensym (format "block-~a." n)))))
  
  (values (dict-ref block->name (first-line prg))
          (for/list ([(n b) (in-dict blocks)])
            (let ([return (gensym 'return)])
              `(define ,(dict-ref block->name n)
                 (lambda (,return)
                   ,@(for/fold ([code null]) ([s (in-list b)])
                       (append code
                               (list
                                (compile-statement 
                                 return prg s block->name))))))))))

(define (compile-statement r p s b->n)
  (match (program-statement p s)
    [(SPrint e*)      `(basic-print ,@(map compile-expr e*))]
    [(SEnd)           `(basic-end)]
    [(SInput (Var v)) `(basic-input ,v)]
    [(SRead (list (Var v*) ...)) `(basic-read data ,@v*)]
    [(SLet (Var v) e) `(basic-let ,v ,(compile-expr e))]

    [(SIf test n1 n2) `(if ,(compile-expr test)
                           (,(dict-ref b->n n1) ,r)
                           (,(dict-ref b->n n2) ,r))]
    [(SGoto n)        `(,(dict-ref b->n n) ,r)]
    [x (error 'compile-statement
              "write case for: ~a" x)]))

(define (compile-expr e)
  (match e
    [(EConst val)      val]
    [(ERef (Var name)) name]
    [(EOp op l r) `(,op ,(compile-expr l) ,(compile-expr r))]
    [_ (error 'compile-expr
              "need to write case for: ~a" e)]))


(provide compile-port)

