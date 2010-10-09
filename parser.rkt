#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc         
         "ast.rkt")

(define-empty-tokens basic-punct-tokens
  (nl lpar rpar comma
      plus minus star slash caret
      lt gt eq neq geq leq
      GOTO GOSUB RETURN 
      PRINT INPUT LET 
      IF THEN ELSE 
      FOR TO STEP NEXT
      DIM DATA
      READ REM END))

(define-tokens basic-tokens
  (string int id))

(define basic-lex
  (lexer
   [(eof)             (token-nl)]
   
   [whitespace        (basic-lex input-port)]
   
   [#\( (token-lpar)]
   [#\) (token-rpar)]
   [#\, (token-comma)]
   
   ; Keywords
   ["GOTO"   (token-GOTO)]
   ["GOSUB"  (token-GOSUB)]
   ["RETURN" (token-RETURN)]
   ["PRINT"  (token-PRINT)]
   ["INPUT"  (token-INPUT)]
   ["LET"    (token-LET)]
   ["IF"     (token-IF)]
   ["THEN"   (token-THEN)]
   ["ELSE"   (token-ELSE)]
   ["FOR"    (token-FOR)]
   ["TO"     (token-TO)]
   ["STEP"   (token-STEP)]
   ["NEXT"   (token-NEXT)]
   ["DIM"    (token-DIM)]
   ["REM"    (begin (read-line input-port) 
                    (token-REM))]
   ["END"    (token-END)]
   ["READ"   (token-READ)]
   ["DATA"   (token-DATA)]
   
   [(:: alphabetic
        (:* (:or alphabetic
                 (char-set "0123456789"))))
    (token-id  (Var (string->symbol lexeme)))]
   
   [#\" (let loop ([c (peek-char input-port)]
                   [s null])
          (cond [(eof-object? c) (error 'basic-lex "eof in string")]
                [(char=? #\" c)  (read-char input-port) 
                                 ((compose token-string
                                           list->string
                                           reverse) s)]
                [else
                 (let ([c (read-char input-port)])
                   (loop (peek-char input-port) (cons c s)))]))]
   
   [(:: (:? #\-)
        (:+ (char-set "0123456789")))
    (token-int (string->number lexeme))]
   
   ; Operators
   [#\+  (token-plus)]
   [#\-  (token-minus)]
   [#\*  (token-star)]
   [#\/  (token-slash)]
   [#\^  (token-caret)]
   [#\<  (token-lt)]
   [#\>  (token-gt)]
   [#\=  (token-eq)]
   ["<>" (token-neq)]
   [">=" (token-geq)]
   ["<=" (token-leq)]))

(define basic-statement-parser
  (parser
   [tokens basic-punct-tokens basic-tokens]
   [start  statement]
   [end    nl]
   [error  (lambda x
             (error 'basic-statement-parser "~a" x))]
   
   [grammar
    (statement 
     [(int stmt) (cons $1 $2)])
    
    (stmt
     [(IF rel THEN int)                  (SIf $2 $4 #f)]
     [(IF rel THEN int ELSE int)         (SIf $2 $4 $6)]
     [(LET id eq expr)                   (SLet $2 $4)]
     [(GOTO int)                         (SGoto $2)]
     [(GOSUB int)                        (SGosub $2)]
     [(RETURN)                           (SReturn)]
     [(PRINT arg-list)                   (SPrint $2)]
     [(INPUT id)                         (SInput $2)]
     [(FOR id eq expr TO expr)           (SFor $2 $4 $6 1)]
     [(FOR id eq expr TO expr STEP expr) (SFor $2 $4 $6 $8)]
     [(NEXT id)                          (SNext $2)]
     [(DIM id int)                       (SDim $2 $3)]
     [(REM)                              (SRem)]
     [(READ id-list)                     (SRead $2)]
     [(DATA arg-list)                    (SData $2)]
     [(END)                              (SEnd)])
    
    (rel
     [(expr relop expr) (EOp $2 $1 $3)])
    
    (relop
     [(lt)  '<]
     [(gt)  '>]
     [(eq)  '=]
     [(neq) '<>]
     [(geq) '>=]
     [(leq) '<=])
    
    (expr
     [(sexpr)        $1])
    
    (primary
     [(lpar expr rpar) $2]
     [(int)            (EConst $1)]
     [(string)         (EConst $1)]     
     [(id)             (ERef   $1)])
    
    (fexpr
     [(id lpar arg-list rpar) (EFun $1 $3)]
     [(primary)               $1])
    
    (pexpr
     [(fexpr)             $1]
     [(fexpr caret pexpr) (EOp '^ $1 $3)])
    
    (mexpr
     [(pexpr)             $1]
     [(mexpr star  pexpr) (EOp '* $1 $3)]
     [(mexpr slash pexpr) (EOp '/ $1 $3)])
    
    (sexpr
     [(mexpr)             $1]
     [(sexpr plus mexpr)  (EOp '+ $1 $3)]
     [(sexpr minus mexpr) (EOp '- $1 $3)])
    
    (id-list
     [(id)               (list $1)]
     [(id comma id-list) (cons $1 $3)])
    
    (arg-list
     [()                    null]
     [(expr comma arg-list) (cons $1 $3)]
     [(expr)                (list $1)])]))

(define (parse-basic-program port)
  (let loop ([line (read-line port)]
             [prg  null])
    (if (eof-object? line)
        (let ([stmts (make-immutable-hash prg)])
          (Program (sort (for/list ([n (in-hash-keys stmts)]) n) <)
                   stmts))
        (loop (read-line port)
              (cons (call-with-input-string line
                      (lambda (p)
                        (basic-statement-parser
                         (lambda () (basic-lex p)))))
                    prg)))))

(provide parse-basic-program)
