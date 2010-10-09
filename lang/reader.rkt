#lang s-exp syntax/module-reader
"lang.rkt"
#:read                read-basic
#:read-syntax         read-basic-syntax
#:whole-body-readers? #t

(require "../compiler.rkt")

(define (read-basic-syntax source port)
  (compile-port port))

(define (read-basic port)
  (error 'read-basic "not done"))

