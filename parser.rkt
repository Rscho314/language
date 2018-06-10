#lang racket

(require parser-tools/yacc
         "lexer.rkt")

(provide R7RS-parse)

(define R7RS-parser
  (parser
   (start start)
   (end EOF)
   (tokens value-tokens delimiter-tokens)
   (error (lambda (tok-ok? tok-name tok-value)
            (display (list tok-ok? tok-name tok-value))))
   (grammar
    (start
     [(expression) $1]
     [() #f])
    
    (datum [(simple-datum) $1]
           )
    (simple-datum [(BOOLEAN) $1]
                  [(NUMBER) $1]
                  [(CHARACTER) $1]
                  [(STRING) $1]
                  [(symbol) $1])
    (symbol [(IDENTIFIER) $1])
    (expression [(IDENTIFIER) `(identifier ,$1)]
                [(literal) $1]
                [(procedure-call) $1])
    (literal [(quotation) $1]
             [(self-evaluating) $1])
    (self-evaluating [(BOOLEAN) $1]
                     [(NUMBER) `(number ,$1)]
                     [(CHARACTER) $1]
                     [(STRING) $1]
                     )
    (quotation [(Q datum) $2])
    (procedure-call [(OP operator operand CP) `(procedure-call ,$2 ,$3)])
    (operator [(expression) $1])
    (operand [(expression) (list $1)])
    )))

(define (parse parser lexer s)
(let ([input-port (open-input-string s)])
  (define (run)
    (port-count-lines! input-port)
    (parser (lambda () (lexer input-port))))
  (run)))

(define (R7RS-parse s)
  (parse R7RS-parser R7RS-lexer s))

;(parse R7RS-parser R7RS-lexer "(add1 1)")