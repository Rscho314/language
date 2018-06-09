#lang racket

(require parser-tools/yacc
         (only-in "lexer.rkt"
                  value-tokens
                  delimiter-tokens))

(provide R7RS-parser)

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
    (expression [(IDENTIFIER) $1]
                [(literal) $1])
    (literal [(quotation) $1]
             [(self-evaluating) $1])
    (self-evaluating [(BOOLEAN) $1]
                     [(NUMBER) $1]
                     [(CHARACTER) $1]
                     [(STRING) $1]
                     )
    (quotation [(Q datum) $2])
    )))