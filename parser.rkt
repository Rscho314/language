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
     [(exp) $1]
     [() #f])
    
    (exp [(IDENTIFIER)
          $1])
         )))