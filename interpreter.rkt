#lang racket

(require "parser.rkt"
         "environment.rkt")

(define (R7RS-eval expression environment)
  (let* ([type (car expression)]
         [value (cdr expression)])
    (match type
      ['number
       (string->number (car value))]
      ['identifier
       (hash-ref R7RS-environment (car value))]
      ['procedure-call
       (let* ([operator
              (R7RS-eval (car value) R7RS-environment)]
              [operands
               (map (lambda (op) (R7RS-eval op R7RS-environment))
                    (cadr value))])
         (apply operator operands))]
      [else (error "AST node could not be evaluated: " value)])))

(R7RS-eval (R7RS-parse "(add1 1)") R7RS-environment)