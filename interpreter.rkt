#lang racket

(require "lexer.rkt")
(require "parser.rkt")

(define (R7RS-eval input-port)
  (port-count-lines! input-port)
  (letrec ((one-line
	    (lambda ()
	      (let ((result (R7RS-parser (lambda () (R7RS-lexer input-port)))))
		(when result
                  (printf "~a\n" result)
                  (one-line))))))
    (one-line)))
(R7RS-eval (open-input-string "hello"))
(R7RS-eval (open-input-string "'1"))
(R7RS-eval (open-input-string "'#f"))
(R7RS-eval (open-input-string "'raoul"))