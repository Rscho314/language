#lang racket

(provide R7RS-environment)

(define R7RS-environment (make-hash))
(hash-set! R7RS-environment "add1" add1)