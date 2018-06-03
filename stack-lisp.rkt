#lang racket
(require syntax/readerr)
 
(provide read read-syntax)
 
(define (read in)
  (syntax->datum (read-syntax #f in)))
 
(define (read-syntax src in)
  (skip-whitespace in)
  (read-arith src in))
 
(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))
 
(define (read-arith src in)
  (define-values (line col pos) (port-next-location in))
  (define expr-match
    (regexp-match #px"^(?:[[:digit:]])(?: [[:digit:]])*" in))
 
  (define (to-syntax v delta span-str)
    (datum->syntax #f v (make-srcloc delta span-str)))
  (define (make-srcloc delta span-str)
    (and line
         (vector src line (+ col delta) (+ pos delta)
                 (string-length span-str))))
 
  (define (parse-expr s)
    (match (car (regexp-match #px"^(?:[[:digit:]])(?: [[:digit:]])*" s))
      [(pregexp #px"^(?:[[:digit:]])(?: [[:digit:]])+")
       (define elems (string-split s))
       (map parse-expr elems)
       ]
      [(pregexp #px"^[[:digit:]]$")
       (or (string->number s)
           (string->symbol s))]))
 
  (unless expr-match
    (raise-read-error "bad arithmetic syntax"
                      src line col pos
                      (and pos (- (file-position in) pos))))
  
;  (begin
;    (display (bytes->string/utf-8 (car expr-match)))
;    (newline)
;    (display expr-match)
;    (newline))
  
  (parse-expr (bytes->string/utf-8 (car expr-match))))