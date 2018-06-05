#lang racket

;R7RS lexer
; example lexer: http://planet.racket-lang.org/package-source/soegaard/infix.plt/1/0/parser.ss
; r7rs standard: https://bitbucket.org/cowan/r7rs/raw/4c27517de187142ad2cf4bcd8cb9199ae1e48c09/rnrs/r7rs.pdf

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;TODO What about the 2nd token line std p.62
(define-tokens value-tokens
  (IDENTIFIER
   BOOLEAN
   NUMBER
   CHARACTER
   STRING
   SYMBOL))

(define-empty-tokens intrinsinc-tokens
  (LP CP  ;( )
   OV     ;#(
   OB     ;u8(
   Q      ;'
   QQ     ;`
   UQ     ;,
   UQS))  ;,@

; this respects the standard scrupulously
; but it overwrites some racket constructs such as "whitespace"
; it also does not include the unicode standard p.61
(define-lex-abbrevs
  [intraline-whitespace (:or " " "\t")]
  [line-ending (:or "\n" "\r\n")]
  [whitespace (:or intraline-whitespace line-ending)]
  [delimiter (:or whitespace #\| #\( #\) #\" #\; )]
; TODO conntinue here
  [letter     (:or (:/ #\a #\z) (:/ #\A #\Z) )]
  [digit      (:/ #\0 #\9)]
  [hex-digit  (:or digit (:/ #\a #\f))]
  [identifier (:: letter (:* (:or letter digit #\_ #\?)))]) ;wrong

;(define R7RS-small-lexer
;  (lexer))