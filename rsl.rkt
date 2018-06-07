#lang racket

;R7RS lexer
; example lexer: http://planet.racket-lang.org/package-source/soegaard/infix.plt/1/0/parser.ss
; r7rs standard: https://bitbucket.org/cowan/r7rs/raw/4c27517de187142ad2cf4bcd8cb9199ae1e48c09/rnrs/r7rs.pdf

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

; this works only when reading from a file
; or when the port has been created with open-input-string
; does not work when reading from stdin
(define (unget port)
  (file-position port (- (file-position port) 1)))

;TODO What about the 2nd token line std p.62
(define-tokens value-tokens
  (IDENTIFIER
   BOOLEAN
   NUMBER
   CHARACTER
   STRING
   SYMBOL))

(define-empty-tokens marker-tokens
  (LP CP  ;( )
   OV     ;#(
   OB     ;u8(
   Q      ;'
   QQ     ;`
   UQ     ;,
   UQS    ;,@
   EOF))

; this respects the standard scrupulously
; but it overwrites many imported constructs such as "whitespace"
; so be careful to check everything is defined explicitely as per std
; it also does not include the unicode standard p.61
(define-lex-abbrevs
  [intraline-whitespace (:or " " "\t")]
  [line-ending (:or "\n" "\r\n")]
  [whitespace (:or intraline-whitespace line-ending)]
  [delimiter (:or whitespace #\| #\( #\) #\" #\; )]
  [comment-start (:or #\; "#|" "#;")] ; not used currently
  [directive (:or "#!fold-case" "#!no-fold-case")]
  [atmosphere (:or whitespace comment-start directive)]
  [intertoken-space (:* atmosphere)]
  [special-initial (:or #\! #\$ #\% #\& #\* #\/ #\: #\< #\=
                        #\> #\? #\@ #\^ #\_ #\~)]
  [letter (:or (:/ #\a #\z) (:/ #\A #\Z) )]
  [initial (:or letter special-initial)]
  [digit (:/ #\0 #\9)]
  [explicit-sign (:or #\+ #\-)]
  [special-subsequent (:or explicit-sign #\. #\@)]
  [subsequent (:or initial digit special-subsequent)]
  [hex-digit  (:or digit (:/ #\a #\f))]
  [hex-scalar-value (:+ hex-digit)]
  [inline-hex-escape (:: "\\x" hex-scalar-value #\;)]
  [mnemonic-escape (:or "\\a" "\\b" "\\t" "\\n" "\\r")]
  [symbol-element (:or (:& (:~ #\|) (:~ #\\))
                       inline-hex-escape
                       mnemonic-escape
                       "\\|")] ; this line ambiguous in std p.62
  [sign-subsequent (:or initial explicit-sign #\@)]
  [dot-subsequent (:or sign-subsequent #\.)]
  [peculiar-identifier (:&
                        (:- "+i." "-i" infnan) ; TODO define infnan
                        (:or explicit-sign
                            (:: explicit-sign sign-subsequent (:* subsequent))
                            (:: explicit-sign #\. dot-subsequent (:* subsequent))
                            (:: #\. dot-subsequent (:* subsequent))))]
  [identifier (:or (:: initial (:* subsequent))
                   (:: #\| (:* symbol-element) #\|)
                   peculiar-identifier)]
)
  
(define R7RS-lexer
  (lexer
   [whitespace
    (R7RS-lexer input-port)]
   [#\;
    (line-comment-lexer input-port)]
   ["#|"
    (nested-comment-lexer input-port)]
   ["#;"
    (sexp-comment-lexer input-port)]
   [directive
    (directive-lexer input-port)]))

; choice between conditional lexer with stored unget
; vs separate lexers (chosen here)
(define line-comment-lexer 
  (lexer
   [line-ending
    (R7RS-lexer input-port)]
   [any-char
    (line-comment-lexer input-port)]))
(define nested-comment-lexer 
  (lexer
   ["|#"
    (R7RS-lexer input-port)]
   [any-char
    (nested-comment-lexer input-port)]))
(define sexp-comment-lexer ; TODO not implemented 
  (lexer
   [line-ending
    (R7RS-lexer input-port)]
   [any-char
    (sexp-comment-lexer input-port)]))

(define directive-lexer
  (lexer
   [delimiter
    (R7RS-lexer input-port)] ; TODO not implemented
   [(eof) 'EOF]
   [any-char
    (error "A directive must be followed by either a delimiter or EOF.")]))