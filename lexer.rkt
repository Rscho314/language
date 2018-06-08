#lang racket

;R7RS lexer
; example lexer: http://planet.racket-lang.org/package-source/soegaard/infix.plt/1/0/parser.ss
; r7rs standard: https://bitbucket.org/cowan/r7rs/raw/4c27517de187142ad2cf4bcd8cb9199ae1e48c09/rnrs/r7rs.pdf

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide R7RS-lexer
         value-tokens
         delimiter-tokens)

; this works only when reading from a file
; or when the port has been created with open-input-string
; does not work when reading from stdin
(define (unget port)
  (file-position port (- (file-position port) 1)))

; this global counts parentheses
; used in lexing of #; comments
(define parens-count 0)

(define-tokens value-tokens
  (IDENTIFIER
   BOOLEAN
   NUMBER
   CHARACTER
   STRING))

(define-empty-tokens delimiter-tokens
  (OP CP  ;( )
   OV     ;#(
   OB     ;u8(
   Q      ;'
   QQ     ;`
   UQ     ;,
   UQS    ;,@
   D      ;.
   EOF))

; this respects the standard scrupulously
; but it overwrites many imported constructs such as "whitespace"
; so be careful to check everything is defined explicitely as per std
; it also does not include the unicode standard p.61
(define-lex-abbrevs
  ; std 7.1.1 Lexical structure
  [token (:or identifier boolean number character string
              #\( #\) "#(" "#u8(" #\' #\` #\, ",@" #\.)]
  [delimiter (:or whitespace #\| #\( #\) #\" #\; )]
  [intraline-whitespace (:or " " "\t")]
  [whitespace (:or intraline-whitespace line-ending)]
  [vertical-line #\|]
  [line-ending (:or "\n" "\r\n" "\r")]
  [comment (:or (:: #\; (:* (:- any-char line-ending)))
                nested-comment
                ;(:: "#;" intertoken-space datum)
                )]
  [nested-comment (:: "#|" comment-text "|#")]
  ; [nested-comment (:: "#|" comment-text (:* comment-cont) "|#")] cycle, not possible
  [comment-text (:* (:- any-char "#|" "|#"))]
  ;[comment-cont (:: (:: "#|" comment-text (:* comment-cont) "|#") comment-text)] cycle
  [directive (:or "#!fold-case" "#!no-fold-case")]
  ;[atmosphere (:or whitespace comment directive)]
  ;[intertoken-space (:* atmosphere)]
  [identifier (:or (:: initial (:* subsequent))
                   (:: vertical-line (:* symbol-element) vertical-line)
                   peculiar-identifier)]
  [initial (:or letter special-initial)]
  [letter (:or (:/ #\a #\z) (:/ #\A #\Z) )]
  [special-initial (:or #\! #\$ #\% #\& #\* #\/ #\: #\< #\=
                        #\> #\? #\@ #\^ #\_ #\~)]
  [subsequent (:or initial digit special-subsequent)]
  [digit (:/ #\0 #\9)]
  [hex-digit  (:or digit (:/ #\a #\f))]
  [explicit-sign (:or #\+ #\-)]
  [special-subsequent (:or explicit-sign #\. #\@)]
  [inline-hex-escape (:: "\\x" hex-scalar-value #\;)]
  [hex-scalar-value (:+ hex-digit)]
  [mnemonic-escape (:or "\\a" "\\b" "\\t" "\\n" "\\r")]
  [peculiar-identifier (:&
                        (:- "+i." "-i" infnan)
                        (:or explicit-sign
                             (:: explicit-sign sign-subsequent (:* subsequent))
                             (:: explicit-sign #\. dot-subsequent (:* subsequent))
                             (:: #\. dot-subsequent (:* subsequent))))]
  [dot-subsequent (:or sign-subsequent #\.)]
  [sign-subsequent (:or initial explicit-sign #\@)]
  [symbol-element (:or (:& (:~ #\|) (:~ #\\))
                       inline-hex-escape
                       mnemonic-escape
                       "\\|")] ; this line ambiguous in std p.62
  [boolean (:or "#t" "#f" "#true" "#false")]
  [character (:: "#\\" (:or any-char character-name (:: #\x hex-scalar-value)))]
  [character-name (:or "alarm" "backspace" "delete" "escape" "newline" "null"
                       "return" "space" "tab")]
  [string (:: #\" (:* string-element) #\")]
  [string-element (:or (:- #\" #\\)
                       mnemonic-escape
                       #\"
                       "\\\\"
                       "\\|"
                       (:: #\\ (:* intraline-whitespace) line-ending (:* intraline-whitespace))
                       inline-hex-escape)]
  [bytevector (:: "#u8(" (:* byte) #\))]
  [byte (:or
         (:/ #\0 #\9)
         (:: (:/ #\1 #\9) (:/ #\0 #\9))
         (:: #\1 (:/ #\0 #\9) (:/ #\0 #\9))
         (:: #\2 (:/ #\0 #\4) (:/ #\0 #\9))
         (:: #\2 #\5 (:/ #\0 #\5)))]
  
  [number (:or num2 num8 num10 num16)]
  [num2 (:: prefix2 complex2)]
  [complex2 (:or real2
                 (:: real2 #\@ real2)
                 (:: real2 #\+ ureal2 (:or #\i #\I))
                 (:: real2 #\- ureal2 (:or #\i #\I))
                 (:: real2 #\+ (:or #\i #\I))
                 (:: real2 #\- (:or #\i #\I))
                 (:: real2 infnan (:or #\i #\I))
                 (:: #\+ ureal2 (:or #\i #\I))
                 (:: #\- ureal2 (:or #\i #\I))
                 (:: infnan (:or #\i #\I))
                 (:: #\+ (:or #\i #\I))
                 (:: #\- (:or #\i #\I)))]
  [real2 (:or (:: sign ureal2) infnan)]
  [ureal2 (:or uinteger2
               (:: uinteger2 #\/ uinteger2))]
  [uinteger2 (:+ digit2)]
  [prefix2 (:or (:: radix2 exactness)
                (:: exactness radix2))]

  [num8 (:: prefix8 complex8)]
  [complex8 (:or real8
                 (:: real8 #\@ real8)
                 (:: real8 #\+ ureal8 (:or #\i #\I))
                 (:: real8 #\- ureal8 (:or #\i #\I))
                 (:: real8 #\+ (:or #\i #\I))
                 (:: real8 #\- (:or #\i #\I))
                 (:: real8 infnan (:or #\i #\I))
                 (:: #\+ ureal8 (:or #\i #\I))
                 (:: #\- ureal8 (:or #\i #\I))
                 (:: infnan (:or #\i #\I))
                 (:: #\+ (:or #\i #\I))
                 (:: #\- (:or #\i #\I)))]
  [real8 (:or (:: sign ureal8) infnan)]
  [ureal8 (:or uinteger8
               (:: uinteger8 #\/ uinteger8))]
  [uinteger8 (:+ digit8)]
  [prefix8 (:or (:: radix8 exactness)
                (:: exactness radix8))]

  [num10 (:: prefix10 complex10)]
  [complex10 (:or real10
                 (:: real10 #\@ real10)
                 (:: real10 #\+ ureal10 (:or #\i #\I))
                 (:: real10 #\- ureal10 (:or #\i #\I))
                 (:: real10 #\+ (:or #\i #\I))
                 (:: real10 #\- (:or #\i #\I))
                 (:: real10 infnan (:or #\i #\I))
                 (:: #\+ ureal10 (:or #\i #\I))
                 (:: #\- ureal10 (:or #\i #\I))
                 (:: infnan (:or #\i #\I))
                 (:: #\+ (:or #\i #\I))
                 (:: #\- (:or #\i #\I)))]
  [real10 (:or (:: sign ureal10) infnan)]
  [ureal10 (:or uinteger10
               (:: uinteger10 #\/ uinteger10)
               decimal10)]
  [decimal10 (:or (:: uinteger10 suffix)
                  (:: #\. (:+ digit10) suffix)
                  (:: (:+ digit10) #\. (:* digit10) suffix))]
  [uinteger10 (:+ digit10)]
  [prefix10 (:or (:: radix10 exactness)
                (:: exactness radix10))]

  [num16 (:: prefix16 complex16)]
  [complex16 (:or real16
                 (:: real16 #\@ real16)
                 (:: real16 #\+ ureal16 (:or #\i #\I))
                 (:: real16 #\- ureal16 (:or #\i #\I))
                 (:: real16 #\+ (:or #\i #\I))
                 (:: real16 #\- (:or #\i #\I))
                 (:: real16 infnan (:or #\i #\I))
                 (:: #\+ ureal16 (:or #\i #\I))
                 (:: #\- ureal16 (:or #\i #\I))
                 (:: infnan (:or #\i #\I))
                 (:: #\+ (:or #\i #\I))
                 (:: #\- (:or #\i #\I)))]
  [real16 (:or (:: sign ureal16) infnan)]
  [ureal16 (:or uinteger16
               (:: uinteger16 #\/ uinteger16))]
  [uinteger16 (:+ digit16)]
  [prefix16 (:or (:: radix16 exactness)
                (:: exactness radix16))]
  [infnan (:or "+inf.0" "-inf.0" "+nan.0" "-nan.0")]
  [suffix (:or empty (:: exponent-marker sign (:+ digit10)))]
  [exponent-marker (:or #\e #\E)]
  [sign (:or empty #\+ #\-)]
  [exactness (:or empty "#i" "#e" "#I" "#E")]
  [radix2 (:or "#b" "#B")]
  [radix8 (:or "#o" "#O")]
  [radix10 (:or empty "#d" "#D")]
  [radix16 (:or "#x" "#X")]
  [digit2 (:or #\0 #\1)]
  [digit8 (:/ #\0 #\7)]
  [digit10 digit]
  [digit16 (:or digit10 (:/ #\a #\f) (:/ #\A #\F))]

  ; std 7.1.2 External representations
  [datum (:or simple-datum compound-datum (:: label #\= datum) (:: label #\#))]
  [simple-datum (:or boolean number character string symbol bytevector)]
  [symbol identifier]
  [compound-datum (:or list vector abbreviation)]
  [list (:or (:: #\( (:* datum) #\)) (:: #\( (:+ datum) #\. datum #\)))]
  [abbreviation (:: abbrev-prefix datum)]
  [abbrev-prefix (:or #\' #\` #\, ",@")]
  [vector (:: "#(" (:* datum) #\))]
  [label (:: #\# uinteger10)]

  ; std 7.1.3 Expressions
  [expression (:or identifier
                   literal
                   procedure-call
                   lambda-expression
                   conditional
                   assignment
                   derived-expression
                   macro-use
                   macro-block
                   includer)]
  [literal (:or quotation self-evaluating)]
  [self-evaluating (:or boolean number vector character string bytevector)]
  [quotation (:or (:: #\' datum) (:: #\( "quote" datum) #\))]
  [procedure-call (:: #\( operator (:* operand) #\))]
  [operator expression]
  [operand expression]
  [lambda-expression (:: #\( "lambda" formals body #\))]
  [formals (:or (:: #\( (:* identifier) #\))
                identifier
                (:: #\( (:+ identifier) #\. identifier #\)))]
  [body (:: (:* definition) sequence)]
  [sequence (:: (:* command) expression)]
  [command expression]
  [conditional (:: #\( "if" test consequent atenate #\))]
  [test expression]
  [consequent expression]
  [alternate (:or expression empty)]
  [assignment (:: #\( "set!" identifier expression #\))]
  [derived-expression (:or (:: #\( "cond" (:+ cond-clause) #\))
                           (:: #\( "cond" (:* cond-clause) #\( "else" sequence #\) #\))
                           (:: #\( "case" expression (:+ case-clause) #\))
                           (:: #\( "case" expression (:* case-clause) #\( "else" sequence #\) #\))
                           (:: #\( "case" expression (:* case-clause) #\( "else" "=>" recipient #\) #\))
                           (:: #\( "and" (:* test) #\))
                           (:: #\( "or" (:* test) #\))
                           (:: #\( "when" test sequence #\))
                           (:: #\( "unless" test sequence #\))
                           (:: #\( "let" #\( (:* binding-spec) #\) body #\))
                           (:: #\( "let" identifier #\( (:* binding-spec) #\) body #\))
                           (:: #\( "let*" #\( (:* binding-spec) #\) body #\))
                           (:: #\( "letrec" #\( (:* binding-spec) #\) body #\))
                           (:: #\( "letrec*" #\( (:* binding-spec) #\) body #\))
                           (:: #\( "let-values" #\( (:* mv-binding-spec) #\) body #\))
                           (:: #\( "let*-values" #\( (:* mv-binding-spec) #\) body #\))
                           (:: #\( "begin" sequence #\))
                           (:: #\( "do" #\( (:* iteration-spec) #\) #\( test do-result #\) (:* command) #\))
                           (:: #\( "delay" expression #\))
                           (:: #\( "delay-force" expression #\))
                           (:: #\( "parametrize" #\( (:* (:: #\( expression expression #\))) #\) body #\))
                           (:: #\( "guard" #\( identifier (:* cond-clause) #\) body #\))
                           quasiquotation
                           (:: #\( "case-lambda" (:* case-lambda-clause) #\)))]
  [cond-clause (:or (:: #\( test sequence #\))
                    (:: #\( test #\))
                    (:: #\( test "=>" recipient #\)))]
  [recipient expression]
  [case-clause (:or (:: #\( #\( (:* datum) #\) sequence #\))
                    (:: #\( #\( (:* datum) #\) "=>" recipient #\)))]
  [binding-spec (:: #\( identifier expression #\))]
  [mv-binding-spec (:: #\( formals expression #\))]
  [iteration-spec (:or (:: #\( identifier init step #\))
                       (:: #\( identifier init #\)))]
  [case-lambda-clause (:: #\( formals body #\))]
  [init expression]
  [step expression]
  [do-result (:: sequence empty)]
  [macro-use (:: #\( keyword (:* datum) #\))]
  [keyword identifier]
  [macro-block (:or (:: #\( "let-syntax" #\( (:* syntax-spec) #\) body #\))
                    (:: #\( "letrec-syntax" #\( (:* syntax-spec) #\) body #\)))]
  [syntax-spec (:: #\( keyword transformer-spec #\))]
  [includer (:or (:: #\( "include" (:+ string) #\))
                 (:: #\( "include-ci" (:+ string) #\)))]

  ; std 7.1.4 quasiquotations
  [D (:+ (:/ #\1 #\9))]
  [quasiquotation (:: quasiquotation1)]
  [qq-template-0 expression]
  ; the rest of this section can't be abbreviated
  ; and has to be lexed directly, because D can take whatever value.

  ; std 7.1.5 transformers
  [transformer-spec (:or (:: #\( "syntax-rules" #\( (:* identifier) #\) (:* syntax-rule) #\))
                         (:: #\( "syntax-rules" identifier #\( (:* identifier) #\) (:* syntax-rule) #\)))]
  [syntax-rule (:: #\( pattern template #\))]
  [pattern (:or pattern-identifier
                underscore
                (:: #\( (:* pattern) #\))
                (:: #\( (:+ pattern) #\. pattern #\))
                (:: #\( (:* pattern) pattern ellipsis (:* pattern) #\))
                (:: #\( (:* pattern) pattern ellipsis (:* pattern) #\. pattern #\))
                (:: "#(" (:* pattern) #\))
                (:: "#(" (:* pattern) pattern ellipsis (:* pattern) #\))
                pattern-datum)]
  [pattern-datum (:or string character boolean number bytevector)]
  [template (:or pattern-identifier
                 (:: #\( (:* template-element) #\) )
                 (:: #\( (:+ template-element) #\. template #\) )
                 (:: "#(" (:*template-element) #\))
                 template-datum)]
  [template-element (:or template (:: template ellipsis))]
  [template-datum pattern-datum]
  [pattern-identifier (:& (:~ "...") identifier)]
  [ellipsis (:or "..." identifier)]
  [underscore (:: identifier #\_)]

  ; std 7.1.6 programs and definitions
  [program (:: (:+ import-declaration) (:+ command-or-definition))]
  [command-or-definition (:or command definition
                              (:: #\( "begin" (:+ command-or-definition) #\)))]
  [definition (:or (:: #\( "define" identifier expression #\))
                   (:: #\( "define" #\( identifier def-formals #\) body #\) )
                   syntax-definition
                   (:: #\( "define-values" formals body #\) )
                   (:: #\( "define-record-type" identifier
                       constructor identifier (:* field-spec) #\) )
                   (:: #\( "begin" (:* definition) #\) ))]
  [def-formals (:or (:* identifier)
                    (:: (:* identifier) #\. identifier))]
  [constructor (:: #\( identifier (:* field-name) #\))]
  [field-spec (:or (:: #\( field-name accessor #\) )
                   (:: #\( field-name accessor mutator #\) ))]
  [field-name identifier]
  [accessor identifier]
  [mutator identifier]
  [syntax-definition (:: #\( "define-syntax" keyword transformer-spec #\) )]

  ; std 7.1.7 libraries
  [library (:: #\( "define-library" library-name (:* library-declaration) #\) )]
  [library-name (:: #\( (:+ library-name-part) #\) )]
  [library-name-part (:or identifier uinteger10)]
  [library-declaration (:or (:: #\( "export" (:* export-spec) #\) )
                            import-declaration
                            (:: #\( "begin" (:* command-or-definition) #\) )
                            includer
                            (:: #\( "include-library-declarations" (:+ string) #\) )
                            (:: #\( "cond-expand" (:+ cond-expand-clause) #\) )
                            (:: #\( "cond-expand" (:+ cond-expand-clause) #\)
                                #\( "else" (:* library-declaration) #\) #\) ))]
  [import-declaration (:: #\( "import" (:+ import-set) #\))]
  [export-spec (:or identifier (:: #\( "rename" identifier identifier #\)))]
  [import-set (:or library-name
                   (:: #\( "only" import-set (:+ identifier) #\))
                   (:: #\( "except" import-set (:+ identifier) #\))
                   (:: #\( "prefix" import-set identifier #\))
                   (:: #\( "rename" import-set (:+ #\( identifier identifier #\) ) #\) ))]
  [cond-expand-clause (:: #\( feature-requirement (:* library-declaration) #\) )]
  [feature-requirement (:or identifier library-name
                            (:: #\( "and" (:* feature-requirement) #\) )
                            (:: #\( "or" (:* feature-requirement) #\) )
                            (:: #\( "not" (:* feature-requirement) #\) ))]
  [empty ""]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEXERS

(define R7RS-lexer
  (lexer
   [(eof) (token-EOF)]
   ["#;" (sexp-comment-lexer input-port)]
   [comment (R7RS-lexer input-port)]
   [identifier (token-IDENTIFIER lexeme)]
   [boolean (token-BOOLEAN lexeme)]
   [number (token-NUMBER lexeme)]
   [character (token-CHARACTER lexeme)]
   [string (token-STRING lexeme)]
   [#\( (token-OP)]
   [#\) (token-CP)]
   ["#(" (token-OV)]
   ["#u8(" (token-OB)]
   [#\' (token-Q)]
   [#\` (token-QQ)]
   [#\, (token-UQ)]
   [",@" (token-UQS)]
   [#\. (token-D)]
   [whitespace (R7RS-lexer input-port)]
   [directive (directive-lexer input-port)]))

(define sexp-comment-lexer
  (lexer
   [#\(
    (begin
      (set! parens-count (add1 parens-count))
      (sexp-comment-lexer input-port))]
   [#\)
    (begin
      (set! parens-count (sub1 parens-count))
      (if (= parens-count 0)
          (R7RS-lexer input-port)
          (sexp-comment-lexer input-port)))]
   [(:~ #\( #\)) (sexp-comment-lexer input-port)]))

(define directive-lexer
  (lexer
   [(eof) '()]
   [whitespace (R7RS-lexer input-port)]
   [any-char (error "Directives must be followed by a delimiter or EOF: " lexeme)]))