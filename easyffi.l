; easyffi.l -*- Scheme -*-
;
; Copyright (c) 2000-2004, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Unter den Gleichen 1
; 37130 Gleichen
; Germany


letter [a-zA-Z_]
digit  [0-9.]
space  [ ]

%%

\\[ ]                              (yycontinue)
\\?\13?\n	                   (if pp-mode
				       (begin 
					 (set! pp-mode #f) 'pp-end)
				       (yycontinue) )
{space}+                           (yycontinue)
\9+                                (yycontinue)
\13+                               (yycontinue)
\12+                               (yycontinue)
"//"                               (let loop ()
				     (let ([c (yygetc)])
				       (if (or (eq? 'eof c) (char=? #\newline c))
					   (begin
					     (if pp-mode
						 (begin
						   (set! pp-mode #f)
						   'pp-end)
						 (yycontinue) ) )
					   (loop) ) ) )
"/*"                               (let loop ([c (yygetc)])
				     (cond [(eq? 'eof c) (parsing-error "unexpected end of comment")]
					   [(char=? #\newline c) (loop (yygetc))]
					   [(char=? c #\*)
					    (let ([c2 (yygetc)])
					      (if (eq? #\/ c2)
						  (yycontinue)
						  (loop c2) ) ) ]
					   [else (loop (yygetc))] ) )
"enum"                             'enum
"typedef"                          'typedef
"extern"                           'extern
"static"                           'static
"___fixnum"                         'fixnum
"___number"                         'number
"___symbol"                         'symbol
"___bool"                           'bool
"___pointer"                       'pointer
"___u32"                           'u32
"___s32"                           's32
"___s64"                           's64
"int64_t"                          's64
"__int64"                          's64
"bool"                             'bool
"___callback"                       'callback
"___safe"                           'callback
"___declare"                        'declare
"___scheme_value"                   'scheme-value
"___scheme_pointer"                 'scheme-pointer
"___byte_vector"                    'byte-vector
"C_word"                            'scheme-value
"___abstract"                       'abstract
"___specialize"                     'specialize
"___byte"                           'byte
"___discard"                        'discard
"___in"                            'in
"___out"                           'out
"___inout"                         'inout
"___mutable"                       'mutable
"___length"                        'length
"size_t"                           'size_t
"int"                              'int
"unsigned"                         'unsigned
"signed"                           'signed
"float"                            'float
"double"                           'double
"short"                            'short
"long"                             'long
"char"                             'char
"void"                             'void
"struct"                           'struct
"union"                            'union
"const"                            'const
"class"                            'class
"public"                           'public
"protected"                        'protected
"private"                          'private
"volatile"                         'volatile
"namespace"                        'namespace
"virtual"                          'virtual
"explicit"                         'explicit
"inline"                           'inline
"using"                            'using
"@interface"                       'interface
"@implementation"                  'implementation
"@end"                             'end
"@class"                           'objc-class
"@protocol"                        'protocol
"@public"                          'objc-public
"@protected"                       'objc-protected
"@private"                         'objc-private
"@encode"                          (list 'id "@encode")
"@defs"                            (list 'id "@defs")
"@selector"                        (list 'id "@selector")
"..."                              'dots
^[ \t]*#[ ]*define                        (begin (set! pp-mode #t) 'pp-define)
^[ \t]*#[ ]*include                       (begin (set! pp-mode 'include) 'pp-include)
^[ \t]*#[ ]*import                        (begin (set! pp-mode 'import) 'pp-import)
^[ \t]*#[ ]*ifdef                         (begin (set! pp-mode #t) 'pp-ifdef)
^[ \t]*#[ ]*ifndef                        (begin (set! pp-mode #t) 'pp-ifndef)
^[ \t]*#[ ]*elif                          (begin (set! pp-mode #t) 'pp-elif)
^[ \t]*#[ ]*if                            (begin (set! pp-mode #t) 'pp-if)
^[ \t]*#[ ]*else                          (begin (set! pp-mode #t) 'pp-else)
^[ \t]*#[ ]*pragma                        (begin (set! pp-mode #t) 'pp-pragma)
^[ \t]*#[ ]*endif                         (begin (set! pp-mode #t) 'pp-endif)
^[ \t]*#[ ]*error                         (begin (set! pp-mode #t) 'pp-error)
^[ \t]*#[ ]*undef                         (begin (set! pp-mode #t) 'pp-undef)
#                                  '(op "#")
"if"                               'if
"else"                             'else
@?\"                               (let loop ([cs '()])
				     (let ([c (yygetc)])
				       (cond [(eq? 'eof c)
					      (parsing-error "unexpected end of string constant")]
					     [(char=? c #\\) (loop (cons (yygetc) cs))]
					     [(char=? c #\")
					      (list 'string (reverse-list->string cs)) ]
					     [else (loop (cons c cs))] ) ) )
\'\\{digit}{digit}{digit}\'        (list 'char (string->number (substring yytext 3 5) 8))
\'\\0\'                            '(char #\nul)
\'\\n\'                            '(char #\newline)
\'\\t\'                            '(char #\tab)
\'\\r\'                            '(char #\return)
\'\\.\'                            (list 'char (string-ref yytext 2))
\'.\'                              (list 'char (string-ref yytext 1))
({letter}|_)({letter}|_|{digit})*  (list 'id yytext)
0(x|X)({digit}|{letter})+          (list 'num (string->number (substring yytext 2 (string-length yytext)) 16))
0{digit}+                          (list 'num (string->number (substring yytext 1 (string-length yytext)) 8))
-?{digit}+                         (list 'num (string->number yytext))
"<"                                (if (eq? pp-mode 'include)
				       (let loop ([s '()])
					 (let ([c (yygetc)])
					   (cond [(eq? 'eof c) (parsing-error "unexpected end of include file name")]
						 [(char=? #\> c)
						  (set! pp-mode #f)
						  `(i-string ,(reverse-list->string s)) ]
						 [else (loop (cons c s))] ) ) ) 
				       `(op "<") )
"("                                'open-paren
")"                                'close-paren
"["                                'open-bracket
"]"                                'close-bracket
"{"                                'open-curly
"}"                                'close-curly
","                                'comma
";"                                'semicolon
"*"                                'star
"+="|"-="|">>="|"<<="|"*="|"/="|"%="|"%"|"&="|"|="|"^="|"+"|"-"|"/"|">="|"<="|"=="|"<<"|">>"|"&&"|"||"|"&"|"|"|">"|"<"|"^"|"~"|"?"|"::"|":"|"="|"!="|"!"
                                     (list 'op yytext)
<<EOF>>                            (begin (set! pp-mode #f) 'stop)
<<ERROR>>                          (lexer-error (yygetc))
