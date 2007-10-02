;;;; regex-extras.scm - Unit for using the PCRE regex extras package
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
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


(cond-expand
 [chicken-compile-shared]
 [else (declare (unit regex-extras))] )

(declare
  (uses regex extras)
  (usual-integrations)
  (disable-interrupts)
  (generic) ; PCRE options use lotsa bits
  (disable-warning var)
  (export
    regex-version
    regex-chardef-table regex-chardef-set! regex-chardefs-update! regex-chardefs
    regexp-extra-info-set! regexp-extra-info
    regexp-options-set! regexp-options
    regexp-info regexp-info-nametable
    regex-build-config-info)
  (bound-to-procedure
    ;; Imports
    alist-ref
    substring substring-index string-append
    char->integer integer->char
    set-finalizer!
    ##sys#make-tagged-pointer
    ##sys#slot ##sys#setslot
    ##sys#structure?
    ##sys#error ##sys#signal-hook) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings) ) ] )

(cond-expand
 [unsafe
  (eval-when (compile)
    (define-macro (##sys#check-chardef-table . _) '(##core#undefined))
    (define-macro (##sys#check-integer . _) '(##core#undefined))
    (define-macro (##sys#check-blob . _) '(##core#undefined))
    (define-macro (##sys#check-vector . _) '(##core#undefined))
    (define-macro (##sys#check-structure . _) '(##core#undefined))
    (define-macro (##sys#check-range . _) '(##core#undefined))
    (define-macro (##sys#check-pair . _) '(##core#undefined))
    (define-macro (##sys#check-list . _) '(##core#undefined))
    (define-macro (##sys#check-symbol . _) '(##core#undefined))
    (define-macro (##sys#check-string . _) '(##core#undefined))
    (define-macro (##sys#check-char . _) '(##core#undefined))
    (define-macro (##sys#check-exact . _) '(##core#undefined))
    (define-macro (##sys#check-port . _) '(##core#undefined))
    (define-macro (##sys#check-number . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else
  (declare
    (bound-to-procedure
      ;; Imports
      ##sys#check-chardef-table
      ##sys#check-string ##sys#check-list ##sys#check-exact ##sys#check-vector
      ##sys#check-structure ##sys#check-symbol ##sys#check-blob ##sys#check-integer)
    (emit-exports "regex-extras.exports")) ] )


;;;

;FIXME should have a common handler in "runtime.c"
(foreign-declare #<<EOF
static void
out_of_memory_failure(const char *modnam, const char *prcnam, const char *typnam)
{
  fprintf(stderr, "%s@%s: out of memory - cannot allocate %s\\n", modnam, prcnam, typnam);
  exit(EXIT_FAILURE);
}
EOF
)

(foreign-declare "#include \"pcre/pcre.h\"")


;;; From unit lolevel:

(define-inline (%tag-pointer ptr tag)
  (let ([tp (##sys#make-tagged-pointer tag)])
    (##core#inline "C_copy_pointer" ptr tp)
    tp) )


;;; Which version of PCRE:

(define regex-version
  (let ([substring substring]
        [substring-index substring-index])
    (lambda ()
      (let ([str ((foreign-lambda nonnull-c-string "pcre_version"))])
        (substring str 0 (substring-index " " str)) ) ) ) )


;;; PCRE Types:

(define-foreign-type pcre (c-pointer "pcre"))
(define-foreign-type nonnull-pcre (nonnull-c-pointer "pcre"))

(define-foreign-type pcre_extra (c-pointer "pcre_extra"))
(define-foreign-type nonnull-pcre_extra (nonnull-c-pointer "pcre_extra"))

;FIXME the use of 'define-foreign-enum' causes unused global variable warning!

(define-foreign-enum (pcre-option unsigned-integer)
  (caseless             PCRE_CASELESS)
  (multiline            PCRE_MULTILINE)
  (dotall               PCRE_DOTALL)
  (extended             PCRE_EXTENDED)
  (anchored             PCRE_ANCHORED)
  (dollar-endonly       PCRE_DOLLAR_ENDONLY)
  (extra                PCRE_EXTRA)
  (notbol               PCRE_NOTBOL)
  (noteol               PCRE_NOTEOL)
  (ungreedy             PCRE_UNGREEDY)
  (notempty             PCRE_NOTEMPTY)
  (utf8                 PCRE_UTF8)
  (no-auto-capture      PCRE_NO_AUTO_CAPTURE)
  (no-utf8-check        PCRE_NO_UTF8_CHECK)
  (auto-callout         PCRE_AUTO_CALLOUT)
  (partial              PCRE_PARTIAL)
  (dfa-shortest         PCRE_DFA_SHORTEST)
  (dfa-restart          PCRE_DFA_RESTART)
  (firstline            PCRE_FIRSTLINE)
  (dupnames             PCRE_DUPNAMES)
  (newline-cr           PCRE_NEWLINE_CR)
  (newline-lf           PCRE_NEWLINE_LF)
  (newline-crlf         PCRE_NEWLINE_CRLF)
  (newline-any          PCRE_NEWLINE_ANY)
  (newline-anycrlf      PCRE_NEWLINE_ANYCRLF)
  (bsr-anycrlf          PCRE_BSR_ANYCRLF)
  (bsr-unicode          PCRE_BSR_UNICODE) )

(define pcre-option-symbols '(
  caseless
  multiline
  dotall
  extended
  anchored
  dollar-endonly
  extra
  notbol
  noteol
  ungreedy
  notempty
  utf8
  no-auto-capture
  no-utf8-check
  auto-callout
  partial
  dfa-shortest
  dfa-restart
  firstline
  dupnames
  newline-cr
  newline-lf
  newline-crlf
  newline-any
  newline-anycrlf
  bsr-anycrlf
  bsr-unicode) )

(define-foreign-enum (pcre-info-field unsigned-int)
  (options          PCRE_INFO_OPTIONS)
  (size             PCRE_INFO_SIZE)
  (capturecount     PCRE_INFO_CAPTURECOUNT)
  (backrefmax       PCRE_INFO_BACKREFMAX)
  (firstbyte        PCRE_INFO_FIRSTBYTE)
  (firstchar        PCRE_INFO_FIRSTCHAR)
  (firsttable       PCRE_INFO_FIRSTTABLE)
  (lastliteral      PCRE_INFO_LASTLITERAL)
  (nameentrysize    PCRE_INFO_NAMEENTRYSIZE)
  (namecount        PCRE_INFO_NAMECOUNT)
  (nametable        PCRE_INFO_NAMETABLE)
  (studysize        PCRE_INFO_STUDYSIZE)
  (default-tables   PCRE_INFO_DEFAULT_TABLES)
  (okpartial        PCRE_INFO_OKPARTIAL)
  (jchanged         PCRE_INFO_JCHANGED)
  (hascrorlf        PCRE_INFO_HASCRORLF) )

(define pcre-info-field-types '(
  (options          . long-integer)
  (size             . long-integer)
  (capturecount     . integer)
  (backrefmax       . integer)
  (firstbyte        . integer)
  (firstchar        . integer)
  (firsttable       . pointer)
  (lastliteral      . integer)
  (nameentrysize    . integer)
  (namecount        . integer)
  (nametable        . pointer)
  (studysize        . integer)
  (default-tables   . pointer)
  (okpartial        . boolean)
  (jchanged         . boolean)
  (hascrorlf        . boolean) ) )

(define pcre-info-field-symbols (map car pcre-info-field-types))

(define-foreign-enum (pcre-config-field unsigned-int)
  (utf8                     PCRE_CONFIG_UTF8)
  (newline                  PCRE_CONFIG_NEWLINE)
  (link-size                PCRE_CONFIG_LINK_SIZE)
  (posix-malloc-threshold   PCRE_CONFIG_POSIX_MALLOC_THRESHOLD)
  (match-limit              PCRE_CONFIG_MATCH_LIMIT)
  (stackrecurse             PCRE_CONFIG_STACKRECURSE)
  (unicode-properties       PCRE_CONFIG_UNICODE_PROPERTIES)
  (match-limit-recursion    PCRE_CONFIG_MATCH_LIMIT_RECURSION)
  (bsr                      PCRE_CONFIG_BSR) )

(define pcre-config-field-types '(
  (utf8                     . boolean)
  (newline                  . integer)
  (link-size                . integer)
  (posix-malloc-threshold   . integer)
  (match-limit              . integer)
  (stackrecurse             . boolean)
  (unicode-properties       . boolean)
  (match-limit-recursion    . integer)
  (bsr                      . boolean) ) )

(define pcre-config-field-symbols (map car pcre-config-field-types))

#; ; UNUSED
(define-foreign-enum (pcre-extra-option unsigned-int)
  (study-data     PCRE_EXTRA_STUDY_DATA)
  (match-limit    PCRE_EXTRA_MATCH_LIMIT)
  (callout-data   PCRE_EXTRA_CALLOUT_DATA)
  (tables         PCRE_EXTRA_TABLES)
  (match-limit    PCRE_EXTRA_MATCH_LIMIT_RECURSION) )


;;; The regexp structure primitives:

(define re-finalizer
  (foreign-lambda void "pcre_free" c-pointer) )

(define-inline (%regexp? x)
  (##sys#structure? x 'regexp) )

(define-inline (%regexp-code rx)
  (##sys#slot rx 1) )

(define-inline (%regexp-extra rx)
  (##sys#slot rx 2) )

(define-inline (%regexp-options rx)
  (##sys#slot rx 3) )

(define-inline (%regexp-extra-set! rx extra)
  (when extra (set-finalizer! extra re-finalizer))
  (##sys#setslot rx 2 extra) )

(define-inline (%regexp-options-set! rx options)
  (##sys#setslot rx 3 options) )


;;; Character Definition Tables:

(foreign-declare "#include \"pcre/pcre_internal.h\"")

;;

(define (re-chardef-table tables)
  (%tag-pointer tables 'chardef-table) )

;; Make character definition tables

;FIXME (const (nonnull-c-pointer unsigned-char)) doesn't work
(define re-maketables
  (foreign-lambda* (nonnull-c-pointer unsigned-char) ()
    "const unsigned char *tables = pcre_maketables();"
    "if (!tables) out_of_memory_failure(\"regex-extras\", \"re-maketables\", \"tables\");"
    "return(tables);"))

;; Get a character definitions tables structure for the current locale.

(define (regex-chardef-table)
  (let ([tables (re-chardef-table (re-maketables))])
    (set-finalizer! tables re-finalizer)
    tables ) )

;; Character Class

(define-foreign-enum (pcre-cbit unsigned-int)
  (space cbit_space)
  (xdigit cbit_xdigit)
  (digit cbit_digit)
  (upper cbit_upper)
  (lower cbit_lower)
  (word cbit_word)
  (graph cbit_graph)
  (print cbit_print)
  (punct cbit_punct)
  (cntrl cbit_cntrl) )

(define cbit-symbols '(space xdigit digit upper lower word graph print punct cntrl))

;; Character Type

(define-foreign-enum (pcre-ctype unsigned-int)
  (space ctype_space)
  (letter ctype_letter)
  (digit ctype_digit)
  (xdigit ctype_xdigit)
  (word ctype_word)
  (meta ctype_meta) )

(define ctype-symbols '(space xdigit digit letter word meta))

;; Accessors

(define chardef-lower-case-set!
  (foreign-lambda* void (((c-pointer unsigned-char) tables) (int idx) (unsigned-char lower))
    "tables[lcc_offset + idx] = lower;"))

(define chardef-lower-case
  (foreign-lambda* unsigned-int (((const (c-pointer unsigned-char)) tables) (int idx))
    "return(tables[lcc_offset + idx]);"))

(define chardef-flipped-case-set!
  (foreign-lambda* void (((c-pointer unsigned-char) tables) (int idx) (unsigned-char flipped))
    "tables[fcc_offset + idx] = flipped;"))

(define chardef-flipped-case
  (foreign-lambda* unsigned-int (((const (c-pointer unsigned-char)) tables) (int idx))
    "return(tables[fcc_offset + idx]);"))

(define chardef-class-clear!
  (foreign-lambda* void (((c-pointer unsigned-char) tables) (int idx) (unsigned-int class))
    "tables[cbits_offset + class + idx/8] &= ~(1 << (idx & 7));"))

(define chardef-class-set!
  (foreign-lambda* void (((c-pointer unsigned-char) tables) (int idx) (unsigned-int class))
    "tables[cbits_offset + class + idx/8] |= 1 << (idx & 7);"))

(define chardef-class
  (foreign-lambda* unsigned-int (((const (c-pointer unsigned-char)) tables) (int idx) (unsigned-int class))
    "return(tables[cbits_offset + class + idx/8] & (1 << (idx & 7)));"))

(define (chardef-classes-clear! tables idx)
  (for-each
    (lambda (sym)
      (chardef-class-clear! tables idx (pcre-cbit->number sym)))
    cbit-symbols) )

(define chardef-type-clear!
  (foreign-lambda* void (((c-pointer unsigned-char) tables) (int idx))
    "tables[ctypes_offset + idx] = 0;"))

(define chardef-type-set!
  (foreign-lambda* void (((c-pointer unsigned-char) tables) (int idx) (unsigned-int type))
    "tables[ctypes_offset + idx] += type;"))

(define chardef-type
  (foreign-lambda* unsigned-int (((const (c-pointer unsigned-char)) tables) (int idx) (unsigned-int type))
    "return(tables[ctypes_offset + idx] & type);"))

;; Update a character definition.
;;
;; 'tables' is a character definition tables structure
;; 'chardef' is a length 4 vector where
;; element 0 is the lower-case character or #f
;; element 1 is the flipped-case character or #f
;; element 2 is a list of character class names or #f
;; element 3 is a list of character type names or #f

(define (regex-chardef-set! tables idx-or-char chardef)
  ; Check proper character definition tables structure
  (##sys#check-chardef-table tables 'regex-chardef-set!)
  ; Need character table index
  (let ([idx (if (char? idx-or-char) (char->integer idx-or-char) idx-or-char)])
    ; Check proper index
    (##sys#check-exact idx 'regex-chardef-set!)
    (unless (and (fx<= 0 idx) (fx<= idx 255))
      (##sys#error 'regex-chardef-set! "invalid character index - must be in [0 255]" idx) )
    ; Check proper character definition structure
    (##sys#check-vector chardef 'regex-chardef-set!)
    (unless (fx= 4 (vector-length chardef))
      (##sys#error 'regex-chardef-set! "invalid chardef length - must be 4" chardef) )
    ; Change lower case character?
    (and-let* ([lower (vector-ref chardef 0)])
      (chardef-lower-case-set! tables idx lower) )
    ; Change flipped case character?
    (and-let* ([flipped (vector-ref chardef 1)])
       (chardef-flipped-case-set! tables idx flipped) )
    ;
    (let ([set-symbols
            (lambda (syms clear sym->num set)
              (##sys#check-list syms 'regex-chardef-set!)
              (clear tables idx)
              (for-each
                (lambda (sym)
                  (set tables idx (sym->num sym)))
                syms) )])
      ; Change character class?
      (and-let* ([cbits (vector-ref chardef 2)])
        (set-symbols cbits chardef-classes-clear! pcre-cbit->number chardef-class-set!) )
      ; Change character type?
      (and-let* ([ctypes (vector-ref chardef 3)])
        (set-symbols ctypes chardef-type-clear! pcre-ctype->number chardef-type-set!) ) ) ) )

;; Update character definition tables.
;;
;; 'tables' is a character definition tables structure
;; 'chardefs' is a length 256 vector of #f or character definitions

(define (regex-chardefs-update! tables chardefs)
  ; Check proper character definition tables structure
  (##sys#check-chardef-table tables 'regex-chardef-update!)
  ; Check proper character definition table structure
  (##sys#check-vector chardefs 'regex-chardefs-update!)
  (unless (fx= 256 (vector-length chardefs))
    (##sys#error 'regex-chardefs-update! "invalid chardefs length - must be 256" chardefs) )
  ; Set every character definition
  (do ([idx 0 (fx+ idx 1)])
      [(fx= 256 idx)]
    ; When a new character definition
    (and-let* ([chardef (vector-ref chardefs idx)])
      (regex-chardef-set! tables idx chardef) ) ) )

;; Get the character definitions.
;; Returns a character definitions vector.

(define (regex-chardefs tables)
  ; Check proper character definition tables structure
  (##sys#check-chardef-table tables 'regex-chardefs)
  ; 256 character definitions
  (let ([chardefs (make-vector 256)])
    ; Get every character definition
    (do ([idx 0 (fx+ idx 1)])
        [(fx= 256 idx) chardefs]
      ; This character definition
      (let ([chardef (make-vector 4)])
        (vector-set! chardefs idx chardef)
        ; Lower-case
        (vector-set! chardef 0 (integer->char (chardef-lower-case tables idx)))
        ; Flipped-case
        (vector-set! chardef 1 (integer->char (chardef-flipped-case tables idx)))
        ;
        (let ([get-symbols
                (lambda (syms get sym->num)
                  (let loop ([syms syms]
                             [lst '()])
                    (if (null? syms)
                        lst
                        (let ([sym (car syms)])
                          (loop (cdr syms)
                                (if (fx= 0 (get tables idx (sym->num sym)))
                                    lst
                                    (cons sym lst) ) ) ) ) ) )])
          ; Character class
          (vector-set! chardef 2 (get-symbols cbit-symbols chardef-class pcre-cbit->number))
          ; Character type
          (vector-set! chardef 3 (get-symbols ctype-symbols chardef-type pcre-ctype->number)) ) ) ) ) )


;;; Optimized compiled regular expression extra structure:

;; Create optimization structure

(define re-extra
  ; Only the flags need to be initialized
  ; When a flag bit is 0 the corresponding field is ignored
  (foreign-lambda* nonnull-pcre_extra ()
    "pcre_extra *extra = (pcre_extra *)(pcre_malloc)(sizeof(pcre_extra));"
    "if (!extra) out_of_memory_failure(\"regex-extras\", \"re-extra\", \"pcre_extra\");"
    "extra->flags = 0;"
    "extra->study_data = NULL;"
    "extra->match_limit = 0;"
    "extra->match_limit_recursion = 0;"
    "extra->callout_data = NULL;"
    "extra->tables = NULL;"
    "return(extra);"))

;; Set specific field

(define (re-extra-field-set! loc extra key val)
  (case key
    [(match-limit)
      (##sys#check-exact match-limit loc)
      ((foreign-lambda* void ((nonnull-pcre_extra extra) (unsigned-long val))
          "extra->match_limit = val;"
          "extra->flags |= PCRE_EXTRA_MATCH_LIMIT;")
        extra val)]
    [(match-limit-recursion)
      (##sys#check-exact match-limit-recursion loc)
      ((foreign-lambda* void ((nonnull-pcre_extra extra) (unsigned-long val))
          "extra->match_limit_recursion = val;"
          "extra->flags |= PCRE_EXTRA_MATCH_LIMIT_RECURSION;")
        extra val)]
    [(callout-data)
      (##sys#check-blob callout-data loc)
      ((foreign-lambda* void ((nonnull-pcre_extra extra) (nonnull-byte-vector val))
          "extra->callout_data = val;"
          "extra->flags |= PCRE_EXTRA_CALLOUT_DATA;")
        extra val)]
    [(tables)
      (##sys#check-chardef-table tables loc)
      ((foreign-lambda* void ((nonnull-pcre_extra extra) ((const (c-pointer unsigned-char)) val))
          "extra->tables = val;"
          "extra->flags |= PCRE_EXTRA_TABLES;")
        extra val)]
    [else
      (warning loc "unrecognized extra field key" key)] ) )

;;

(define (re-extra-field loc extra key)
  (case key
    [(match-limit)
      ((foreign-lambda* unsigned-int ((nonnull-pcre_extra extra) (unsigned-long val))
          "return(extra->match_limit);")
        extra)]
    [(match-limit-recursion)
      ((foreign-lambda* unsigned-int ((nonnull-pcre_extra extra) (unsigned-long val))
          "return(extra->match_limit_recursion);")
        extra)]
    [(callout-data)
      ((foreign-lambda* c-pointer ((nonnull-pcre_extra extra) (nonnull-byte-vector val))
          "return(extra->callout_data);")
        extra)]
    [(tables)
      (re-chardef-table
        ((foreign-lambda* (c-pointer unsigned-char) ((nonnull-pcre_extra extra) ((const (c-pointer unsigned-char)) val))
            "return(extra->tables);")
          extra))]
    [else
      (warning loc "unrecognized extra field key" key)] ) )

;; Set fields

(define (re-extra-fields-set! loc extra . args)
  (let loop ([args args])
    (unless (null? args)
      (let ([key (car args)]
            [val (cadr args)])
        (re-extra-field-set! loc extra key val)
        (loop (cddr args)) ) ) ) )

;; Set user override fields

(define (regexp-extra-info-set! rx . args)
  (##sys#check-structure rx 'regexp 'regexp-extra-info-set!)
  (apply re-extra-fields-set! 'regexp-extra-info-set!
                              (or (%regexp-extra rx)
                                  (let ([extra (re-extra)])
                                    (%regexp-extra-set! rx extra)
                                    extra))
                              args) )

;; Get user override fields

(define (regexp-extra-info rx . fields)
  (##sys#check-structure rx 'regexp 'regexp-extra-info)
  (let ([extra (%regexp-extra rx)])
    (if extra
        (map
          (lambda (sym)
            (##sys#check-symbol sym 'regexp-extra-info)
            (list sym (re-extra-field 'regexp-extra-info extra sym)) )
          fields)
        '() ) ) )


;;; Regexp options:

;; Get a list of option symbols from the integer options

(define (integer-options->symbols options)
  (let ([lst '()])
    (for-each
      (lambda (sym)
        (unless (zero? (bitwise-and options (pcre-option->number sym)))
          (set! lst (cons sym lst)) ) )
      pcre-option-symbols)
    lst ) )

;; Set the 'exec' options

(define (regexp-options-set! rx . options)
  (##sys#check-structure rx 'regexp 'regexp-options-set!)
  (%regexp-options-set! rx (pcre-option->number options)) )

;; Get the 'exec' options

(define (regexp-options obj)
  (integer-options->symbols
    (cond [(%regexp? obj)   (%regexp-options obj)]
          [(integer? obj)   obj]
          [else
            (##sys#signal-hook #:type-error
                               'regexp-options
                     "bad argument type - not an integer or compiled regular expression"
                     obj)] ) ) )


;;; Regexp 'fullinfo':

(define re-info-long-integer
  (foreign-lambda* long (((const nonnull-pcre) code) (pcre_extra extra) (int fieldno))
    "long int val;"
    "pcre_fullinfo(code, extra, fieldno, &val);"
    "return(val);") )

(define re-info-integer
  (foreign-lambda* int (((const nonnull-pcre) code) (pcre_extra extra) (int fieldno))
    "int val;"
    "pcre_fullinfo(code, extra, fieldno, &val);"
    "return(val);") )

(define re-info-boolean
  (foreign-lambda* bool (((const nonnull-pcre) code) (pcre_extra extra) (int fieldno))
    "int val;"
    "pcre_fullinfo(code, extra, fieldno, &val);"
    "return(val);") )

(define re-info-pointer
  (foreign-lambda* c-pointer (((const nonnull-pcre) code) (pcre_extra extra) (int fieldno))
    "void *val;"
    "pcre_fullinfo(code, extra, fieldno, &val);"
    "return(val);") )

;;

(define (regexp-info rx . fields)
  (##sys#check-structure rx 'regexp 'regexp-info)
  (let ([code (%regexp-code rx)]
        [extra (%regexp-extra rx)])
    (map
      (lambda (sym)
        (##sys#check-symbol sym 'regexp-info)
        (list sym
              (and-let* ([typ (alist-ref sym pcre-info-field-types eq?)])
                (let ([fldno (pcre-info-field->number sym)])
                  (case typ
                    [(boolean)
                      (re-info-boolean code extra fldno)]
                    [(long-integer)
                      (re-info-long-integer code extra fldno)]
                    [(integer)
                      (re-info-integer code extra fldno)]
                    [(pointer)
                      (and-let* ([ptr (re-info-pointer code extra fldno)])
                        (case sym
                          [(default-tables firsttable)
                            (re-chardef-table ptr)]
                          [else
                            ptr]))]
                    [else
                      (##sys#error 'regexp-info "unknown type" typ) ] ) ) ) ) )
      (if (null? fields) pcre-info-field-symbols fields)) ) )

;;

(foreign-declare #<<EOF

typedef struct {
  char *nametable;
  int entrysize;
} pcre_nametable;

static int
get_nametable_entrycount(const pcre *code, const pcre_extra *extra)
{
  int val;
  pcre_fullinfo(code, extra, PCRE_INFO_NAMECOUNT, &val);
  return val;
}

static pcre_nametable *
get_nametable(const pcre *code, const pcre_extra *extra)
{
  pcre_nametable *nametable = (pcre_nametable *)(pcre_malloc)(sizeof(pcre_nametable));
  if (!nametable) out_of_memory_failure("regex-extras", "get_nametable", "pcre_nametable");
  pcre_fullinfo(code, extra, PCRE_INFO_NAMETABLE, &nametable->nametable);
  pcre_fullinfo(code, extra, PCRE_INFO_NAMEENTRYSIZE, &nametable->entrysize);
  return nametable;
}

static char *
get_nametable_entry(const pcre_nametable *nt, int idx, int *pcc)
{
  typedef struct {
    uint16_t cc;
    char name[1];
  } pcre_nametable_entry;

  pcre_nametable_entry *entry = ((pcre_nametable_entry *)(nt->nametable)) + (idx * nt->entrysize);
  /* Number of capturing parentheses is MSB */
# ifdef C_LITTLE_ENDIAN
  uint16_t cc;
  ((uint8_t *)&cc)[0] = ((uint8_t *)&entry->cc)[1];
  ((uint8_t *)&cc)[1] = ((uint8_t *)&entry->cc)[0];
  *pcc = cc;
# else
  *pcc = entry->cc;
# endif
  return entry->name;
}
EOF
)

(define-foreign-type pcre_nametable (nonnull-c-pointer "pcre_nametable"))

(define re-nametable-entrycount
  (foreign-lambda int "get_nametable_entrycount" (const nonnull-pcre) (const pcre_extra)))

(define re-nametable
  (foreign-lambda nonnull-c-pointer "get_nametable" (const nonnull-pcre) (const pcre_extra)))

(define re-nametable-entry
  (foreign-lambda nonnull-c-string "get_nametable_entry" (const pcre_nametable) int (c-pointer int)))

(define (regexp-info-nametable rx)
  (##sys#check-structure rx 'regexp 'regexp-info-nametable)
  (let ([code (%regexp-code rx)]
        [extra (%regexp-extra rx)])
    (let ([cnt (re-nametable-entrycount code extra)])
      (unless (fx= 0 cnt)
        (let ([nt (re-nametable code extra)])
          (set-finalizer! nt re-finalizer)
          (let loop ([idx 0] [lst '()])
            (if (fx= cnt idx)
                lst
                (let-location ([cc int])
                (let ([nam (re-nametable-entry nt idx #$cc)])
                  (loop (fx+ idx 1) (cons (list nam idx cc) lst)) ) ) ) ) ) ) ) ) )


;;; PCRE config info:

;;

(define config-info-integer
  (foreign-lambda* int ((int fieldno))
    "int val;"
    "pcre_config(fieldno, &val);"
    "return(val);") )

(define config-info-boolean
  (foreign-lambda* bool ((int fieldno))
    "int val;"
    "pcre_config(fieldno, &val);"
    "return(val);") )

;;

(define (regex-build-config-info . fields)
  (map
    (lambda (sym)
      (##sys#check-symbol sym 'regex-build-config-info)
      (list sym
            (and-let* ([typ (alist-ref sym pcre-config-field-types eq?)])
              (let ([fldno (pcre-config-field->number sym)])
                (case typ
                  [(boolean)
                    (config-info-boolean fldno)]
                  [(integer)
                    (let ([int (config-info-integer fldno)])
                      (if (eq? 'newline sym)
                          (integer->char int)
                          int ) ) ]
                  [else
                    (##sys#error 'regex-build-config-info "unknown type" typ) ] ) ) ) ) )
    (if (null? fields) pcre-config-field-symbols fields)) )
