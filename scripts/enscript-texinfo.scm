;;
;; Texinfo variant of the enscript extension for stream-wiki.  Please
;; note that this enscript extension is not the same as the enscript
;; extension distributed with stream-wiki.
;;
;; Author: Ivan Raikov <raikov@oist.jp>
;;

(require-extension syntax-case srfi-40 html-stream stream-ext)

(define-syntax environment
  (syntax-rules ()
    ((environment original ((name expr) ...))
     (lambda (op)
       (case op
         ((name) expr)
         ...
         (else (original op)))))
    ((environment ((name expr) ...))
     (environment (lambda (op)
                    (warning "unbound variable (dynamic environment)" op)
                    (if #f #f))
                  ((name expr) ...)))))

(define-syntax environment-get
  (syntax-rules ()
    ((environment-get env sym) (env 'sym))))

(define-syntax let-from-environment
  (syntax-rules ()
    ((let-from-environment env (sym ...) body ...)
     (let ((sym (environment-get env sym)) ...) body ...))))

(define-syntax environment-capture
  (syntax-rules ()
    ((environment-capture env (sym ...))
     (environment env ((sym sym) ...)))
    ((environment-capture (sym ...))
     (environment ((sym sym) ...)))))

(define (stream-take-while-all pred str)
  (stream-delay
    (if (or (stream-null? str) (not (pred str)))
      stream-null
      (stream-cons
        (stream-car str)
        (stream-take-while-all pred (stream-cdr str))))))

(define (stream-drop-while-all pred str)
  (stream-delay
    (if (or (stream-null? str) (not (pred str)))
      str
      (stream-drop-while-all pred (stream-cdr str)))))

; In Gentoo, enscript 1.6.3 advertises the -w option:
;
;  -w, --language=LANG        set output language to LANG
;
; However, it stupidly fails:
;
; enscript: invalid option -- w
;
; Trying -whtml, -w=html, -w html all fail.
;
; Interestingly, --language=html does work.
;
; In SLES9, Enscript 1.6.2 doesn't have any --highlight option.  We're
; using -E instead.

(define (tag-enscript env)
  (let-from-environment env (params text)
    (let ((highlight (assoc 'highlight params)))
      (if (or (not highlight) (stream-every (disjoin char-alphabetic? char-numeric?) (cdr highlight)))
        (receive (in out pid)
                 (process (format #f "enscript~A~A -w texinfo -p- -q"
                                  (if highlight " -E" "")
                                  (if highlight (stream->string (cdr highlight)) "")))
          (write-stream (stream-drop-while (lambda (a) (char=? a #\newline)) text) out)
	  (close-output-port out)
          (let ((in-str (port->stream in)))
            (stream-append
	     in-str
	     (stream-delay
	      (begin ;(process-wait pid)
		     stream-null)))))
        stream-null))))

(define *extensions*
  `((enscript (code-span ,tag-enscript))))
