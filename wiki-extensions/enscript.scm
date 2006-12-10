(require-extension srfi-40 html-stream)

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

(define (tag-enscript params text info)
  (let ((highlight (assoc 'highlight params)))
    (if (or (not highlight) (stream-every (disjoin char-alphabetic? char-numeric?) (cdr highlight)))
      (receive (in out pid)
               (process (format #f "enscript~A~A --color --language=html -p-"
                                (if highlight " -E" "")
                                (if highlight (stream->string (cdr highlight)) "")))
        (write-stream (stream-drop-while (lambda (a) (char=? a #\newline)) text) out)
        (close-output-port out)
        (let ((in-str (port->stream in)))
          (stream-append
            (if (stream-null? in-str)
              (html-stream (pre text))
              (stream-take-while-all
                (complement (lambda (x) (stream-prefix= x (list #\< #\H #\R #\>))))
                (stream-drop-while-all
                  (complement (lambda (x) (stream-prefix= x (list #\< #\P #\R #\E #\>))))
                  in-str)))
            (stream-delay
              (begin (process-wait pid)
                     stream-null)))))
      stream-null)))

(define *extensions*
  `((enscript (code-span ,tag-enscript))))
