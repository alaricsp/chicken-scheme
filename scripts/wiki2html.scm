;;;; wiki2html.scm - quick-and-dirty svnwiki->HTML conversion
;
; usage: wiki2html <INPUTFILE >OUTPUTFILE


(use regex srfi-1 extras utils)
(use htmlprag matchable)


;;; inline elements

(define +code+ '(: #\{ #\{ (submatch (* (~ #\}))) #\} #\}))
(define +bold+ '(: (= 3 #\') (submatch (* (~ #\'))) (= 3 #\')))
(define +italic+ '(: (= 2 #\') (submatch (* (~ #\'))) (= 2 #\')))
(define +html-tag+ '(: #\< (submatch (* (~ #\>))) #\>))

(define +link+
  '(: #\[ #\[ (? (: (* space) "image:" (* space)))
      (submatch (* (~ #\] #\|))) (? #\| (submatch (* (~ #\])))) #\] #\]))

(define +inline-element+
  `(or ,+code+ ,+link+ ,+html-tag+ ,+bold+ ,+italic+))


;;; Block elements

(define +header+ '(: (submatch (>= 2 #\=)) (* space) (submatch (* any))))
(define +pre+ '(: (>= 1 space) (submatch (* any))))
(define +d-list+ '(: (* space) #\; (submatch (~ #\:)) #\: (submatch (* any))))
(define +u-list+ '(: (* space) (submatch (>= 1 #\*)) (* space) (submatch (* any))))
(define +o-list+ '(: (* space) (submatch (>= 1 #\*)) #\# (* space) (submatch (* any))))
(define +hr+ '(: (* space) (submatch (>= 3 #\-)) (* space)))
(define +emptyline+ '(: bos (* space) eos))

(define +block-element+
  `(or ,+pre+
       ,+header+
       ,+d-list+
       ,+u-list+
       ,+o-list+
       ,+hr+))


;;; Global state

(define *toc* #f)
(define *tags* '())
(define *open* '())

(define (push-tag tag out)
  (unless (and (pair? *open*) (equal? tag (car *open*)))
    (when (pair? *open*)
      (pop-tag out))
    (fprintf out "<~a>~%" (if (pair? tag) (car tag) tag))
    (set! *open* (cons tag *open*))))

(define (pop-tag out)
  (let ((tag (car *open*)))
    (fprintf out "</~a>~%" (if (pair? tag) (car tag) tag))
    (set! *open* (cdr *open*))))

(define (pop-all out)
  (when (pair? *open*)
    (pop-tag out)
    (pop-all out)))


;;; Helper syntax

(define-syntax rx
  (syntax-rules ()
    ((_ rx) (force (delay (regexp rx))))))


;;; Conversion entry point

(define (wiki->html #!optional (in (current-input-port)) (out (current-output-port)))
  (call/cc
   (lambda (return)
     (let loop ()
       (let ((ln (read-line in)))
	 (cond ((eof-object? ln) (return #f))
	       ((string-match (rx +emptyline+) ln)
		(fprintf out "~%"))
	       ((not (string-match (rx +block-element+) ln)) 
		(pop-all out)
		(fprintf out "~a~%" (inline ln)))
	       ((string-match (rx +header+) ln) =>
		(lambda (m)
		  (pop-all out)
		  (let ((n (sub1 (string-length (second m)))))
		    (fprintf out "<h~a>~a</h~a>~%" n (third m) n))))
	       ((string-match (rx +pre+) ln) =>
		(lambda (m)
		  (push-tag 'pre out)))
	       ((string-match (rx +hr+) ln) =>
		(lambda (m)
		  (fprintf out "<hr />~%")))
	       ((string-match (rx +d-list+) ln) =>
		(lambda (m)
		  (push-tag 'dl out)
		  (fprintf out "<dt>~a</dt><dd>~a</dd>~%" 
			   (inline (second m)) (inline (third m)))))
	       ((string-match (rx +u-list+) ln) =>
		(lambda (m)
		  (push-tag `(ul . ,(string-length (second m))) out)
		  (fprintf out "<li>~a~%" (inline (third m)))))
	       ((string-match (rx +o-list+) ln) =>
		(lambda (m)
		  (push-tag `(ol . ,(string-length (second m))) out)
		  (fprintf out "<li>~a~%" (inline (third m)))))
	       (else (error "unknown block match" m))´)
	 (loop))))))


;;; Substitute inline elements

(define (inline str)
  (or (and-let* ((m (string-search-positions (rx +inline-element+) str)))
	(string-append
	 (clean (substring str 0 (caar m)))
	 (let ((rest (substring str (caar m))))
	   (define (continue m)
	     (inline (substring rest (string-length (first m)))))
	   (cond ((string-search (rx `(: bos ,+code+)) rest) =>
		  (lambda (m)
		    (string-append 
		     "<tt>" (clean (second m)) "</tt>"
		     (continue m))))
		 ((string-search (rx `(: bos ,+html-tag+)) rest) =>
		  (lambda (m)
		    (string-append
		     (first m)
		     (continue m))))
		 ((string-search(rx `(: bos ,+link+)) rest) =>
		  (lambda (m)
		    (string-append
		     "<a href='" (clean (second m)) "'>"
		     (clean (or (third m) (second m)))
		     "</a>"
		     (continue m)))) 
		 ((string-search (rx `(: bos ,+bold+)) rest) =>
		  (lambda (m)
		    (string-append
		     "<b>" (inline (second m)) "</b>"
		     (continue m)))) 
		 ((search (rx `(: bos ,+italic+)) rest) =>
		  (lambda (m)
		    (string-append
		     "<i>" (inline (second m)) "</i>"
		     (continue m)))) 
		 (else (error "unknown inline match" m rest))))))
      str))


;;; Normalize text

(define (clean str)
  (string-translate* str '(("<" . "&lt;") ("&" . "&amp;") ("'" . "&quot;"))))


;;; run it

(let ((sxml (html->sxml (open-input-string (with-output-to-string wiki->html)))))
  (define (walk n)
    (match n
      (('*PI* . _) n)
      (('enscript strs ...)
       `(pre ,@strs))
      (((? symbol? tag) . body)
       `(,tag ,@(map walk body)))
      (_ n)))
  (sxml->html (walk sxml)))
