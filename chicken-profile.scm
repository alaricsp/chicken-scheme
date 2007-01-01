;;;; chicken-profile.scm - Formatted display of profile outputs - felix -*- Scheme -*-
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


(declare
  (block)
  (uses srfi-1))


(define sort-by #f)
(define file #f)
(define no-unused #f)

(define (print-usage)
  (display #<<EOF
Usage: chicken-profile [FILENAME | OPTION] ...

 -sort-by-calls            sort output by call frequency
 -sort-by-time             sort output by procedure execution time
 -sort-by-avg              sort output by average procedure execution time
 -sort-by-name             sort output alphabetically by procedure name
 -no-unused                remove procedures that are never called
 -help                     show this text and exit
 -version                  show version and exit
 -release                  show release number and exit

 FILENAME defaults to "PROFILE"

EOF
)
 (exit 64) )

(define (run args)
  (let loop ([args args])
    (if (null? args)
	(begin
	  (unless file (set! file "PROFILE"))
	  (write-profile) )
	(let ([arg (car args)]
	      [rest (cdr args)] )
	  (match arg
	    [(or "-h" "-help" "--help") (print-usage)]
	    [(or "-v" "-version") 
	     (print "chicken-profile - Version " (chicken-version))
	     (exit) ]
	    ["-release" 
	     (print (chicken-version))
	     (exit) ]
	    ["-no-unused" (set! no-unused #t)]
	    ["-sort-by-calls" (set! sort-by sort-by-calls)]
	    ["-sort-by-time" (set! sort-by sort-by-time)]
	    ["-sort-by-avg" (set! sort-by sort-by-avg)]
	    ["-sort-by-name" (set! sort-by sort-by-name)]
	    [_ (cond [(and (> (string-length arg) 1) (char=? #\- (string-ref arg 0)))
		      (error "invalid option" arg) ]
		     [file (print-usage)]
		     [else (set! file arg)] ) ] )
	  (loop rest) ) ) ) )

(define (sort-by-calls x y)
  (let ([c1 (second x)]
	[c2 (second y)] )
    (if (= c1 c2)
	(> (third x) (third y))
	(> c1 c2) ) ) )

(define (sort-by-time x y)
  (let ([c1 (third x)]
	[c2 (third y)] )
    (if (= c1 c2)
	(> (second x) (second y))
	(> c1 c2) ) ) )

(define (sort-by-avg x y)
  (let ([c1 (cadddr x)]
	[c2 (cadddr y)] )
    (if (= c1 c2)
	(> (third x) (third y))
	(> c1 c2) ) ) )

(define (sort-by-name x y)
  (string<? (symbol->string (first x)) (symbol->string (first y))) )

(set! sort-by sort-by-time)

(define (read-profile)
  (let ((hash (make-hash-table eq?)))
    (do ((line (read) (read)))
	((eof-object? line))
      (hash-table-set! hash (first line)
		       (map + (hash-table-ref/default hash (first line) '(0 0)) (cdr line))))
    (hash-table->alist hash)))

(define (format-string str cols #!optional right (padc #\space))
  (let* ((len (string-length str))
	 (pad (make-string (fxmax 0 (fx- cols len)) padc)) )
    (if right
	(string-append pad str)
	(string-append str pad) ) ) )

(define (format-real n cols fcols)
  (let ((an (abs n)))
    (format-string
     (string-append
      (number->string (inexact->exact (truncate n)))
      "."
      (let ((fstr (format-string (substring (number->string (exact->inexact (- an (truncate an)))) 2) fcols #f #\0)))
	(substring fstr 0 (fxmin (string-length fstr) fcols))) )
     cols #t #\space) ) )

(define (write-profile)
  (let* ([data0 (with-input-from-file file read-profile)]
	 [max-t (fold (lambda (t result)
			(max (third t) result))
		      0
		      data0)]
	 [data (sort (map
		      (lambda (t) (append t (let ((c (second t))
						  (t (third t)))
					      (list (or (and (> c 0) (/ t c))
							0)
						    (or (and (> max-t 0) (* (/ t max-t) 100))
							0)
						    ))))
		      data0)
                     sort-by)]
	 [line (make-string 79 #\-)] )
    (print (format-string "procedure" 38)
	   " "
	   (format-string "calls" 9 #t)
	   " "
	   (format-string "seconds" 9 #t)
	   " "
	   (format-string "average" 9 #t)
	   " "
	   (format-string "percent" 8 #t) )
    (print line)
    (for-each
     (lambda (entry)
       (let ([c (second entry)]
	     [t (third entry)]
	     [a (cadddr entry)]
	     [p (list-ref entry 4)] )
	 (unless (and (zero? c) no-unused)
	   (print (format-string (##sys#symbol->qualified-string (first entry)) 38)
		  " "
		  (format-string (number->string c) 9 #t)
		  " "
		  (format-real (/ t 1000) 9 3)
		  " "
		  (format-real (/ a 1000) 9 3)
		  " "
		  (format-real p 8 4) ) ) ) )
     data) ) )

(run (command-line-arguments))
