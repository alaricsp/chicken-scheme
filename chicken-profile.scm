;;;; chicken-profile.scm - Formatted display of profile outputs - felix -*- Scheme -*-
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; Copyright (c) 2008-2009, The Chicken Team
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


(declare
  (block)
  (uses srfi-1
	srfi-13
	srfi-69
	posix
	utils))

(define sort-by #f)
(define file #f)
(define no-unused #f)
(define seconds-digits 3)
(define average-digits 3)
(define percent-digits 3)
(define top 0)

(define (print-usage)
  (display #<#EOF
Usage: chicken-profile [FILENAME | OPTION] ...

 -sort-by-calls            sort output by call frequency
 -sort-by-time             sort output by procedure execution time
 -sort-by-avg              sort output by average procedure execution time
 -sort-by-name             sort output alphabetically by procedure name
 -decimals DDD             set number of decimals for seconds, average and
                           percent columns (three digits, default: #{seconds-digits}#{average-digits}#{percent-digits})
 -no-unused                remove procedures that are never called
 -top N                    display only the top N entries
 -help                     show this text and exit
 -version                  show version and exit
 -release                  show release number and exit

 FILENAME defaults to the `PROFILE.<number>', selecting the one with
 the highest modification time, in case multiple profiles exist.

EOF
)
 (exit 64) )

(define (run args)
  (let loop ([args args])
    (if (null? args)
	(begin
	  (unless file 
	    (set! file
	      (let ((fs (glob "PROFILE.*")))
		(if (null? fs)
		    (error "no PROFILEs found")
		    (first (sort fs 
				 (lambda (f1 f2)
				   (> (file-modification-time f1)
				      (file-modification-time f2))) ) ) ) ) ) )
	  (write-profile) )
	(let ([arg (car args)]
	      [rest (cdr args)] )
	  (define (next-arg)
	    (if (null? rest)
		(error "missing argument to option" arg)
		(let ((narg (car rest)))
		  (set! rest (cdr rest))
		  narg)))
	  (define (next-number)
	    (let ((n (string->number (next-arg))))
	      (if (and n (> n 0)) n (error "invalid argument to option" arg))))
	  (cond 
	   [(member arg '("-h" "-help" "--help")) (print-usage)]
	   [(member arg '("-v" "-version"))
	    (print "chicken-profile - Version " (chicken-version))
	    (exit) ]
	   [(string=? arg "-release")
	    (print (chicken-version))
	    (exit) ]
	   [(string=? arg "-no-unused") (set! no-unused #t)]
	   [(string=? arg "-top") (set! top (next-number))]
	   [(string=? arg "-sort-by-calls") (set! sort-by sort-by-calls)]
	   [(string=? arg "-sort-by-time") (set! sort-by sort-by-time)]
	   [(string=? arg "-sort-by-avg") (set! sort-by sort-by-avg)]
	   [(string=? arg "-sort-by-name") (set! sort-by sort-by-name)]
	   [(string=? arg "-decimals") (set-decimals (next-arg))]
	   [(and (> (string-length arg) 1) (char=? #\- (string-ref arg 0)))
	    (error "invalid option" arg) ]
	   [file (print-usage)]
	   [else (set! file arg)] )
	  (loop rest) ) ) ) )

(define (sort-by-calls x y)
  (let ([c1 (second x)]
	[c2 (second y)] )
    (if (eqv? c1 c2)
	(> (third x) (third y))
	(if c1 (if c2 (> c1 c2) #t) #t) ) ) )

(define (sort-by-time x y)
  (let ([c1 (third x)]
	[c2 (third y)] )
    (if (= c1 c2)
	(> (second x) (second y))
	(> c1 c2) ) ) )

(define (sort-by-avg x y)
  (let ([c1 (cadddr x)]
	[c2 (cadddr y)] )
    (if (eqv? c1 c2)
	(> (third x) (third y))
	(> c1 c2) ) ) )

(define (sort-by-name x y)
  (string<? (symbol->string (first x)) (symbol->string (first y))) )

(set! sort-by sort-by-time)

(define (set-decimals arg)
  (if (= (string-length arg) 3)
      (begin
	(define (arg-digit n)
	  (let ((n (- (char->integer (string-ref arg n))
		      (char->integer #\0))))
	    (if (<= 0 n 9)
		(if (= n 9) 8 n) ; 9 => overflow in format-real
		(error "invalid argument to -decimals option" arg))))
	(set! seconds-digits (arg-digit 0))
	(set! average-digits (arg-digit 1))
	(set! percent-digits (arg-digit 2)))
      (error "invalid argument to -decimals option" arg)))

(define (read-profile)
  (let ((hash (make-hash-table eq?)))
    (do ((line (read) (read)))
	((eof-object? line))
      (hash-table-set!
       hash (first line)
       (map (lambda (x y) (and x y (+ x y)))
	    (hash-table-ref/default hash (first line) '(0 0)) 
	    (cdr line))))
    (hash-table->alist hash)))

(define (format-string str cols #!optional right (padc #\space))
  (let* ((len (string-length str))
	 (pad (make-string (fxmax 0 (fx- cols len)) padc)) )
    (if right
	(string-append pad str)
	(string-append str pad) ) ) )

(define (format-real n d)
  (let ((exact-value (inexact->exact (truncate n))))
    (string-append
     (number->string exact-value)
     (if (> d 0) "." "")
     (substring
      (number->string
       (inexact->exact
	(truncate
	 (* (- n exact-value -1) (expt 10 d)))))
      1 (+ d 1)))))

(define (write-profile)
  (print "reading `" file "' ...\n")
  (let* ([data0 (with-input-from-file file read-profile)]
	 [max-t (fold (lambda (t result)
			(max (third t) result))
		      0
		      data0)]
	 [data (sort (map
		      (lambda (t) (append t (let ((c (second t))
						  (t (third t)))
					      (list (or (and c (> c 0) (/ t c))
							0)
						    (or (and (> max-t 0) (* (/ t max-t) 100))
							0)
						    ))))
		      data0)
                     sort-by)])
    (if (< 0 top (length data))
	(set! data (take data top)))
    (set! data (map (lambda (entry)
		      (let ([c (second entry)]
			    [t (third entry)]
			    [a (cadddr entry)]
			    [p (list-ref entry 4)] )
			(list (##sys#symbol->qualified-string (first entry))
			      (if (not c) "overflow" (number->string c))
			      (format-real (/ t 1000) seconds-digits)
			      (format-real (/ a 1000) average-digits)
			      (format-real p percent-digits))))
		    (remove (lambda (entry) 
			      (if (second entry) 
				  (and (zero? (second entry)) no-unused)
				  #f) )
			    data)))
    (let* ([headers (list "procedure" "calls" "seconds" "average" "percent")]
	   [alignments (list #f #t #t #t #t)]
	   [spacing 2]
	   [spacer (make-string spacing #\space)]
	   [column-widths (fold
			   (lambda (row max-widths)
			     (map max (map string-length row) max-widths))
			   (list 0 0 0 0 0)
			   (cons headers data))])
      (define (print-row row)
	(print (string-join (map format-string row column-widths alignments) spacer)))
      (print-row headers)
      (print (make-string (+ (reduce + 0 column-widths)
			     (* spacing (- (length alignments) 1)))
			  #\-))
      (for-each print-row data))))
  
(run (command-line-arguments))
