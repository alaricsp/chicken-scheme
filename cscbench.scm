;;;; cscbench - Compile and run benchmarks - felix -*- Scheme -*-
;
; - Usage: cscbench [-debug] [-cc=<path>] OPTION ...

(require-extension srfi-1 utils posix regex)

(define flonum-files '("fft" "maze"))

(define cc "`../csc -cc-name`")
(define chicken "../chicken")

(define (abort-run) #f)

(define run
  (let ([secrx (regexp "^ *([-.+e0-9]*(\\.[0-9]*)?) seconds elapsed$")])
    (lambda ()
      (system* "./tmpfile >tmpfile.out")
      (with-input-from-file "tmpfile.out"
        (lambda ()
          (let loop ([line (read-line)])
            (if (eof-object? line) 
                (abort-run)
                (let ([m (string-match secrx line)])
                  (if m
                      (string->number (second m)) 
                      (loop (read-line)) ) ) ) ) ) ) ) ) )

(define (display-l str len pad)
  (let ([slen (string-length str)])
    (display (substring str 0 (min slen len)))
    (display (make-string (max 0 (- len slen)) pad)) ) )

(define (display-r str len pad)
  (let ([slen (string-length str)])
    (display (make-string (max 0 (- len slen)) pad))
    (display (substring str 0 (min slen len))) ) )

(define display-f-4.3
  (let ([florx (regexp "^([-+e0-9]*)(\\.([0-9]*))?$")])
    (lambda (n)
      (let* ([m (string-match florx (number->string n))]
	     [is (second m)]
	     [fs (fourth m)] )
        (display-r is 4 #\space)
        (display #\.)
        (display-r fs 3 #\0) ) ) ) )

(define (compile-and-run file decls options coptions unsafe)
  (system* "~A ~A -quiet -no-warnings -heap-size 8m -output-file tmpfile.c ~A ~A"
           chicken file decls options)
  (system* "~a ~a -static -I.. tmpfile.c -o tmpfile ../lib~achicken.a -lm"
	   cc coptions (if unsafe "u" ""))
  (let ([time (call-with-current-continuation
	       (lambda (abort)
		 (set! abort-run (cut abort #f))
		 (/ (+ (run) (run) (run)) 3) ) ) ] )
    (display #\space)
    (cond (time
            (display-f-4.3 time)
            time)
          (else
            (display "FAILED")
            9999.9))))

(define (dflush x)
  (display x)
  (flush-output) )

(define (main options)
  (when (and (pair? options) (string=? "-debug" (car options)))
    (set! options (cdr options))
    (set! system*
      (let ([system* system*])
	(lambda args
	  (let ([s (apply sprintf args)])
	    (printf "system: ~A~%" s)
	    (system* s) ) ) ) ) )
  (and-let* ([(pair? options)]
	     [m (string-match "-cc=(.*)" (car options))] )
    (set! options (cdr options))
    (set! cc (second m)) )
  (delete-file* "tmpfile.scm")
  (system* "~A -version" chicken)
  (dflush "\nCC:\n")
  (if (eq? (build-platform) 'sun)
      (system (conc cc " -V"))
      (system* "~A -v" cc) )
  (dflush "\nCFLAGS:\n")
  (system* "echo `../csc -cflags`")
  (display "\n                  base       unsafe     max\n")
  (let ((sum-base 0.0)
        (sum-unsafe 0.0)
        (sum-max 0.0))
  (for-each
   (lambda (file)
     (let* ([name (pathname-file file)]
	    [options (string-intersperse options " ")] )
       (display-l name 16 #\space)
       (flush-output)
       (set! sum-base (+ sum-base (compile-and-run file "-debug-level 0 -optimize-level 1 -lambda-lift" options "" #f)))
       (dflush "  ")
       (set! sum-unsafe (+ sum-unsafe (compile-and-run file "-debug-level 0 -optimize-level 3 -block -disable-interrupts -lambda-lift" options "" #t)))
       (dflush "  ")
       (unless (member name flonum-files)
         (set! sum-max (+ sum-max (compile-and-run file "-benchmark-mode" options "" #t) )))
       (newline)
       (flush-output) ) )
   (lset-difference string=? (sort (glob "*.scm") string<?) '("plists.scm")))
  (display "\nTOTAL            ")
  (display-f-4.3 sum-base)
  (display "   ")
  (display-f-4.3 sum-unsafe)
  (display "   ")
  (display-f-4.3 sum-max)
  (newline)
 0))

(main (command-line-arguments))
