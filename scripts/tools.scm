;;;; tools.scm


(use (srfi 1) posix utils files)


(define *verbose* (##sys#fudge 13))
(define *dependencies* (make-hash-table string=?))
(define *variables* (make-hash-table string=?))
(define *actions* (make-hash-table string=?))
(define *pseudo-targets* '())
(define *sleep-delay* 2)

(define *windows-shell*
  (memq (build-platform) '(mingw32 msvc)))


;;; Verbosity and output

(define *tty* 
  (and (##sys#tty-port? (current-output-port)) 
       (not (equal? (getenv "EMACS") "t"))
       (not (equal? (getenv "TERM") "dumb"))))

(define *tty-width*
  (or (and *tty*
	   (not *windows-shell*)
	   (with-input-from-pipe "stty size 2>/dev/null"
	     (lambda () (read) (read))))
      72))

(define *info-message-escape* (if *tty* "\x1b[0m\x1b[2m" ""))
(define *target-message-escape* (if *tty* "\x1b[0m\x1b[32m" ""))
(define *error-message-escape* (if *tty* "\x1b[0m\x1b[31m" ""))
(define *command-message-escape* (if *tty* "\x1b[0m\x1b[33m" ""))
(define *reset-escape* (if *tty* "\x1b[0m" ""))

(define (format-message msg #!optional (nl #t))
  (if (or *verbose* (not *tty*))
      ((if nl print print*) msg)
      (for-each
       (lambda (ln)
	 (printf "\r\x1b[K~a~!"
		 (if (>= (string-length ln) (sub1 *tty-width*))
		     (string-append
		      (substring ln 0 (- *tty-width* 5))
		      "...")
		     ln) ) )
       (string-split msg "\n")) ) )

(define (message fstr . args)
  (when *verbose*
    (format-message (sprintf "~a* ~?~a " *info-message-escape* fstr args *reset-escape*)) ) )

(define (message* fstr . args)
  (when *verbose*
    (format-message (sprintf "~a* ~?~a " *info-message-escape* fstr args *reset-escape*) #f) ) )

(define (target-message fstr . args)
  (format-message (sprintf "~a~?~a " *target-message-escape* fstr args *reset-escape*)))

(define (command-message fstr . args)
  (when *verbose*
    (format-message (sprintf "~a  ~?~a " *command-message-escape* fstr args *reset-escape*))) )

(define (error-message fstr . args)
  (sprintf "~%~a~?~a~%" *error-message-escape* fstr args *reset-escape*))

(define (quit fstr . args)
  (display (apply error-message fstr args) (current-error-port))
  (reset) )

(define (cleanup-output)
  (when (and (not *verbose*) *tty*)
    (printf "\r\x1b[0m\x1b[K~!") ) )


;;; make-code stolen from PLT

(define (find-matching-line str spec)
  (let ([match? (lambda (s) (string=? s str))])
    (let loop ([lines spec])
      (cond
       [(null? lines) #f]
       [else (let* ([line (car lines)]
		    [names (if (string? (car line))
			       (list (car line))
			       (car line))])
	       (if (any match? names)
		   line
		   (loop (cdr lines))))]))))

(define (form-error s p) (quit "~a: ~s" s p))
(define (line-error s p n) (quit "~a: ~s in line ~a" s p))

(define (check-spec spec)
  (and (or (list? spec) (form-error "specification is not a list" spec))
       (or (pair? spec) (form-error "specification is an empty list" spec))
       (every
	(lambda (line)
	  (and (or (and (list? line) (<= 2 (length line) 3))
		   (form-error "list is not a list with 2 or 3 parts" line))
	       (or (or (string? (car line))
		       (and (list? (car line))
			    (every string? (car line))))
		   (form-error "line does not start with a string or list of strings" line))
	       (let ([name (car line)])
		 (or (list? (cadr line))
		     (line-error "second part of line is not a list" (cadr line) name)
		     (every (lambda (dep)
			       (or (string? dep)
				   (form-error "dependency item is not a string" dep)))
			     (cadr line)))
		 (or (null? (cddr line))
		     (procedure? (caddr line))
		     (line-error "command part of line is not a thunk" (caddr line) name)))))
	spec)))

(define (check-argv argv)
  (or (string? argv)
      (and (vector? argv)
	   (every string? (vector->list argv)))
      (error "argument is not a string or string vector" argv)))

(define (make/proc/helper spec argv)
  (check-spec spec)
  (check-argv argv)
  (letrec ([made '()]
	   [exn? (condition-predicate 'exn)]
	   [exn-message (condition-property-accessor 'exn 'message)]
	   [make-file
	    (lambda (s indent)
	      (let ([line (find-matching-line s spec)]
		    [date (and (not (member s *pseudo-targets*))
			       (file-exists? s)
			       (file-modification-time s))])
		(if line
		    (let ([deps (cadr line)])
		      (for-each (let ([new-indent (string-append " " indent)])
				  (lambda (d) (make-file d new-indent)))
				deps)
		      (let ([reason
			     (or (not date)
				 (any (lambda (dep)
					  (unless (file-exists? dep)
					    (quit "dependancy ~a was not made~%" dep))
					  (and (> (file-modification-time dep) date)
					       dep))
					deps))])
			(when reason
			  (let ([l (cddr line)])
			    (unless (null? l)
			      (set! made (cons s made))
			      ((car l)))))))
		    (when (not date) 
		      (quit "don't know how to make ~a" s)))))])
    (cond
     [(string? argv) (make-file argv "")]
     [(equal? argv '#()) (make-file (caar spec) "")]
     [else (for-each (lambda (f) (make-file f "")) (vector->list argv))]) ) )

(define make/proc
  (case-lambda
   [(spec) (make/proc/helper spec '#())]
   [(spec argv) (make/proc/helper spec argv)]))


;;; Run subcommands

(define (execute exps)
  (for-each
   (lambda (exp)
     (let ((cmd (string-intersperse (map ->string (flatten exps)))))
       (command-message "~A" cmd)
       (let ((s (system cmd)))
	 (unless (zero? s)
	   (quit (sprintf "invocation of command failed with non-zero exit-status ~a: ~a~%" s cmd) s) ) ) ) )
   exps) )

(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (execute (list `exp ...)))))


;;; String and path helper functions

(define (prefix dir . files)
  (if (null? files)
      (pathname-directory dir)
      (let ((files2 (map (cut make-pathname dir <>) (normalize files))))
	(if (or (pair? (cdr files)) (pair? (car files)))
	    files2
	    (car files2) ) ) ) )

(define (suffix suf . files)
  (if (null? files)
      (pathname-extension suf)
      (let ((files2 (map (cut pathname-replace-extension <> suf) (normalize files))))
	(if (or (pair? (cdr files)) (pair? (car files)))
	    files2
	    (car files2) ) ) ) )

(define (normalize fs)
  (delete-duplicates
   (map ->string
	(if (pair? fs)
	    (flatten fs)
	    (list fs) ) )
   equal?) )

(define path make-pathname)


;;; "Stateful" build interface

(define (build-clear)
  (set! *dependencies* (make-hash-table string=?)) 
  (set! *actions* (make-hash-table string=?)) 
  (set! *variables* (make-hash-table string=?)) )

(define (depends target . deps)
  (let ((deps (normalize deps)))
    (hash-table-update!
     *dependencies* target
     (lambda (old) (lset-union string=? old deps))
     (lambda () deps) ) ) )

(define actions
  (let ((doaction 
	  (lambda (name target proc)
	    (hash-table-update! *dependencies* target identity (constantly '()))
	    (hash-table-set! 
	     *actions* target 
	     (lambda ()
	       (target-message "~a\t~a" name target)
	       (proc) ) ) ) ) )
    (case-lambda
     ((target proc) (doaction "build " target proc))
     ((name target proc) (doaction name target proc)) ) ) )

(define (notfile . targets)
  (set! *pseudo-targets* (lset-union string=? *pseudo-targets* targets)))

(define (clean-on-error t thunk)
  (handle-exceptions ex
      (begin
	(when (file-exists? t)
	  (message "deleting ~a" t)
	  (delete-file t) )
	(abort ex) )
    (thunk) ) )

(define (build #!optional
	       (targets "all")
	       #!key
	       continuous
	       (verbose *verbose*) )
  (fluid-let ((*verbose* verbose))
    (let* ((deps (hash-table->alist *dependencies*))
	   (wdeps (delete-duplicates (append-map cdr deps) string=?))
	   (targets (list->vector (normalize targets)) ) 
	  (ftable (and continuous (make-hash-table string=?))))
      (when continuous
	(for-each 
	 (lambda (dep)
	   (when (file-exists? dep) 
	     (hash-table-set! ftable dep (file-modification-time dep))))
	 wdeps))
      (let loop ()
	(make/proc
	 (map (lambda (dep)
		(let ((target (car dep))
		      (deps (cdr dep)))
		 (list target deps
		       (eval
			`(lambda ()
			   (clean-on-error
			    ',target
			    (lambda ()
			      ((hash-table-ref/default *actions* ',target noop)))))))))
	      deps)
	 targets)
	(when continuous
	  (watch-dependencies wdeps ftable)
	  (loop)))
      (cleanup-output))))

(define (build-dump #!optional (port (current-output-port)))
  (with-output-to-port port
    (lambda ()
      (message "dependencies:")
      (for-each show-dependencies (hash-table-keys *dependencies*))
      (when (positive? (hash-table-size *variables*))
	(message "variables:")
	(hash-table-walk
	 *variables*
	 (lambda (v x)
	   (message "  ~s:" v)
	   (for-each
	    (lambda (p)
	      (message "    ~a\t-> ~s~%" (car p) (cadr p))) 
	    x))) ) ) ) )

(define (show-dependencies target)
  (let ((i ""))
    (let loop ((t target))
      (message "~a~a ~a" i t (if (member t *pseudo-targets*) "(p)" ""))
      (fluid-let ((i (string-append i " ")))
	(for-each loop (hash-table-ref/default *dependencies* t '())) ) ) ) )


;;; Command line processing

(define (build* . args)
  (let ((continuous #f)
	(targets '()) 
	(debug #f) )
    (let-values (((procs arglists) (partition procedure? args)))
      (let loop ((args (if (null? arglists) 
			   (command-line-arguments) 
			   (concatenate arglists))) )
	(cond ((null? args) 
	       (when debug (build-dump))
	       (for-each (lambda (p) (p)) procs)
	       (build 
		(if (null? targets) "all" (reverse targets))
		verbose: *verbose*
		continuous: continuous) )
	      (else
	       (let ((x (car args)))
		 (cond ((and (> (string-length x) 0) (char=? #\- (string-ref x 0)))
			(cond ((string=? "-v" x) 
			       (set! *verbose* #t) )
			      ((member x '("-h" "-help" "--help"))
			       (usage 0) )
			      ((string=? "-c" x)
			       (set! continuous #t) )
			      ((string=? "-d" x)
			       (set! debug #t) )
			      (else (usage 1)) )
			(loop (cdr args)) )
		       ((string-match "([-_A-Za-z0-9]+)=(.*)" x) =>
			(lambda (m)
			  (let* ((sym (string->symbol (cadr m))))
			    (if (##sys#symbol-has-toplevel-binding? sym)
				(let ((val (##sys#slot sym 0)))
				  (if (or (boolean? val) (string? val) (symbol? val) (eq? (void) val))
				      (##sys#setslot sym 0 (caddr m)) 
				      (quit "variable `~a' already has a suspicious value" sym) ) )
				(##sys#setslot sym 0 (caddr m)) )
			    (loop (cdr args)) ) ) )
		       (else
			(set! targets (cons x targets))
			(loop (cdr args))))))))) ) )

(define (usage code)
  (print "usage: " (car (argv)) " [ -v | -c | -d | TARGET | VARIABLE=VALUE ] ...")
  (exit code) )


;;; Check dependencies for changes

(define (watch-dependencies deps tab)
  (let loop ((f #f))
    (sleep *sleep-delay*)
    (cond ((any (lambda (dep)
		  (and-let* (((file-exists? dep))
			     (ft (file-modification-time dep))
			     ((> ft (hash-table-ref/default tab dep 0))))
		    (hash-table-set! tab dep ft)
		    (message "~a changed" dep)
		    #t) )
		deps))
	  (f (loop #t))
	  (else 
	   (message "waiting for changes ...")
	   (loop #t)))))


;;; Other useful procedures

(define -e file-exists?)
(define -d (conjoin file-exists? directory?))
(define -x (conjoin file-exists? file-execute-access?))

(define cwd current-directory)
(define (cd #!optional d) (if d (current-directory d) (getenv "HOME")))

(define (with-cwd dir thunk)
  (if (or (not dir) (equal? "." dir))
      (thunk)
      (let ((old #f))
	(dynamic-wind
	    (lambda () (set! old (current-directory)))
	    (lambda ()
	      (command-message "cd ~a" dir)
	      (change-directory dir)
	      (thunk) )
	    (lambda ()
	      (change-directory old)
	      (command-message "cd ~a" old) ) ) ) ) )

(define (try-run code #!optional (msg "trying to compile and run some C code") (flags "") (cc "cc"))
  (let ((tmp (create-temporary-file "c")))
    (with-output-to-file tmp (lambda () (display code)))
    (message* "~a ..." msg)
    (let ((r (zero? (system (sprintf "~a ~a ~a 2>/dev/null && ./a.out" cc tmp flags)))))
      (delete-file* tmp)
      (message (if r "ok" "failed"))
      r) ) )

(define (true? x)
  (and x (not (member x '("no" "false" "off" "0" "")))))

(define (simple-args #!optional (args (command-line-arguments)) (error error))
  (define (assign var val)
    (##sys#setslot 
     (string->symbol (string-append "*" var "*"))
     0
     (if (string? val) 
	 (or (string->number val) val)
	 val)))
  (let loop ((args args) (vals '()))
    (cond ((null? args) (reverse vals))
	  ((string-match "(-{1,2})([-_A-Za-z0-9]+)(=)?\\s*(.+)?" (car args)) 
	   =>
	   (lambda (m)
	     (let*-values (((next) (cdr args))
			   ((var val)
			    (cond ((equal? "=" (fourth m))
				   (let ((opt (third m))
					 (val (fifth m)))
				     (cond (val (values opt val))
					   (else 
					    (when (null? next)
					      (error "missing argument for option" (car args)) )
					    (let ((x (car next)))
					      (set! next (cdr next))
					      (values opt x))))) )
				  ((string? (second m)) (values (third m) #t))
				  (else (values #f #f)) ) ) )
	       (cond (var 
		      (assign var val)
		      (loop next vals) )
		     (else (loop next (cons (car args) vals)))))))
	  (else (loop (cdr args) (cons (car args) vals))))))

(define (yes-or-no? str . default)
  (let ((def (optional default #f)))
    (let loop ()
      (printf "~%~A (yes/no) " str)
      (when def (printf "[~A] " def))
      (flush-output)
      (let ((ln (read-line)))
	(cond ((eof-object? ln) (set! ln "abort"))
	      ((and def (string=? "" ln)) (set! ln def)) )
	(cond ((string-ci=? "yes" ln) #t)
	      ((string-ci=? "no" ln) #f)
	      (else
	       (printf "~%Please enter \"yes\" or \"no\".~%")
	       (loop) ) ) ) ) ) )
