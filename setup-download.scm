;;;; setup-download.scm
;
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


(require-library extras regex posix utils setup-api srfi-1 data-structures tcp srfi-13
		 files)


(module setup-download (retrieve-extension
			locate-egg/local
			locate-egg/svn
			locate-egg/http
			gather-egg-information
			list-extensions
			temporary-directory)

  (import scheme chicken)
  (import extras regex posix utils srfi-1 data-structures tcp srfi-13 files setup-api)

  (define-constant +default-tcp-connect-timeout+ 10000) ; 10 seconds
  (define-constant +default-tcp-read/write-timeout+ 20000) ; 20 seconds

  (tcp-connect-timeout +default-tcp-connect-timeout+)
  (tcp-read-timeout +default-tcp-read/write-timeout+)
  (tcp-write-timeout +default-tcp-read/write-timeout+)

  (define *quiet* #f)

  (define *chicken-install-user-agent* (conc "chicken-install " (chicken-version)))

  (define (d fstr . args)
    (let ([port (if *quiet* (current-error-port) (current-output-port))])
      (apply fprintf port fstr args)
      (flush-output port) ) )

  (define temporary-directory (make-parameter #f))

  (define (get-temporary-directory)
    (or (temporary-directory)
	(let ([dir (create-temporary-directory)])
	  (temporary-directory dir)
	  dir ) ) )

  (define (existing-version egg version vs)
    (if version
        (if (member version vs)
            version
            (error "version not found" egg version) )
        (let ([vs (sort vs version>=?)])
          (and (pair? vs)
               (car vs) ) ) ) )

  (define (when-no-such-version-warning egg version)
    (when version (warning "extension has no such version - using default" egg version)) )

  (define (list-eggs/local dir)
    (string-concatenate (map (cut string-append <> "\n") (directory dir))) )

  (define (locate-egg/local egg dir #!optional version destination)
    (let* ([eggdir (make-pathname dir egg)]
	   [tagdir (make-pathname eggdir "tags")]
           [tagver (and (file-exists? tagdir) (directory? tagdir)
                        (existing-version egg version (directory tagdir)) ) ] )
      (if tagver
          (values (make-pathname tagdir tagver) tagver)
          (let ([trunkdir (make-pathname eggdir "trunk")])
            (when-no-such-version-warning egg version)
            (if (and (file-exists? trunkdir) (directory? trunkdir))
                (values trunkdir "trunk")
                (values eggdir "") ) ) ) ) )

  (define (gather-egg-information dir)
    (let ((ls (directory dir)))
      (filter-map
       (lambda (egg)
	 (let-values (((loc version) (locate-egg/local egg dir)))
	   (let ((meta (make-pathname loc egg "meta")))
	     (and (file-exists? meta)
		  (call/cc
		   (lambda (return)
		     (cons (string->symbol egg) 
			   (cons (list 'version version)
				 (handle-exceptions ex
				     (begin
				       (warning "extension has syntactically invalid .meta file" egg)
				       (return #f))
				   (with-input-from-file meta read))))))))))
       ls)))

  (define (make-svn-ls-cmd uarg parg pnam #!key recursive?)
    (conc "svn ls " uarg #\space parg (if recursive? " -R " " ") (qs pnam)) )

  (define (make-svn-export-cmd uarg parg dir tmpdir)
    (conc "svn export " uarg #\space parg #\space #\" dir #\" #\space #\" tmpdir #\"
          (if *quiet* " 1>&2" "")) )

  (define (list-eggs/svn repo #!optional username password)
    (let ([uarg (if username (string-append "--username='" username "'") "")]
          [parg (if password (string-append "--password='" password "'") "")])
      (let ([cmd (make-svn-ls-cmd uarg parg repo)])
        (d "listing extension directory ...~%  ~a~%" cmd)
        (string-concatenate
         (map (lambda (s) (string-append (string-chomp s "/") "\n"))
              (with-input-from-pipe cmd read-lines))) ) ) )

  (define (locate-egg/svn egg repo #!optional version destination username  password)
    (let* ([uarg (if username (string-append "--username='" username "'") "")]
	   [parg (if password (string-append "--password='" password "'") "")]
	   [cmd (make-svn-ls-cmd uarg parg (make-pathname repo egg) recursive?: #t)])
      (d "checking available versions ...~%  ~a~%" cmd)
      (let* ([files (with-input-from-pipe cmd read-lines)]
	     [tagver (existing-version
	              egg version
	              (filter-map
                       (lambda (f) (and-let* ((m (string-search "^tags/([^/]+)/" f))) (cadr m)))
                       files))])
	(let-values ([(filedir ver)
	              (if tagver
                          (values (string-append "tags/" tagver) tagver)
                          (begin
                            (when-no-such-version-warning egg version)
                            (if (member "trunk/" files)
                                (values "trunk" "trunk")
                                (values "" "") ) ) ) ] )
          (let* ([tmpdir (make-pathname (or destination (get-temporary-directory)) egg)]
                 [cmd (make-svn-export-cmd uarg parg (conc repo #\/ egg #\/ filedir) tmpdir)])
	    (d "  ~a~%" cmd)
	    (if (zero? (system cmd))
                (values tmpdir ver)
	        (values #f "") ) ) ) ) ) )

  (define (deconstruct-url url)
    (let ([m (string-match "(http://)?([^/:]+)(:([^:/]+))?(/.+)" url)])
      (values
       (if m (caddr m) url)
       (if (and m (cadddr m))
	   (or (string->number (list-ref m 4))
	       (error "not a valid port" (list-ref m 4)))
	   80)
       (if m (list-ref m 5) "/")) ) )

  (define (locate-egg/http egg url #!optional version destination tests)
    (let ([tmpdir (or destination (get-temporary-directory))])
      (let-values ([(host port locn) (deconstruct-url url)])
	(let ([locn (string-append
		     locn
		     "?name=" egg
		     (if version (string-append "&version=" version) "")
		     (if tests "&tests=yes" ""))]
	      [eggdir (make-pathname tmpdir egg) ] )
	  (unless (file-exists? eggdir) (create-directory eggdir))
	  (http-fetch host port locn eggdir)
	  ; If we get here then version of egg exists
	  (values eggdir (or version "")) ) ) ) )

  (define (network-failure msg . args)
    (signal
     (make-composite-condition
      (make-property-condition
       'exn
       'message "invalid response from server"
       'arguments args)
      (make-property-condition 'http-fetch))) )

  (define (make-HTTP-GET/1.1 location user-agent host
                             #!key
                             (port 80)
                             (connection "close")
                             (accept "*")
                             (content-length 0))
    (conc
     "GET " location " HTTP/1.1" "\r\n"
     "Connection: " connection "\r\n"
     "User-Agent: " user-agent "\r\n"
     "Accept: " accept "\r\n"
     "Host: " host #\: port "\r\n"
     "Content-length: " content-length "\r\n"
     "\r\n") )

  (define (match-http-response rsp)
    (and (string? rsp)
         (string-match "HTTP/[0-9.]+\\s+([0-9]+)\\s+.*" rsp)) )

  (define (response-match-code? mrsp code)
    (and mrsp (string=? (number->string code) (cadr mrsp))) )

  (define (match-chunked-transfer-encoding ln)
    (string-match "[Tt]ransfer-[Ee]ncoding:\\s*chunked.*" ln) )

  (define (http-fetch host port locn dest)
    (d "connecting to host ~s, port ~a ...~%" host port)
    (let-values ([(in out) (tcp-connect host port)])
      (d "requesting ~s ...~%" locn)
      (display
       (make-HTTP-GET/1.1 locn *chicken-install-user-agent* host port: port accept: "*/*")
       out)
      (flush-output out)
      (d "reading response ...~%")
      (let ([chunked #f])
	(let* ([h1 (read-line in)]
	       [response-match (match-http-response h1)])
	  (d "~a~%" h1)
	  ;;*** handle redirects here
	  (unless (response-match-code? response-match 200)
	    (network-failure "invalid response from server" h1) )
	  (let loop ()
	    (let ([ln (read-line in)])
	      (unless (string-null? ln)
		(when (match-chunked-transfer-encoding ln) (set! chunked #t))
		(d "~a~%" ln)
		(loop) ) ) ) )
	(when chunked
	  (d "reading chunks ...~%")
	  (let ([data (read-chunks in)])
	    (close-input-port in)
	    (set! in (open-input-string data))) ) )
      (d "reading files ...~%")
      (let get-files ([files '()])
        (let ([name (read in)])
	  (cond [(and (pair? name) (eq? 'error (car name)))
		 (apply error (string-append "[Server] " (cadr name)) (cddr name)) ]
		[(or (eof-object? name) (not name))
		 (close-input-port in)
		 (close-output-port out)
		 (reverse files) ]
		[(not (string? name))
		 (error "invalid file name - possibly corrupt transmission" name) ]
		[(string-suffix? "/" name)
		 (read in)		; skip size
		 (d "  ~a~%" name)
		 (create-directory (make-pathname dest name))
		 (get-files files) ]
		[else
		 (d "  ~a~%" name)
		 (let* ([size (read in)]
			[_ (read-line in)]
			[data (read-string size in)] )
		   (with-output-to-file (make-pathname dest name) (cut display data) ) )
		 (get-files (cons name files)) ] ) ) ) ) )

  (define (read-chunks in)
    (let get-chunks ([data '()])
      (let ([size (string->number (read-line in) 16)])
	(if (zero? size)
	    (string-concatenate-reverse data)
	    (let ([chunk (read-string size in)])
	      (read-line in)
	      (get-chunks (cons chunk data)) ) ) ) ) )

  (define (retrieve-extension name transport location
                              #!key version quiet destination username password tests)
    (fluid-let ([*quiet* quiet])
      (case transport
	[(local)
	 (when destination (warning "destination for transport `local' ignored"))
	 (locate-egg/local name location version destination) ]
	[(svn)
	 (locate-egg/svn name location version destination username password) ]
	[(http)
	 (locate-egg/http name location version destination tests) ]
	[else
	 (error "cannot retrieve extension unsupported transport" transport) ] ) ) )

  (define (list-extensions transport location #!key quiet username password)
    (fluid-let ([*quiet* quiet])
      (case transport
	[(local)
	 (list-eggs/local location) ]
	[(svn)
	 (list-eggs/svn location username password) ]
	[else
	 (error "cannot list extensions - unsupported transport" transport) ] ) ) )

) ;module setup-download
