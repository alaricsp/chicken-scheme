#!/bin/sh
#| wwchicken.scm -*- hen -*-
exec csi -s $0 "$@"
|#

(use srfi-1 posix srfi-37 utils)

(define output-path "site")
(define indexfile "index.html")

(define (sxml->xml sxml . port)
  (let ([port (:optional port (current-output-port))])
    (let rec ([sxml sxml])
      (match sxml
	[(tag ('@ . attrs) . data)
	 (fprintf port "<~A" tag)
	 (for-each
	  (match-lambda 
	    [(name val) (fprintf port " ~A=\"~A\"" name (->string val))]
	    [(name) (fprintf port " ~A=\"~A\"" name name)]
	    [a (error "invalid SXML attribute syntax" a)] ) 
	  attrs) 
	 (if (null? data)
	     (display " />" port)
	     (begin
	       (write-char #\> port)
	       (for-each rec data)
	       (fprintf port "</~A>~%" tag) ) ) ]
	[(tag . data) (rec `(,tag (@) . ,data))]
	[_ (display (with-output-to-string (cut display sxml)) port)] ) ) ) )

(define categories
  '((lang-exts "Language extensions")
    (graphics "Graphics")
    (debugging "Debugging tools")
    (logic "Logic programming")
    (net "Networking")
    (io "Input/Output")
    (db "Databases")
    (ffi "Interfacing to other languages")
    (web "Web programing")
    (xml "XML processing")
    (doc-tools "Documentation tools")
    (math "Mathematical libraries")
    (oop "Object-oriented programming")
    (data "Algorithms and data-structures")
    (parsing "Data formats and parsing")
    (tools "Tools")
    (testing "Unit-testing")
    (crypt "Cryptography")
    (ui "User interface toolkits")
    (code-generation "Run-time code generation")
    (macros "Macros and meta-syntax")
    (misc "Miscellaneous") ) )

(define style
  "body { 
      background-color: #fff;
      color: #000;
      margin-top: 40px;
      margin-bottom: 10px;
      margin-left: 40px;
      margin-right: 40px;
    }
    table.main { 
      margin-left: 40px;
    }
    table.news { 
      margin-left: 40px;
    }
    td { 
      font-family: arial, sans-serif;
      font-size: 80%;
    }
    td.grey { 
      background-color: #ddd;
    }
    td.first { 
      background-color: #cff;
      font-size: 100%;
    }
    td.newsdate { 
      font-weight: bold;
    }
    td.item { 
      font-weight: bold;
    }
    h2 { 
      font-family: arial, sans-serif;
      font-size: 120%;
      font-weight: bold;
    }
    h2.news { 
      margin-left: 30px;
      margin-bottom: 5px;
      font-size: 100%;
    }
    p.small { 
      font-size: 90%;
      font-style: italic;
    }
    tt { 
      font-weight: bold;
    }
    ul { 
      list-style-type: square;
    }
    a { 
      color: #09f;
      text-decoration: none;
    }
    a:visited { 
      color: #0c9;
    }
    p { 
      font-family: arial, sans-serif;
    }")

(define snapshot
  (first (sort (glob (make-pathname output-path "chicken-*.tar.gz")) string>?)) )

(define (make-chicken-page)
  `(html
    (head
     (style ,style)
     (title "The CHICKEN Scheme Compiler - call-with-current-continuation.org") )
    (body
     (img (@ (style "clear: both; margin-left: 180px") (src "chicken-title.png")))
     (img (@ (align "right") (style "margin-top: 20px") (src "chicken3.png")))
     (p "CHICKEN is a compiler for the " (a (@ (href "http://schemers.org/")) "Scheme")
	" programming language.
         CHICKEN produces portable, efficient C, supports almost all of the current
         Scheme language standard, the Revised" (sup "5") " Report on the Algorithmic Language Scheme ("
	(a (@ (href "http://schemers.org/Documents/Standards/R5RS/HTML/")) "R5RS") "), 
         and includes many enhancements and extensions.  CHICKEN runs on MacOS X,
         Windows, and many Unix flavours.")
     (img (@ (style "clear: right; margin-top: 20px; margin-left: 40px") (src "checkerline2.png")))
     (table (@ (width 850) (class main) (cellspacing 4) (cellpadding 4))
	    (tr (@ (valign top))
		(td (@ (class "first") (colspan 2)) 
		    "Latest release: &nbsp;&nbsp;&nbsp;" (b "CHICKEN 2.3 ")
		    (a (@ (href "http://www.call-with-current-continuation.org/chicken-2.3.tar.gz"))
		       " tar.gz")
		    (a (@ (href "http://www.call-with-current-continuation.org/chicken-2.3.zip"))
		       " zip")
		    (a (@ (href "http://www.call-with-current-continuation.org/chicken-2.3-win32.zip"))
		       "Windows binaries (2.3)")
		    (br)
		    "Latest snapshot: " 
		    (b ,(string-append "CHICKEN " (second (string-match "chicken-([0-9.]+)\\..*" (pathname-file snapshot)))))
		    (a (@ (href ,(sprintf "http://www.call-with-current-continuation.org/~A"
					  (pathname-strip-directory snapshot)) ) )
		       " tar.gz")
		    (a (@ (href "http://www.call-with-current-continuation.org/ChangeLog.txt"))
		       " ChangeLog")))
	    (tr (@ (valign top))
		(td (@ (width "50%") (class grey)) (h2 "Features")
		    (ul (li "Compiles Scheme to efficient and portable C")
			(li "Includes a full-featured interactive interpreter as well as an optimising batch compiler")
			(li "Full support for tail recursion, first-class continuations, high-level macros
                     and " (tt "dynamic-wind"))
			(li "Highly portable and known to run on many platforms, including IA-32 (80x86),
                     IA-64, PowerPC, SPARC and UltraSPARC, Alpha, MIPS, PA-RISC, ARM, AMD64 and S/390")
			(li "Distributed free for use and modification under the terms of the BSD License")
			(li "Transparent support for shared objects and linkage to C")
			(li "A sophisticated but easy-to-use Foreign Function Interface for linking C and C++
                     libraries and classes to Scheme")
			(li "Lightweight threads based on first-class continuations")
			(li "An object system with multiple dispatch, multiple inheritance and a meta-object protocol")
			(li "Provides the " (tt "syntax-case") " enhanced high-level macro system (including R5RS " (tt "syntax-rules") 
			    ") &deg;, and " (tt "define-macro") "style low-level macros")
			(li "Support for syntactic pattern-matching via Andrew Wright's " (tt "match") " package")
			(li "Execution profiling, debugging and backtrace support")
			(li "A clean POSIX interface that covers environment and filesystem access, pipes, processes,
                     signals, locks, sockets, and low-level and memory-mapped I/O")
			(li "Support for interpreted or compiled shell scripts under Unix and Windows")
			(li "CHICKEN is supported by " (a (@ (href "http://www.swig.org")) "SWIG") " so interfacing to C or C++ is made even easier.")
			(li "Support for a large number of " (a (@ (href "http://srfi.schemers.org/")) "Scheme Requests
                     For Implementation") " (SRFIs), some available separately as extensions:"
                     (ul (li "SRFI &nbsp;0 (" (tt "cond-expand") ")")
                         (li "SRFI &nbsp;1 (List library)")
                         (li "SRFI &nbsp;2 (" (tt "and-let*") ")")
                         (li "SRFI &nbsp;4 (Homogeneous numeric vector datatypes)")
                         (li "SRFI &nbsp;6 (Basic string ports)")
                         (li "SRFI &nbsp;8 (" (tt "receive") ")")
                         (li "SRFI &nbsp;9 (Record types)")
                         (li "SRFI 10 (" (tt "#,") " external form)")
                         (li "SRFI 11 (Syntax for receiving multiple values)")
			 (li (i "SRFI-12 (Exceptions)"))
                         (li "SRFI 13 (String library)")
                         (li "SRFI 14 (Character set library)")
                         (li (i "SRFI 15 (" (tt "fluid-let") ")"))
                         (li "SRFI 16 (" (tt "case-lambda") ")")
                         (li "SRFI 17 (Generalized " (tt "set!") ")")
                         (li "SRFI 18 (Multithreading support)")
                         (li "SRFI 19 (Time Data Types and Procedures) &deg;")
                         (li "SRFI 23 (" (tt "error") ")")
                         (li "SRFI 25 (Multidimensional array primitives) &deg;")
                         (li "SRFI 26 (" (tt "cut") ", " (tt "cute") ")")
			 (li "SRFI 27 (Source of random bits) &deg;")
                         (li "SRFI 28 (" (tt "format") ")")
                         (li "SRFI 29 (Localization) &deg;")
                         (li "SRFI 30 (Block comments)")
                         (li "SRFI 31 (A special form " (tt "rec") " for recursive evaluation)")
                         (li "SRFI 37 (Program argument processor) &deg;")
                         (li "SRFI 38 (External representation for data with shared structure) &deg;")
                         (li "SRFI 39 (Parameters)")
                         (li "SRFI 40 (Stream library) &deg;")
                         (li "SRFI 42 (Eager comprehensions) &deg;")
                         (li "SRFI 43 (Vector library) &deg;")
			 (li "SRFI 45 (Primitives for Expressing Iterative Lazy Algorithms) &deg;")
                         (li "SRFI 47 (Arrays) &deg;")
			 (li "SRFI 55 (" (tt "require-extension") ")")
			 (li "SRFI 57 (Records) &deg;") 
			 (li "SRFI 60 (Integers as bits) &deg;")
			 (li "SRFI 61 (a more general " (tt "cond") " clause)")
			 (li "SRFI 62 (S-Expression comments)")
			 (li "SRFI 63 (Homogeneous and Heterogeneous Arrays) &deg;")
			 (li "SRFI 66 (Octet vectors) &deg;")
			 (li "SRFI 69 (Basic hash tables)")
			 (li "SRFI 72 (Hygienic macros) &deg;")) ) )
		    (p (@ (align "right")) "&deg; available separately")
		    (p (b "Limitations")
		    (ul (@ (style "margin-top: 5px"))
			(li "No unlimited-precision integers (bignums), rationals or complex numbers (but for some platforms"
			    " an extension library for extended numeric types is available)")
			(li "Procedures are by default limited to 126 arguments (on certain platforms CHICKEN is able to use "
			    (code libffi) "to raise the limit to 1000")
			(li "Compilation of large files can be slow"))))
		(td (h2 "Documentation")
		    (img (@ (width 325) (height 1)))
		    (p "CHICKEN's design is inspired by the paper "
		       (a (@ (href "http://home.pipeline.com/~hbaker1/CheneyMTA.html"))
			  (i "CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A."))
		       "by Henry Baker.")

		    (p (b "CHICKEN ") (a (@ (href "http://www.call-with-current-continuation.org/FAQ.html"))
					 "Frequently Asked Questions"))
		    (p (b "CHICKEN User's Manual: ") "&nbsp;&nbsp;&nbsp;&nbsp;"
		       (a (@ (href "http://www.call-with-current-continuation.org/chicken-manual-html.zip"))
			  "html.zip")
		       "&nbsp;&nbsp;"
		       (a (@ (href "http://www.call-with-current-continuation.org/chicken.pdf")) "pdf"))
		    (p (a (@ (href "manual/index.html"))
			  "Contents"))
		    ;; this should be generated automatically...
		    (ol (li (a (@ (href "manual/Introduction.html"))
			       "Introduction"))
			(li (a (@ (href "manual/Basic-mode-of-operation.html"))
			       "Basic mode of operation"))
			(li (a (@ (href "manual/Using-the-compiler.html"))
			       "Using the compiler")
			    (ol (li (a (@ (href "manual/Compiler-command-line-format.html"))
				       "Command line format"))
				(li (a (@ (href "manual/Runtime-options.html"))
				       "Runtime options"))
				(li (a (@ (href "manual/An-example.html"))
				       "An example"))
				(li (a (@ (href "manual/Extending-the-compiler.html"))
				       "Extending the compiler"))
				(li (a (@ (href "manual/Distributing-compiled-C-files.html"))
				       "Distributing compiled C files"))))
			(li (a (@ (href "manual/Using-the-interpreter.html"))
			       "Using the interpreter")
			    (ol (li (a (@ (href "manual/Interpreter-command-line-format.html"))
				       "Command line format"))
				(li (a (@ (href "manual/Writing-Scheme-scripts.html"))
				       "Writing Scheme scripts"))
				(li (a (@ (href "manual/Toplevel-commands.html"))
				       "Toplevel commands"))
				(li (a (@ (href "manual/Macros-and-procedures-implemented-in-the-interpreter.html"))
				       "Macros and procedures implemented in the interpreter"))))
			(li (a (@ (href "manual/Supported-language.html"))
			       "Supported language")
			    (ol (li (a (@ (href "manual/Deviations-from-the-standard.html"))
				       "Deviations from the standard"))
				(li (a (@ (href "manual/Extensions-to-the-standard.html"))
				       "Extensions to the standard"))
				(li (a (@ (href "manual/Non-standard-read-syntax.html"))
				       "Non-standard read syntax"))
				(li (a (@ (href "manual/Non-standard-macros-and-special-forms.html"))
				       "Non-standard macros and special forms"))
				(li (a (@ (href "manual/Declarations.html"))
				       "Declarations"))
				(li (a (@ (href "manual/Parameters.html"))
				       "Parameters"))
				(li "Units" (br)
                                    (table (@ (cellspacing 0) (cellpadding 1) (border 0))
                                      (tr
				    (td (a (@ (href "manual/Unit-eval.html"))
				       " eval"))
				    (td (a (@ (href "manual/Unit-extras.html"))
				       " extras")))
                                      (tr
				    (td (a (@ (href "manual/Unit-library.html"))
				       " library"))
				    (td (a (@ (href "manual/Unit-lolevel.html"))
				       " lolevel"))
				    (td (a (@ (href "manual/Unit-match.html"))
				       " match")))
                                      (tr
				    (td (a (@ (href "manual/Unit-posix.html"))
				       " posix"))
				    (td (a (@ (href "manual/Unit-regex.html"))
				       " regex"))
				    (td (a (@ (href "manual/Unit-utils.html"))
				       " script-utils")))
                                      (tr
				    (td (a (@ (href "manual/Unit-srfi-1.html"))
				       " srfi-1"))
				    (td (a (@ (href "manual/Unit-srfi-4.html"))
				       " srfi-4"))
				    (td (a (@ (href "manual/Unit-srfi-13.html"))
				       " srfi-13")))
                                      (tr
				    (td (a (@ (href "manual/Unit-srfi-14.html"))
				       " srfi-14"))
				    (td (a (@ (href "manual/Unit-srfi-18.html"))
				       " srfi-18")))
                                      (tr
				    (td (a (@ (href "manual/Unit-tcp.html"))
				       " tcp")))
                                      (tr
				    (td (a (@ (href "manual/Unit-tinyclos.html"))
				       " tinyclos")))) )))
			(li (a (@ (href "manual/Interface-to-external-functions-and-variables.html"))
			       "Interface to external functions and variables")
			    (ol (li (a (@ (href "manual/Accessing-external-objects.html"))
				       "Accessing external objects"))
				(li (a (@ (href "manual/Foreign-type-specifiers.html"))
				       "Foreign type specifiers"))
				(li (a (@ (href "manual/Callbacks.html"))
				       "Callbacks"))
				(li (a (@ (href "manual/Locations.html"))
				       "Locations"))
				(li (a (@ (href "manual/Other-support-procedures.html"))
				       "Other support procedures"))
				(li (a (@ (href "manual/The-Easy-Foreign-Function-Interface.html"))
				       "The Easy Foreign Function Interface"))
				(li (a (@ (href "manual/C-interface.html"))
				       "C interface"))))
			(li (a (@ (href "manual/Additional-files.html"))
			       "Additional files"))
			(li (a (@ (href "manual/chicken-setup.html"))
			       "chicken-setup") )
			(li (a (@ (href "manual/Data-Representation.html"))
			       "Data representation"))
			(li (a (@ (href "manual/Bugs-and-limitations.html"))
			       "Bugs and limitations"))
			(li (a (@ (href "manual/FAQ.html"))
			       "FAQ"))
			(li (a (@ (href "manual/Acknowledgements.html"))
			       "Acknowledgements")))
		    (ul (li (a (@ (href "manual/Bibliography.html"))
			       "Bibliography"))
			(li (a (@ (href "manual/Index.html"))
			       "Index")))))
	    (tr (@ (valign top))
		(td (h2 "Resources")
		    (table (@ (border 0))
			   (tr (@ (valign top))
			       (td (@ (class "item") (width "25%")) "Project")
			       (td "The " (a (@ (href "http://savannah.gnu.org/projects/chicken"))
					     "CHICKEN project")
				   " is hosted by "
				   (a (@ (href "http://savannah.gnu.org/"))
				      "http://savannah.gnu.org")
				   "."))
			   (tr (@ (valign top))
			       (td (@ (class "item")) "Mailing List")
			       (td "The " (a (@ (href "http://mail.nongnu.org/mailman/listinfo/chicken-users"))
					     "CHICKEN Users")
				   " mailing list is a low-volume but fast-response list for discussing all
                     things related to CHICKEN.  You can send a message to the list by addressing
                     it to " (a (@ (href "mailto:chicken-users@nongnu.org"))
				(tt "chicken-users@nongnu.org"))
				   "."))
			   (tr (@ (valign top))
			       (td (@ (class "item")) "Development repository")
			       (td "The current CHICKEN development version can be accessed via the"
				   (a (@ (href "http://www.darcs.net")) "darcs") 
				   "revision control system, like this:"
				   (br) (br)
				   (tt (@ (style "font-size: 90%")) "$ darcs get http://galinha.ucpel.tche.br/chicken")
				   (br) (br)
				   "A " (a (@ (href "http://subversion.tigris.org")) "subversion")
				   " repository is also available at:"
				   (br) (a (@ (href "https://galinha.ucpel.tche.br/svn/chicken-eggs/chicken"))
					   "https://galinha.ucpel.tche.br/svn/chicken-eggs/chicken")
				   " which can be accessed like this:"
				   (br) (br)
				   (tt "$ svn co https://galinha.ucpel.tche.br/svn/chicken-eggs/chicken")
				   (br) (br) "(username: " (tt "anonymous") ", password: &lt;none&gt;)") (br)
				   "See the file " (tt "README.darcs") " for details on how to bootstrap the system.")
			   (tr (@ (valign top))
			       (td (@ (class "item")) "Other releases")
			       (td "Thanks to Davide Puricelli, a "
				   (a (@ (href "http://packages.debian.org/testing/interpreters/chicken-bin"))
				      "Debian package")
				   " is also available."))
			   (tr (@ (valign top))
			       (td (@ (class "item")) "Software")
			       (td "A collection of " (a (@ (href "http://www.call-with-current-continuation.org/software.html"))
							 "software")
				   " and links to code is available for CHICKEN."))
			   (tr (@ (valign top))
			       (td (@ (class "item")) "Wiki")
			       (td "There is a wiki for all thing related to CHICKEN at the"
				   (a (@ (href "http://galinha.ucpel.tche.br/coop"))
				      " chicken-coop") ) )
			   (tr (@ (valign top))
			       (td (@ (class "item")) "IRC")
			       (td "Join the #chicken IRC channel at irc.freenode.net (port 6667)") )
			   (tr (@ (valign top))
			       (td (@ (class "item")) "Questions and comments")
			       (td "If you have any questions, suggestions or insults regarding CHICKEN,
                     don't hesitate to send an e-mail to the author: "
				   (a (@ (href "mailto:felix@call-with-current-continuation.org"))
				      "felix@call-with-current-continuation.org") "."
				   (br) (img (@ (style "margin-top: 40px; margin-left: 40px") (src "alice-rabbit1.jpg"))) ))))
		(td (@ (class grey))
		    (h2 "Extensions and libraries")
		    ,@(append-map 
		       (lambda (cat)
			 `((br) (a (@ (href ,(sprintf "eggs/index.html#~A" (car cat)))) ,(cadr cat))))
		       categories) 
		    (br)
		    (a (@ (href "eggs/index.html")) (p "more information...")) ) ) )
     (center
      (table 
       (@ (border "0") (cellpadding "25"))
       (tr
	(td
	 (center
	  (font (@ (size "1")) "Find all about"
		(br)
		(a (@ (href "http://scheme-programming-language.wikiverse.org/"))
		   (font (@ (size "2")) "Scheme programming language")))
	  (br)
	  (a (@ (href "http://scheme-programming-language.wikiverse.org/"))
	     (img (@ (src "wikiverse.gif") (alt "Find all about Scheme programming language") (width "78") (height "110")
		     (border "0") ) ) ) ) )
	(td 
	 (center
	  (a (@ (href "http://www.schemers.org")) (font (@ (size "2")) "schemers.org")) 
	  (br)
	  (a (@ (href "http://www.schemers.org"))
	     (img (@ (src "logo-dsandler.gif") (alt "www.schemers.org") (border "0"))) ) ) )
	(td
	 (center 
	  (a (@ (href "http://www.bloodandcoffee.net/campbell/html/scheme-irc.html")) 
	     (font (@ (size "2")) "scheme IRC") )
	  (br)
	  (a (@ (href "http://www.bloodandcoffee.net/campbell/html/scheme-irc.html"))
	     (h1 (tt "#scheme")) ) ) )
	(td
	 (center
	  (a (@ (href "http://community.schemewiki.org"))
	     (font (@ (size "2")) "The Scheme Wiki")))))))
     (img (@ (style "margin-top: 40px; margin-left: 40px") (src "checkerline2.png")))
     (br) 
     (br)
     (p (@ (align right) (style "margin-bottom: 0px; font-size: 70%"))
	,(sprintf "Last modified: ~A" (with-input-from-pipe "date" read-line)) )
     (hr (@ (style "margin: 0px")))
     (p (@ (align right) (style "margin-top: 0px; font-size: 80%"))
	"Copyright &copy; 2004 Felix L. Winkelmann" (br)
	(span (@ (style "font-size: 70%")) "Page design by Category 5") ) ) ) )

(define options
  (list (option '(#\h "help") #f #f (lambda _ (print-usage) (exit)))
	(option '(#\o "output-path") #t #f (lambda (_ _ arg s) (set! output-path arg) s)) ) )

(define (print-usage)
  (display #<<EOF
Usage: csi -s wwchicken.scm [OPTION ...]

  -h  --help                 show this text
  -o  --output-path PATH     set output path for generated files (defaults to ./site)

EOF
) 
  (exit) )

(define (main args)
  (let ([file (args-fold 
	       args
	       options
	       (lambda (_ n . _) (error "invalid option" n))
	       cons
	       '() ) ] )
    (print "generating " indexfile " ...")
    (with-output-to-file (make-pathname output-path indexfile)
      (cut sxml->xml (make-chicken-page) ) )
    0) )

(main (command-line-arguments))
