;;;; tcp.scm - Networking stuff
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
  (unit tcp)
  (uses extras scheduler)
  (usual-integrations)
  (fixnum-arithmetic)
  (no-bound-checks)
  (export tcp-close tcp-listen tcp-connect tcp-accept tcp-accept-ready? ##sys#tcp-port->fileno tcp-listener? tcp-addresses
	  tcp-abandon-port tcp-listener-port tcp-listener-fileno tcp-port-numbers tcp-buffer-size)
  (no-procedure-checks-for-usual-bindings)
  (bound-to-procedure
   ##net#socket ##net#bind ##net#connect ##net#listen ##net#accept make-parameter ##sys#string-append ##sys#tcp-port->fileno
   ##sys#check-port ##sys#port-data ##sys#thread-block-for-i/o! make-string make-input-port make-output-port ##sys#substring
   substring ##sys#make-c-string ##sys#schedule
   ##net#close ##net#recv ##net#send ##net#select ##net#select-write ##net#gethostaddr ##net#io-ports ##sys#update-errno
   ##sys#error ##sys#signal-hook ##net#getservbyname ##net#parse-host ##net#fresh-addr
   ##net#bind-socket ##net#shutdown)
  (foreign-declare #<<EOF
#include <errno.h>
#ifdef _WIN32
# if _MSC_VER > 1300
# include <winsock2.h>
# include <ws2tcpip.h>
# else
# include <winsock.h>
# endif
/* Beware: winsock2.h must come BEFORE windows.h */
# define socklen_t       int
static WSADATA wsa;
# define fcntl(a, b, c)  0
# define EWOULDBLOCK     0
# define EINPROGRESS     0
# define typecorrect_getsockopt(socket, level, optname, optval, optlen)	\
    getsockopt(socket, level, optname, (char *)optval, optlen)
#else
# include <fcntl.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <netinet/in.h>
# include <unistd.h>
# include <netdb.h>
# include <signal.h>
# define closesocket     close
# define INVALID_SOCKET  -1
# define typecorrect_getsockopt getsockopt
#endif

#ifndef SD_RECEIVE
# define SD_RECEIVE      0
# define SD_SEND         1
#endif

#ifdef ECOS
#include <sys/sockio.h>
#endif

static char addr_buffer[ 20 ];
EOF
) )

(register-feature! 'tcp)

(cond-expand
 (unsafe
  (eval-when (compile)
    (define-macro (##sys#check-structure x y . _) '(##core#undefined))
    (define-macro (##sys#check-range x y z) '(##core#undefined))
    (define-macro (##sys#check-pair x) '(##core#undefined))
    (define-macro (##sys#check-list x) '(##core#undefined))
    (define-macro (##sys#check-symbol x) '(##core#undefined))
    (define-macro (##sys#check-string x) '(##core#undefined))
    (define-macro (##sys#check-char x) '(##core#undefined))
    (define-macro (##sys#check-exact x) '(##core#undefined))
    (define-macro (##sys#check-port x) '(##core#undefined))
    (define-macro (##sys#check-number x) '(##core#undefined))
    (define-macro (##sys#check-byte-vector x) '(##core#undefined)) ) )
 (else
  (declare (emit-exports "tcp.exports"))) )

(define-foreign-variable errno int "errno")
(define-foreign-variable strerror c-string "strerror(errno)")

(define-foreign-type sockaddr* (pointer "struct sockaddr"))
(define-foreign-type sockaddr_in* (pointer "struct sockaddr_in"))

(define-foreign-variable _af_inet int "AF_INET")
(define-foreign-variable _sock_stream int "SOCK_STREAM")
(define-foreign-variable _sock_dgram int "SOCK_DGRAM")
(define-foreign-variable _sockaddr_size int "sizeof(struct sockaddr)")
(define-foreign-variable _sockaddr_in_size int "sizeof(struct sockaddr_in)")
(define-foreign-variable _sd_receive int "SD_RECEIVE")
(define-foreign-variable _sd_send int "SD_SEND")
(define-foreign-variable _ipproto_tcp int "IPPROTO_TCP")
(define-foreign-variable _invalid_socket int "INVALID_SOCKET")
(define-foreign-variable _ewouldblock int "EWOULDBLOCK")
(define-foreign-variable _einprogress int "EINPROGRESS")

(define ##net#socket (foreign-lambda int "socket" int int int))
(define ##net#bind (foreign-lambda int "bind" int scheme-pointer int))
(define ##net#listen (foreign-lambda int "listen" int int))
(define ##net#accept (foreign-lambda int "accept" int c-pointer c-pointer))
(define ##net#close (foreign-lambda int "closesocket" int))
(define ##net#send (foreign-lambda int "send" int scheme-pointer int int))
(define ##net#recv (foreign-lambda int "recv" int scheme-pointer int int))
(define ##net#shutdown (foreign-lambda int "shutdown" int int))
(define ##net#connect (foreign-lambda int "connect" int scheme-pointer int))

(define ##net#make-nonblocking
  (foreign-lambda* bool ((int fd))
    "int val = fcntl(fd, F_GETFL, 0);"
    "if(val == -1) return(0);"
    "return(fcntl(fd, F_SETFL, val | O_NONBLOCK) != -1);") )

(define ##net#getsockname 
  (foreign-lambda* c-string ((int s))
    "struct sockaddr_in sa;"
    "unsigned char *ptr;"
    "int len = sizeof(struct sockaddr_in);"
    "if(getsockname(s, (struct sockaddr *)&sa, (socklen_t *)&len) != 0) return(NULL);"
    "ptr = (unsigned char *)&sa.sin_addr;"
    "sprintf(addr_buffer, \"%d.%d.%d.%d\", ptr[ 0 ], ptr[ 1 ], ptr[ 2 ], ptr[ 3 ]);"
    "return(addr_buffer);") )

(define ##net#getsockport
  (foreign-lambda* int ((int s))
    "struct sockaddr_in sa;"
    "int len = sizeof(struct sockaddr_in);"
    "if(getsockname(s, (struct sockaddr *)&sa, (socklen_t *)(&len)) != 0) return(-1);"
    "else return(ntohs(sa.sin_port));") )

(define ##net#getpeerport
 (foreign-lambda* int ((int s))
   "struct sockaddr_in sa;"
   "int len = sizeof(struct sockaddr_in);"
   "if(getpeername(s, (struct sockaddr *)&sa, (socklen_t *)(&len)) != 0) return(-1);"
   "else return(ntohs(sa.sin_port));") )

(define ##net#getpeername 
  (foreign-lambda* c-string ((int s))
    "struct sockaddr_in sa;"
    "unsigned char *ptr;"
    "unsigned int len = sizeof(struct sockaddr_in);"
    "if(getpeername(s, (struct sockaddr *)&sa, ((unsigned int *)&len)) != 0) return(NULL);"
    "ptr = (unsigned char *)&sa.sin_addr;"
    "sprintf(addr_buffer, \"%d.%d.%d.%d\", ptr[ 0 ], ptr[ 1 ], ptr[ 2 ], ptr[ 3 ]);"
    "return(addr_buffer);") )

(define ##net#startup
  (foreign-lambda* bool () #<<EOF
#ifdef _WIN32
     return(WSAStartup(MAKEWORD(1, 1), &wsa) == 0);
#else
     signal(SIGPIPE, SIG_IGN);
     return(1);
#endif
EOF
) )

(unless (##net#startup)
  (##sys#signal-hook #:network-error "can not initialize Winsock") )

(define ##net#getservbyname 
  (foreign-lambda* int ((c-string serv) (c-string proto))
    "struct servent *se;
     if((se = getservbyname(serv, proto)) == NULL) return(0);
     else return(ntohs(se->s_port));") )     

(define ##net#select
  (foreign-lambda* int ((int fd))
    "fd_set in;
     struct timeval tm;
     int rv;
     FD_ZERO(&in);
     FD_SET(fd, &in);
     tm.tv_sec = tm.tv_usec = 0;
     rv = select(fd + 1, &in, NULL, NULL, &tm);
     if(rv > 0) { rv = FD_ISSET(fd, &in) ? 1 : 0; }
     return(rv);") )

(define ##net#select-write
  (foreign-lambda* int ((int fd))
    "fd_set out;
     struct timeval tm;
     int rv;
     FD_ZERO(&out);
     FD_SET(fd, &out);
     tm.tv_sec = tm.tv_usec = 0;
     rv = select(fd + 1, NULL, &out, NULL, &tm);
     if(rv > 0) { rv = FD_ISSET(fd, &out) ? 1 : 0; }
     return(rv);") )

(define ##net#gethostaddr
  (foreign-lambda* bool ((scheme-pointer saddr) (c-string host) (unsigned-short port))
    "struct hostent *he = gethostbyname(host);"
    "struct sockaddr_in *addr = (struct sockaddr_in *)saddr;"
    "if(he == NULL) return(0);"
    "memset(addr, 0, sizeof(struct sockaddr_in));"
    "addr->sin_family = AF_INET;"
    "addr->sin_port = htons((short)port);"
    "addr->sin_addr = *((struct in_addr *)he->h_addr);"
    "return(1);") )

(define (yield)
  (##sys#call-with-current-continuation
   (lambda (return)
     (let ((ct ##sys#current-thread))
       (##sys#setslot ct 1 (lambda () (return (##core#undefined))))
       (##sys#schedule) ) ) ) )

(define ##net#parse-host
  (let ((substring substring))
    (lambda (host proto)
      (let ((len (##sys#size host)))
	(let loop ((i 0))
	  (if (fx>= i len)
	      (values host #f)
	      (let ((c (##core#inline "C_subchar" host i)))
		(if (char=? c #\:)		    
		    (values
		     (substring host (add1 i) len)
		     (let* ((s (substring host 0 i))
			    (p (##net#getservbyname s proto)) )
		       (when (eq? 0 p)
			 (##sys#update-errno)
			 (##sys#signal-hook
			  #:network-error 'tcp-connect (##sys#string-append "can not compute port from service - " strerror)
			  s) )
		       p) )
		    (loop (fx+ i 1)) ) ) ) ) ) ) ) )

(define ##net#fresh-addr
  (foreign-lambda* void ((scheme-pointer saddr) (unsigned-short port))
    "struct sockaddr_in *addr = (struct sockaddr_in *)saddr;"
    "memset(addr, 0, sizeof(struct sockaddr_in));"
    "addr->sin_family = AF_INET;"
    "addr->sin_port = htons(port);"
    "addr->sin_addr.s_addr = htonl(INADDR_ANY);") )

(define (##net#bind-socket port style host)
  (##sys#check-exact port)
  (let ((s (##net#socket _af_inet style 0)))
    (when (eq? _invalid_socket s)
      (##sys#update-errno)
      (##sys#error "can not create socket") )
    ;; PLT makes this an optional arg to tcp-listen. Should we as well?
    (when (eq? -1 ((foreign-lambda* int ((int socket)) 
		     "int yes = 1; 
                      return(setsockopt(socket, SOL_SOCKET, SO_REUSEADDR, (const char *)&yes, sizeof(int)));") 
		   s) )
      (##sys#update-errno)
      (##sys#signal-hook #:network-error 'tcp-listen (##sys#string-append "error while setting up socket - " strerror) s) )
    (let ((addr (make-string _sockaddr_in_size)))
      (if host
	  (unless (##net#gethostaddr addr host port)
	    (##sys#signal-hook #:network-error 'tcp-listen "getting listener host IP failed - " host port) )
	  (##net#fresh-addr addr port) )
      (let ((b (##net#bind s addr _sockaddr_in_size)))
	(when (eq? -1 b)
	  (##sys#update-errno)
	  (##sys#signal-hook #:network-error 'tcp-listen (##sys#string-append "can not bind to socket - " strerror) s port) )
	(values s addr) ) ) ) )

(define-constant default-backlog 10)

(define (tcp-listen port . more)
  (let-optionals more ((w default-backlog) (host #f))
    (let-values (((s addr) (##net#bind-socket port _sock_stream host)))
      (##sys#check-exact w)
      (let ((l (##net#listen s w)))
	(when (eq? -1 l)
	  (##sys#update-errno)
	  (##sys#signal-hook #:network-error 'tcp-listen (##sys#string-append "can not listen on socket - " strerror) s port) )
	(##sys#make-structure 'tcp-listener s) ) ) ) )

(define (tcp-listener? x) 
  (and (##core#inline "C_blockp" x)
       (##sys#structure? x 'tcp-listener) ) )

(define (tcp-close tcpl)
  (##sys#check-structure tcpl 'tcp-listener)
  (let ((s (##sys#slot tcpl 1)))
    (when (fx= -1 (##net#close s))
      (##sys#update-errno)
      (##sys#signal-hook #:network-error 'tcp-close (##sys#string-append "can not close TCP socket - " strerror) tcpl) ) ) )

(define-constant +input-buffer-size+ 1024)
(define-constant +output-chunk-size+ 8192)

(define tcp-buffer-size (make-parameter #f))

(define ##net#io-ports
  (let ((make-input-port make-input-port)
	(make-output-port make-output-port) 
	(tbs tcp-buffer-size)
	(make-string make-string) )
    (lambda (fd)
      (unless (##net#make-nonblocking fd)
	(##sys#update-errno)
	(##sys#signal-hook #:network-error (##sys#string-append "can not create TCP ports - " strerror)) )
      (let* ((buf (make-string +input-buffer-size+))
	     (data (vector fd #f #f))
	     (buflen 0)
	     (bufindex 0)
	     (iclosed #f) 
	     (oclosed #f)
	     (outbufsize (tbs))
	     (outbuf (and outbufsize (fx> outbufsize 0) ""))
	     (read-input
	      (lambda ()
		(let loop ()
		  (let ((n (##net#recv fd buf +input-buffer-size+ 0)))
		    (cond ((eq? -1 n)
			   (cond ((eq? errno _ewouldblock) 
				  (##sys#thread-block-for-i/o! ##sys#current-thread fd #t)
				  (yield)
				  (loop) )
				 (else
				  (##sys#update-errno)
				  (##sys#signal-hook 
				   #:network-error
				   (##sys#string-append "can not read from socket - " strerror) 
				   fd) ) ) )
			  (else
			   (set! buflen n)
			   (set! bufindex 0) ) ) ) ) ) )
	     (in
	      (make-input-port
	       (lambda ()
		 (when (fx>= bufindex buflen)
		   (read-input))
		 (if (fx>= bufindex buflen)
		     #!eof
		     (let ((c (##core#inline "C_subchar" buf bufindex)))
		       (set! bufindex (fx+ bufindex 1))
		       c) ) )
	       (lambda ()
		 (or (fx< bufindex buflen)
		     (let ((f (##net#select fd)))
		       (when (eq? f -1)
			 (##sys#update-errno)
			 (##sys#signal-hook
			  #:network-error
			  (##sys#string-append "can not check socket for input - " strerror) 
			  fd) )
		       (eq? f 1) ) ) )
	       (lambda ()
		 (unless iclosed
		   (set! iclosed #t)
		   (unless (##sys#slot data 1) (##net#shutdown fd _sd_receive))
		   (when (and oclosed (eq? -1 (##net#close fd)))
		     (##sys#update-errno)
		     (##sys#signal-hook
		      #:network-error
		      (##sys#string-append "can not close socket input port - " strerror)
		      fd) ) ) )
	       #f
	       (lambda (p n dest start)	; read-string!
		 (let loop ((n n) (m 0) (start start))
		   (cond ((eq? n 0) m)
			 ((fx< bufindex buflen)
			  (let* ((rest (fx- buflen bufindex))
				 (n2 (if (fx< n rest) n rest)))
			    (##core#inline "C_substring_copy" buf dest bufindex (fx+ bufindex n2) start)
			    (set! bufindex (fx+ bufindex n2))
			    (loop (fx- n n2) (fx+ m n2) (fx+ start n2)) ) )
			 (else
			  (read-input)
			  (if (eq? buflen 0) 
			      m
			      (loop n m start) ) ) ) ) )
	       (lambda (p limit)	; read-line
		 (let loop ((str #f))
		   (cond ((fx< bufindex buflen)
			  (##sys#scan-buffer-line
			   buf 
			   buflen
			   bufindex
			   (lambda (pos2 next)
			     (let ((dest (##sys#make-string (fx- pos2 bufindex))))
			       (##core#inline "C_substring_copy" buf dest bufindex pos2 0)
			       (set! bufindex next)
			       (cond ((eq? pos2 next) ; no line-terminator encountered
				      (read-input)
				      (if (fx>= bufindex buflen)
					  (or str "")
					  (loop (if str (##sys#string-append str dest) dest)) ) )
				     (else 
				      (##sys#setislot p 4 (fx+ (##sys#slot p 4) 1))
				      (if str (##sys#string-append str dest) dest)) ) ) ) ) )
			 (else
			  (read-input)
			  (if (fx< bufindex buflen)
			      (loop str)
			      #!eof) ) ) ) ) ) )
	     (output
	      (lambda (s)
		(let loop ((len (##sys#size s)) 
			   (s s))
		  (let* ((count (fxmin +output-chunk-size+ len))
			 (n (##net#send fd s count 0)) )
		    (cond ((eq? -1 n)
			   (if (eq? errno _ewouldblock)
			       (begin
				 (##sys#thread-block-for-i/o! ##sys#current-thread fd #f)
				 (yield) 
				 (loop len s) )
			       (begin
				 (##sys#update-errno)
				 (##sys#signal-hook 
				  #:network-error (##sys#string-append "can not write to socket - " strerror) fd) ) ) )
			  ((fx< n len)
			   (loop (fx- len n) (##sys#substring s n (##sys#size s))) ) ) ) ) ) )
	     (out
	      (make-output-port
	       (if outbuf
		   (lambda (s)
		     (set! outbuf (##sys#string-append outbuf s))
		     (when (fx>= (##sys#size outbuf) outbufsize)
		       (output outbuf)
		       (set! outbuf "") ) )
		   (lambda (s) 
		     (when (fx> (##sys#size s) 0)
		       (output s)) ) )
	       (lambda ()
		 (unless oclosed
		   (set! oclosed #t)
		   (when (and outbuf (fx> (##sys#size outbuf) 0))
		     (output outbuf)
		     (set! outbuf "") )
		   (unless (##sys#slot data 2) (##net#shutdown fd _sd_send))
		   (when (and iclosed (eq? -1 (##net#close fd)))
		     (##sys#update-errno)
		     (##sys#signal-hook
		      #:network-error (##sys#string-append "can not close socket output port - " strerror) fd) ) ) )
	       (and outbuf
		    (lambda ()
		      (when (fx> (##sys#size outbuf) 0)
			(output outbuf)
			(set! outbuf "") ) ) ) ) ) )
	(##sys#setslot in 3 "(tcp)")
	(##sys#setslot out 3 "(tcp)")
	(##sys#setslot in 7 'socket)
	(##sys#setslot out 7 'socket)
	(##sys#setslot (##sys#port-data in) 0 data)
	(##sys#setslot (##sys#port-data out) 0 data)
	(values in out) ) ) ) )

(define (tcp-accept tcpl)
  (##sys#check-structure tcpl 'tcp-listener)
  (let ((fd (##sys#slot tcpl 1)))
    (let loop ()
      (if (eq? 1 (##net#select fd))
	  (let ((fd (##net#accept fd #f #f)))
	    (when (eq? -1 fd)
	      (##sys#update-errno)
	      (##sys#signal-hook 
	       #:network-error 'tcp-accept (##sys#string-append "could not accept from listener - " strerror) 
	       tcpl) )
	    (##net#io-ports fd) )
	  (begin
	    (##sys#thread-block-for-i/o! ##sys#current-thread fd #t)
	    (yield)
	    (loop) ) ) ) ) )

(define (tcp-accept-ready? tcpl)
  (##sys#check-structure tcpl 'tcp-listener 'tcp-accept-ready?)
  (let ((f (##net#select (##sys#slot tcpl 1))))
    (when (eq? -1 f)
      (##sys#update-errno)
      (##sys#signal-hook 
       #:network-error 'tcp-accept-ready? (##sys#string-append "can not check socket for input - " strerror) 
       tcpl) )
    (eq? 1 f) ) )

(define get-socket-error
  (foreign-lambda* int ((int socket))
		   "int err, optlen;"
		   "optlen = sizeof(err);"
		   "if (typecorrect_getsockopt(socket, SOL_SOCKET, SO_ERROR, &err, (socklen_t *)&optlen) == -1)"
		   "return(-1);"
		   "return(err);"))

(define general-strerror (foreign-lambda c-string "strerror" int))

(define (tcp-connect host . more)
  (let ((port (:optional more #f)))
    (##sys#check-string host)
    (unless port
      (set!-values (host port) (##net#parse-host host "tcp"))
      (unless port (##sys#signal-hook #:network-error 'tcp-connect "no port specified" host)) )
    (##sys#check-exact port)
    (let ((addr (make-string _sockaddr_in_size))
	  (s (##net#socket _af_inet _sock_stream 0)) )
      (define (fail)
	(##net#close s)
	(##sys#update-errno)
	(##sys#signal-hook 
	 #:network-error 'tcp-connect (##sys#string-append "can not connect to socket - " strerror) 
	 host port) )
      (when (eq? -1 s)
	(##sys#update-errno)
	(##sys#signal-hook #:network-error 'tcp-connect (##sys#string-append "can not create socket - " strerror) host port) )
      (unless (##net#gethostaddr addr host port)
	(##sys#signal-hook #:network-error 'tcp-connect "can not find host address" host) )
      (unless (##net#make-nonblocking s)
	(##sys#update-errno)
	(##sys#signal-hook #:network-error 'tcp-connect (##sys#string-append "fcntl() failed - " strerror)) )
      (when (eq? -1 (##net#connect s addr _sockaddr_in_size))
	(if (eq? errno _einprogress)
	    (let loop ()
	      (let ((f (##net#select-write s)))
		(when (eq? f -1) (fail))
		(unless (eq? f 1)
		  ;(##sys#thread-block-for-i/o! ##sys#current-thread s #t)
		  (yield)
		  (loop) ) ) )
	    (fail) ) )
      (let ((err (get-socket-error s)))
	(cond ((= err -1) 
	       (##sys#signal-hook #:network-error 'tcp-connect (##sys#string-append "getsockopt() failed - " strerror)))
	      ((> err 0) 
	       (##sys#signal-hook #:network-error 'tcp-connect (##sys#string-append "can not create socket - " (general-strerror err))))))
      (##net#io-ports s) ) ) )

(define (##sys#tcp-port->fileno p)
  (##sys#slot (##sys#tcp-port-data p) 0) )

(define (##sys#tcp-port-data p)
  (##sys#check-port p)
  (let ((d (##sys#port-data p)))
    (if d
	(##sys#slot d 0)
	(##sys#signal-hook #:type-error "bad argument type - not a TCP port - " p) ) ) )

(define (tcp-addresses p)
  (let ((fd (##sys#tcp-port->fileno p)))
    (values 
     (or (##net#getsockname fd)
	 (##sys#signal-hook #:network-error 'tcp-addresses (##sys#string-append "can not compute local address - " strerror) p) )
     (or (##net#getpeername fd)
	 (##sys#signal-hook #:network-error 'tcp-addresses (##sys#string-append "can not compute remote address - " strerror) p) ) ) ) )

(define (tcp-port-numbers p)
 (let ((fd (##sys#tcp-port->fileno p)))
   (values
    (or (##net#getsockport fd)
        (##sys#signal-hook #:network-error 'tcp-port-numbers (##sys#string-append "can not compute local port - " strerror) p) )
    (or (##net#getpeerport fd)
        (##sys#signal-hook #:network-error 'tcp-port-numbers (##sys#string-append "can not compute remote port - " strerror) p) ) ) ) )

(define (tcp-listener-port tcpl)
  (##sys#check-structure tcpl 'tcp-listener 'tcp-listener-port)
  (let* ((fd (##sys#slot tcpl 1))
	 (port (##net#getsockport fd)) )
    (when (eq? -1 port)
      (##sys#signal-hook
       #:network-error 'tcp-listener-port (##sys#string-append "can not obtain listener port - " strerror) 
       tcpl fd) )
    port) )

(define (tcp-abandon-port p)
  (##sys#setislot
   (##sys#tcp-port-data p)
   (if (##sys#slot p 1) 2 1)
   #t) ) 

(define (tcp-listener-fileno l)
  (##sys#check-structure l 'tcp-listener 'tcp-listener-fileno)
  (##sys#slot l 1) )
