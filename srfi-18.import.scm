;;;; srfi-18.import.scm - import library for "srfi-18" module
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


(##sys#register-primitive-module
 'srfi-18
 '(abandoned-mutex-exception?
   condition-variable-broadcast!
   condition-variable-signal!
   condition-variable-specific
   condition-variable-specific-set!
   condition-variable?
   current-thread
   current-time
   join-timeout-exception?
   make-condition-variable
   make-mutex
   make-thread
   milliseconds->time
   mutex-lock!
   mutex-name
   mutex-specific
   mutex-specific-set!
   mutex-state
   mutex-unlock!
   mutex?
   raise
   seconds->time
   srfi-18:current-time
   srfi-18:time?
   terminated-thread-exception?
   thread-join!
   thread-name
   thread-quantum
   thread-quantum-set!
   thread-resume!
   thread-signal!
   thread-sleep!
   thread-specific
   thread-specific-set!
   thread-start!
   thread-state
   thread-suspend!
   thread-terminate!
   thread-wait-for-i/o!
   thread-yield!
   thread?
   time->milliseconds
   time->seconds
   time?
   uncaught-exception-reason
   uncaught-exception?))
