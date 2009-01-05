;;;; data-structures.import.scm - import library for "data-structures" module
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
 'data-structures
 '(->string
   alist-ref
   alist-update!
   always?
   any?
   atom?
   binary-search
   butlast
   chop
   complement
   compose
   compress
   conc
   conjoin
   constantly
   disjoin
   each
   flatten
   flip
   identity
   intersperse
   join
   left-section
   list->queue
   list-of?
   make-queue
   merge
   merge!
   never?
   none?
   noop
   o
   project
   queue->list
   queue-add!
   queue-empty?
   queue-first
   queue-last
   queue-push-back!
   queue-push-back-list!
   queue-remove!
   queue?
   rassoc
   right-section
   shuffle
   sort
   sort!
   sorted?
   string-chomp
   string-chop
   string-compare3
   string-compare3-ci
   string-intersperse
   string-split
   string-translate
   string-translate*
   substring-ci=?
   substring-index
   substring-index-ci
   substring=?
   tail?))
