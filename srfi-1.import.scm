;;;; srfi-1.import.scm - import library for "srfi-1" module
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
 'srfi-1
 '(alist-cons
   alist-copy
   alist-delete
   alist-delete!
   any
   append!
   append-map
   append-map!
   append-reverse
   append-reverse!
   assoc
   break
   break!
   car+cdr
   circular-list
   circular-list?
   concatenate
   concatenate!
   cons*
   count
   delete
   delete!
   delete-duplicates
   delete-duplicates!
   dotted-list?
   drop
   drop-right
   drop-right!
   drop-while
   eighth
   every
   fifth
   filter
   filter!
   filter-map
   find
   find-tail
   first
   fold
   fold-right
   fourth
   iota
   last
   last-pair
   length+
   list-copy
   list-index
   list-tabulate
   list=
   lset-adjoin
   lset-diff+intersection
   lset-diff+intersection!
   lset-difference
   lset-difference!
   lset-intersection
   lset-intersection!
   lset-union
   lset-union!
   lset-xor
   lset-xor!
   lset<=
   lset=
   make-list
   map
   map!
   map-in-order
   member
   ninth
   not-pair?
   null-list?
   pair-fold
   pair-fold-right
   pair-for-each
   partition
   partition!
   proper-list?
   reduce
   reduce-right
   remove
   remove!
   reverse!
   second
   seventh
   sixth
   span
   span!
   split-at
   split-at!
   take
   take!
   take-right
   take-while
   take-while!
   tenth
   third
   unfold
   unfold-right
   unzip1
   unzip2
   unzip3
   unzip4
   unzip5
   xcons
   zip))
