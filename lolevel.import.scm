;;;; lolevel.import.scm - import library for "lolevel" module
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
 'lolevel
 '(address->pointer
   align-to-word
   allocate
   block-ref
   block-set!
   extend-procedure
   extended-procedure?
   free
   global-bound?
   global-make-unbound!
   global-ref
   global-set!
   locative->object
   locative-ref
   locative-set!
   locative?
   make-locative
   make-record-instance
   make-weak-locative
   move-memory!
   mutate-procedure
   null-pointer
   null-pointer?
   number-of-bytes
   number-of-slots
   object->pointer
   object-become!
   object-copy
   object-evict
   object-evict-to-location
   object-evicted?
   object-release
   object-size
   object-unevict
   pointer->address
   pointer-like?
   pointer->object
   pointer-f32-ref
   pointer-f32-set!
   pointer-f64-ref
   pointer-f64-set!
   pointer-offset
   pointer-s16-ref
   pointer-s16-set!
   pointer-s32-ref
   pointer-s32-set!
   pointer-s8-ref
   pointer-s8-set!
   pointer-tag
   pointer-u16-ref
   pointer-u16-set!
   pointer-u32-ref
   pointer-u32-set!
   pointer-u8-ref
   pointer-u8-set!
   pointer=?
   pointer?
   procedure-data
   record->vector
   record-instance?
   record-instance-length
   record-instance-slot
   record-instance-slot-set!
   record-instance-type
   set-invalid-procedure-call-handler!
   set-procedure-data!
   tag-pointer
   tagged-pointer?
   unbound-variable-value
   vector-like?))
