;;;; srfi-4.import.scm - import library for "srfi-4" module
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
 'srfi-4
 '(blob->f32vector
   blob->f32vector/shared
   blob->f64vector
   blob->f64vector/shared
   blob->s16vector
   blob->s16vector/shared
   blob->s32vector
   blob->s32vector/shared
   blob->s8vector
   blob->s8vector/shared
   blob->u16vector
   blob->u16vector/shared
   blob->u32vector
   blob->u32vector/shared
   blob->u8vector
   blob->u8vector/shared
   f32vector
   f32vector->blob
   f32vector->blob/shared
   f32vector->byte-vector
   f32vector->list
   f32vector-length
   f32vector-ref
   f32vector-set!
   f32vector?
   f64vector
   f64vector->blob
   f64vector->blob/shared
   f64vector->byte-vector
   f64vector->list
   f64vector-length
   f64vector-ref
   f64vector-set!
   f64vector?
   list->f32vector
   list->f64vector
   list->s16vector
   list->s32vector
   list->s8vector
   list->u16vector
   list->u32vector
   list->u8vector
   make-f32vector
   make-f64vector
   make-s16vector
   make-s32vector
   make-s8vector
   make-u16vector
   make-u32vector
   make-u8vector
   read-u8vector
   read-u8vector!
   release-number-vector
   s16vector
   s16vector->blob
   s16vector->blob/shared
   s16vector->byte-vector
   s16vector->list
   s16vector-length
   s16vector-ref
   s16vector-set!
   s16vector?
   s32vector
   s32vector->blob
   s32vector->blob/shared
   s32vector->byte-vector
   s32vector->list
   s32vector-length
   s32vector-ref
   s32vector-set!
   s32vector?
   s8vector
   s8vector->blob
   s8vector->blob/shared
   s8vector->byte-vector
   s8vector->list
   s8vector-length
   s8vector-ref
   s8vector-set!
   s8vector?
   subf32vector
   subf64vector
   subs16vector
   subs32vector
   subs8vector
   subu16vector
   subu32vector
   subu8vector
   u16vector
   u16vector->blob
   u16vector->blob/shared
   u16vector->byte-vector
   u16vector->list
   u16vector-length
   u16vector-ref
   u16vector-set!
   u16vector?
   u32vector
   u32vector->blob
   u32vector->blob/shared
   u32vector->byte-vector
   u32vector->list
   u32vector-length
   u32vector-ref
   u32vector-set!
   u32vector?
   u8vector
   u8vector->blob
   u8vector->blob/shared
   u8vector->byte-vector
   u8vector->list
   u8vector-length
   u8vector-ref
   u8vector-set!
   u8vector?
   write-u8vector))
