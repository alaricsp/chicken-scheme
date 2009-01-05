/* apply-hack.ppc.s
;
; Copyright (c) 2008-2009, Peter Bex
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
*/

	.text

	.globl	_C_do_apply_hack
	.align	8
	.equ	BIAS, 2047
	
_C_do_apply_hack:
	mov	%o1, %g4      /* buf */
	sllx	%o2, 3, %o2   /* o2 <- count, in bytes */
	subcc	%o2, 6*8, %o2 /* 6 params are in registers */
	bg	l0
	mov	%o0, %g1      /* proc (delay slot) */
	udivx	%o2, -2, %o2  /* Offset in quads -> offset in words from l2 */
	setx	l2, %o3, %o1
	jmp	%o1+%o2
	nop
	/* Rest args on stack if > 6 args */
l0:	sub	%sp, %o2, %sp /* Alloc space for additional args */
	clr	%o1
	add	%sp, BIAS+176, %o5
	add	%g4, 6*8, %o3 /* 7th arg and up */
l1:	ldx	[%o3], %o4
	stx	%o4, [%o5+%o1]
	add	%o1, 8, %o1
	cmp	%o1, %o2
	bl	l1
	add	%o3, 8, %o3   /* (delay slot) */
l2:	ldx	[%g4+40], %o5
	ldx	[%g4+32], %o4
	ldx	[%g4+24], %o3
	ldx	[%g4+16], %o2
	ldx	[%g4+8], %o1
	ldx	[%g4+0], %o0
	
	jmp	%g1
	nop
