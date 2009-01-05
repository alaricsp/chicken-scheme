/* apply-hack.ppc.s
;
; Copyright (c) 2007, Felix L. Winkelmann
; Copyright (c) 2008-2009 The Chicken Team
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

	.globl _C_do_apply_hack
	.align 2
	
_C_do_apply_hack:
	mr r13, r3		/* r13=proc */
	mr r14, r4		/* r14=buf */
	mr r15, r5		/* r15=count */
	cmpwi r15, 8
	bge l1
	li r3, 8		/* offset is (8 - count) * 4 */
	sub r15, r3, r15
	slwi r15, r15, 2
	bl l2			/* compute branch address */
l2:	mflr r4
	add r15, r4, r15
	addi r15, r15, lo16(l1 - l2)
	mtctr r15
	bctr
l1:	lwz r10, 28(r14)	/* load register arguments */
	lwz r9, 24(r14)
	lwz r8, 20(r14)
	lwz r7, 16(r14)
	lwz r6, 12(r14)
	lwz r5, 8(r14)
	lwz r4, 4(r14)
	lwz r3, 0(r14)
	lwz r15, 20(r1)		/* save link area above faked argument area */
	stw r15, -4(r14)	/* (start from end if destination overlaps) */
	lwz r15, 16(r1)		/* is this needed at all? at least for proper gdb backtraces? */
	stw r15, -8(r14)
	lwz r15, 12(r1)
	stw r15, -12(r14)
	lwz r15, 8(r1)
	stw r15, -16(r14)
	lwz r15, 4(r1)
	stw r15, -20(r14)
	lwz r15, 0(r1)
	stw r15, -24(r14)
	addi r1, r14, -24	/* set frame-pointer to faked frame */
	mtctr r13		/* jump to proc, lr is invalid, but we won't return anyway */
	bctr
