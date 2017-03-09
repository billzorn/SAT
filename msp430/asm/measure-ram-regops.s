.section .resetvec, "aw"
.balign 2
	.word	_start

.section .bss
.balign 2
_tmp1:	.skip 2
_tmp2:	.skip 2
_tmpsr:	.skip 2
_res:	.skip 2
_ressp:	.skip 4
_ressr:	.skip 4

_save:
	.skip 4
_save_r1:
	.skip 12
_save_r4:
	.skip 48
_save_end:
_rand:
	.skip 4
_rand_r1:
	.skip 12
_rand_r4:
	.skip 48
_rand_end:

_arg1:
	.skip 48
_arg2:
	.skip 48
_argsr:
	.skip 48

_results:
	.skip 1536
_results_end:

.section .rodata
.balign 2
_initregs:
_r4:	.word _arg1		; address of first argument
	.word 0x0
_r5:	.word _arg2		; address of second argument
	.word 0x0
_r6:	.word _argsr		; address of SR value
	.word 0x0
_r7:	.word 0x0
	.word 0x0
_r8:	.word 0x0
	.word 0x0
_r9:	.word 0x0
	.word 0x0
_r10:	.word 0x0
	.word 0x0
_r11:	.word 0x0
	.word 0x0
_r12:	.word 0x0
	.word 0x0
_r13:	.word 0x0
	.word 0x0
_r14:	.word 0x0		; i
	.word 0x0
_r15:	.word 0x18		; 24, max_iters
	.word 0x0
	
.section .text
.balign 2
_start:
	;; disable watchdog timer
	MOV.W	#23168, &0x015C

	;; abort and halt
	JMP	.go
.abort:
	JMP	.abort
.go:

	;; ;; timer init
	;; MOV.W	#16, &0x0342
	;; MOV.W	#512, &0x0340

	;; ;; timer start
	;; MOV.W	#0, &0x0350
	;; MOV.W	#50000, &0x0352
	;; BIS.W	#16, &0x0340

;;; tests begin

	;;  load all initial register values
	MOV.W	#_initregs, SP
	POPM.A	#12, R15
	;; result storage location is kept separately
	MOV.W	#_results, &_res

_loop:
	;; count test, check if we're done yet
	INC.W	R14
	CMP.W	R14, R15 	; Is R15 (max iters) < R14?
	JL	_loop_done	; Yes, we're done.
	
	;; set up arguments
	MOV.W	@R4+, &_tmp1
	MOV.W	@R5+, &_tmp2
	MOV.W	@R6+, &_tmpsr
	AND.W	#0x0107, &_tmpsr ; clear invalid flags

	;; save the current register state
	MOV.W	#_save_end, SP
	PUSHM.A	#16, R15
	
	;; initialize registers with arbitrary bits
	MOV.W 	#_rand_r4, SP
	POPM.A	#12, R15
	MOVX.A	&_rand_r1, SP

	;; initialize test data
_set_arg_r1:
	MOV.W	&_tmp1, R4
_set_arg_r2:
	MOV.W	&_tmp2, R5
_set_arg_sr:
	MOV.W	&_tmpsr, SR

_test_critical:
	;; run test
	;; e.g. DADD.B	R4, R5
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
_test_critical_end:

	;; save results
	MOVX.A	SP, &_ressp
	MOVX.A	SR, &_ressr
	ADD.W	#64, &_res	; corrupts SR
	MOV.W	&_res, SP
	PUSHM.A	#16, R15
	MOVX.A	&_ressp, 4(SP)
	MOVX.A	&_ressr, 8(SP)

	;; restore registers
	MOV.W	#_save_r4, SP
	POPM.A #12, R15
	
	;; run again
	JMP	_loop
_loop_done:

;;; tests end

	;; landing pad
.halt:
	.word	0x3fff	; halt
	.word	0x3fff	; halt
	.word	0x3fff	; halt
	.word	0x3fff	; halt
	.word	0x3fff	; halt
	.word	0x3fff	; halt
	.word	0x3fff	; halt
	JMP	.halt
