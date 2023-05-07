;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	extrn	.ng
	public	.dv,.ud
.dv: 			; DE has dividend, HL has divisor
	mov	a,d
	xra	h		;check if signs differ
	push	psw	;and remember
	call	divsub	;use same routine as modulo
	xchg		;and swap results
	pop	psw
	jm	.ng		;negate result if signs of operands differ
	mov	a,l
	ora	h
	RET
;
.ud:
	CALL	.um	;use same routine as modulo
	XCHG		;and swap results
	mov	a,l
	ora	h
	RET
;
	public	.rm,.um
.rm:
	mov	a,d
	push	psw
	call	divsub
	pop	psw
	ora	a
	jm	.ng		;negate result if dividend was signed
	mov	a,h
	ora	l
	ret
;
divsub:
	mov	a,h
	ora	a
	jp	hlpos
	cma
	mov	h,a
	mov	a,l
	cma
	mov	l,a
	inx	h
hlpos:
	mov	a,d
	ora	a
	jp	.um
	cma
	mov	d,a
	mov	a,e
	cma
	mov	e,a
	inx	d
;			fall through into .um
;
.um:	push	b		;save for C
	mov	c,l
	mov	b,h
	lxi	h,0
	call	div16
	pop	b
	mov	a,l		;set flags for C
	ora	h
	ret
;
;	div16:  divides (hl,de) by bc
;		returns remainder in hl, quotient in de
	public	div16
div16:
	mov	a,c
	cma
	mov	c,a
	mov	a,b
	cma
	mov	b,a
	inx	b
	MVI	A,16	;iteration count
divloop:
	DAD	H		;shift hl left
	XCHG
	DAD	H		;shift de left
	XCHG
	JNC	nocy
	INR	L		;carry into high part
nocy:
	dad	b		;subtract divisor
	jc	setbit
	push	psw
	mov	a,l
	sub	c
	mov	l,a
	mov	a,h
	sbb	b
	mov	h,a
	pop	psw
	DCR	A		;count times thru
	JNZ	divloop
	ret
setbit:
	INR	E		;set quotient bit
	DCR	A		;count times thru
	JNZ	divloop
	ret
	end
