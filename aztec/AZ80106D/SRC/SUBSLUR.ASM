; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.lur		;unsigned right shift primary by secondary bits
.lur:
;	clc			;propogate 0 bit
	jmp	rs_sub
;
	public	.lrs		;right shift primary by secondary bits
.lrs:
	lda	lnprm+3
	ral		;set carry to MSB
rs_sub:
	push	psw
	lda	lnsec
	ani	03fH		;limit to 63 places
	jz	rsdone
	mov	d,a
rslp1:
	lxi	h,lnprm+3
	mvi	e,4
	pop	psw		;get correct carry setting
	push	psw
rslp2:
	mov	a,m
	rar
	mov	m,a
	dcx	h
	dcr	e
	jnz	rslp2
	dcr	d
	jnz	rslp1
rsdone:
	pop	psw
	ret
	end
