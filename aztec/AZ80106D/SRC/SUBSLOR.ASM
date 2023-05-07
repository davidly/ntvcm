; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.lor		;or primary with secondary
.lor:
	push	b
	lxi	d,lnprm
	lxi	h,lnsec
	mvi	b,4
orloop:
	ldax	d
	ora	m
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	orloop
	pop	b
	ret
	end
