; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
	public	.lsb		;subtract secondary from primary
.lsb:
	push	b
	lxi	d,lnprm
	lxi	h,lnsec
	xra	a	;clear carry
	mvi	b,4
sbloop:
	ldax	d
	sbb	m
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	sbloop
	pop	b
	ret
	end
