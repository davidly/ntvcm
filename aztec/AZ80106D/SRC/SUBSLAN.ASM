; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.lan		;and primary with secondary
.lan:
	push	b
	lxi	d,lnprm
	lxi	h,lnsec
	mvi	b,4
ndloop:
	ldax	d
	ana	m
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	ndloop
	pop	b
	ret
	end
