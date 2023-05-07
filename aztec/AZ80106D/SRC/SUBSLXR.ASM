; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.lxr		;exclusive or primary with secondary
.lxr:
	push	b
	lxi	d,lnprm
	lxi	h,lnsec
	mvi	b,4
xrloop:
	ldax	d
	xra	m
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	xrloop
	pop	b
	ret
	end
