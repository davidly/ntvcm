; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.lcm		;complement primary
.lcm:
	lxi	h,lnprm
	mvi	d,4
cmloop:
	mov	a,m
	cma
	mov	m,a
	inx	h
	dcr	d
	jnz	cmloop
	ret
	end
