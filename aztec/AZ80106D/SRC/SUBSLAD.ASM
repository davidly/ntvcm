; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public .lad		;add secondary to primary
.lad:
			;DE is used as primary address
			;and HL is used as secondary address
	push	b
	lxi	d,lnprm
	lxi	h,lnsec
	xra	a	;clear carry
	mvi	b,4
adloop:
	ldax	d
	adc	m
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	adloop
	pop	b
	ret
	end
