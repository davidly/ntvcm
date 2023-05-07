; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
	public	.lcmp		;compare primary and secondary
;
			;return 0 if p == s
p.lt.s:			;return < 0 if p < s
	xra	a
	dcr	a
	pop	b
	ret
;
p.gt.s:			;	> 0 if p > s
	xra	a
	inr	a
	pop	b
	ret
;
.lcmp:
	push	b
	lxi	d,lnprm+3
	lxi	h,lnsec+3
	mov	a,m
	xri	80h
	mov	c,a
	ldax	d
	xri	80h
	cmp	c
	mvi	b,4
	jmp	pswchk

	public	.ulcmp
.ulcmp:
	push	b
	lxi	d,lnprm+3
	lxi	h,lnsec+3
	mvi	b,4
cmploop:
	ldax	d
	cmp	m
pswchk:
	jc	p.lt.s
	jnz	p.gt.s
	dcx	h
	dcx	d
	dcr	b
	jnz	cmploop
			;return 0 if p == s
	xra	a
	pop	b
	ret
	end
