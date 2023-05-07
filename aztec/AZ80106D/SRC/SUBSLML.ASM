; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
	public	.lml
.lml:		;long multiply	(primary = primary * secondary)
	push	b
;
	lxi	h,lnprm
	mvi	b,4
	lxi	d,lntmp		;copy multiplier into work area
msav:
	mov	a,m
	stax	d
	mvi	m,0
	inx	h
	inx	d
	dcr	b
	jnz	msav
;
	mvi	a,32		;initialize loop counter
muloop:
	push	psw
	lxi	h,lnprm
	mvi	b,8
	ora	a		;clear carry
mshlp:
	mov	a,m
	adc	a		;shift one bit to the left
	mov	m,a
	inx	h
	dcr	b
	jnz	mshlp
	jnc	mnext

	mvi	b,4
	lxi	d,lnprm
	lxi	h,lnsec
	ora	a		;clear carry
maddlp:
	ldax	d
	adc	m
	stax	d
	inx	d
	inx	h
	dcr	b
	jnz	maddlp
;
mnext:
	pop	psw
	dcr	a
	jnz	muloop
	pop	b
	ret
	end
