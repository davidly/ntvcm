; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.lng		;negate primary
.lng:
	lxi	h,lnprm
negate:
	xra	a
	mvi	d,4
ngloop:
	mvi	a,0
	sbb	m
	mov	m,a
	inx	h
	dcr	d
	jnz	ngloop
	ret
;
setup:
	lxi	h,3
	dad	d
	mov	c,m
	mov	a,c
	ora	a
	rp
	xchg
	jmp	negate		;force positive
;
	public	.ldv
.ldv:		;long divide	(primary = primary/secondary)
	push	b
	lxi	d,lnprm
	call	setup
	push	b
	lxi	d,lnsec
	call	setup
	mov	a,c
	pop	b		;get primary sign
	xra	c		;merge signs
	push	psw		;save for return
	call	dodivide
	pop	psw
	pop	b
	jm	.lng
	ret
;
	public	.lrm
.lrm:		;long remainder	(primary = primary%secondary)
	push	b
	lxi	d,lnprm
	call	setup
	mov	a,c
	ora	a
	push	psw
	lxi	d,lnsec
	call	setup
	call	dodivide
	lxi	d,lntmp
	lxi	h,lnprm
	mvi	b,4
remsave:
	ldax	d
	mov	m,a
	inx	d
	inx	h
	dcr	b
	jnz	remsave
	pop	psw
	pop	b
	jm	.lng
	ret
;
	public	.lud
.lud:		;unsigned long divide	(primary = primary/secondary)
	push	b
	call	dodivide
	pop	b
	ret
;
	public	.lum
.lum:		;long remainder	(primary = primary%secondary)
	push	b
	call	dodivide
	lxi	d,lntmp
	lxi	h,lnprm
	mvi	b,4
uremsave:
	ldax	d
	mov	m,a
	inx	d
	inx	h
	dcr	b
	jnz	uremsave
	pop	b
	ret
;
;
dodivide:
	mvi	b,4
	lxi	h,lntmp		;clear quotient buffer
	xra	a
quinit:
	mov	m,a
	inx	h
	dcr	b
	jnz	quinit

	mvi	a,32		;initialize loop counter
divloop:
	push	psw
	lxi	h,lnprm
	mvi	b,8
	ora	a		;clear carry
shlp:
	mov	a,m
	adc	a		;shift one bit to the left
	mov	m,a
	inx	h
	dcr	b
	jnz	shlp
	sbb	a
	ani	1
	mov	c,a

	mvi	b,4
	lxi	d,lntmp
	lxi	h,lnsec
	ora	a		;clear carry
sublp:
	ldax	d
	sbb	m
	stax	d
	inx	d
	inx	h
	dcr	b
	jnz	sublp
	mov	a,c
	sbi	0
	jnz	zerobit
onebit:
	lxi	h,lnprm
	inr	m
	pop	psw
	dcr	a
	jnz	divloop
	ret
;
zerobit:
	pop	psw
	dcr	a
	jz	restore
	push	psw
	lxi	h,lnprm
	mvi	b,8
	ora	a		;clear carry
zshlp:
	mov	a,m
	adc	a		;shift one bit to the left
	mov	m,a
	inx	h
	dcr	b
	jnz	zshlp
	sbb	a
	mov	c,a

	mvi	b,4
	lxi	d,lntmp
	lxi	h,lnsec
	ora	a		;clear carry
daddlp:
	ldax	d
	adc	m
	stax	d
	inx	d
	inx	h
	dcr	b
	jnz	daddlp
	mov	a,c
	aci	0
	jnz	zerobit
	jmp	onebit
;
restore:			;fix up remainder if still negative
	mvi	b,4
	lxi	d,lntmp
	lxi	h,lnsec
	ora	a		;clear carry
resloop:
	ldax	d
	adc	m
	stax	d
	inx	d
	inx	h
	dcr	b
	jnz	resloop
	ret
	end
