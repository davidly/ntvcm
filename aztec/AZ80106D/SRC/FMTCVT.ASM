; Copyright (C) 1983 by Manx Software Systems
; :ts=8
	dseg
string:	ds	2
size:	dw	0
number:	ds	4
	cseg
	public	fmtcvt_
fmtcvt_:			;char *fmtcvt(ptr, base, buffer, size)
	push	b
	lxi	h,0
	shld	number
	shld	number+2
	lxi	h,10
	dad	sp
	mov	a,m
	sta	size
	mov	b,a		;save size for later
	dcx	h
	mov	d,m
	dcx	h
	mov	e,m
	dcx	h
	xchg
	mvi	m,0		;null terminate string
	shld	string
	xchg
	dcx	h
	mov	c,m		;C = base
	dcx	h
	mov	d,m
	dcx	h
	mov	e,m
	lxi	h,number
cpnum:
	ldax	d
	mov	m,a
	inx	d
	inx	h
	dcr	b
	jnz	cpnum

	mov	a,c
	ora	a
	jp	unsigned	; base < 0, means do signed conversion
	cma
	inr	a
	mov	c,a		;C = base
	lhld	size
	lxi	d,number-1
	dad	d
	mov	a,m
	ora	a
	push	psw
	jp	top
				;number is negative, so make it positive
		;note: carry is already cleared by 'ora' above
	lda	size
	mov	b,a
	lxi	h,number
ngloop:
	mvi	a,0
	sbb	m
	mov	m,a
	inx	h
	dcr	b
	jnz	ngloop
	jmp	top
unsigned:
	push	psw
top:
	lxi	h,number+3
	mvi	d,0
	mvi	a,4
outer:
	push	psw
	mov	e,m
	xchg
	mvi	b,8
inner:
	dad	h
	mov	a,h
	sub	c
	jc	zero
	mov	h,a
	inr	l
zero:
	dcr	b
	jnz	inner
	xchg
	mov	m,e
	dcx	h
	pop	psw
	dcr	a
	jnz	outer
;
	mov	e,d
	mvi	d,0
	lxi	h,digits
	dad	d
	mov	a,m
	lhld	string
	dcx	h
	shld	string
	mov	m,a
;
	lxi	h,number
	mvi	b,4
	xra	a
zcheck:
	cmp	m
	jnz	top
	inx	h
	dcr	b
	jnz	zcheck

	lhld	string
	pop	psw
	jp	notneg
	dcx	h
	mvi	m,'-'
notneg:
	pop	b
	ret
;
digits:	db	'0123456789abcdef'
	end
