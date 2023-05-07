; Copyright (C) 1983 by Manx Software Systems
; :ts=8
	public	setjmp_
setjmp_:
	lxi	h,2
	dad	sp
	mov	e,m		;get address of jump buffer
	inx	h
	mov	d,m
	dcx	h		;get SP value back
	xchg
	mov	m,e		;save SP value
	inx	h
	mov	m,d
	inx	h
	pop	d
	push	d
	mov	m,e		;save PC value
	inx	h
	mov	m,d
	inx	h
	mov	m,c		;save BC value
	inx	h
	mov	m,b
	xra	a
	adi	3
	jpe	setdone
	inx	h
	db	221,229		;push ix
	pop	d
	mov	m,e		;save IX value
	inx	h
	mov	m,d
	inx	h
	db	253,229		;push iy
	pop	d
	mov	m,e		;save IY value
	inx	h
	mov	m,d
setdone:
	lxi	h,0
	xra	a		;set zero flag
	ret
;
	public	longjmp_
longjmp_:
	lxi	h,2
	dad	sp
	mov	e,m		;get address of jump buffer
	inx	h
	mov	d,m
	inx	h
	mov	c,m		;get return value
	inx	h
	mov	b,m
	xchg
	mov	e,m		;get SP value
	inx	h
	mov	d,m
	inx	h
	xchg
	sphl			;switch to original stack
	xchg
	mov	e,m		;get PC value
	inx	h
	mov	d,m
	inx	h
	push	d		;save for return
	push	b		;save return value
	mov	c,m		;get BC value
	inx	h
	mov	b,m
	xra	a
	adi	3
	jpe	longdone
	inx	h
	mov	e,m		;get IX value
	inx	h
	mov	d,m
	inx	h
	push	d
	db	221,225		;pop ix
	mov	e,m		;get IY value
	inx	h
	mov	d,m
	push	d
	db	253,225		;pop iy
longdone:
	pop	h
	mov	a,l
	ora	h
	rnz
	inx	h		;force non-zero return
	inr	a		;set non-zero flag
	ret
	end
