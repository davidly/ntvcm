;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	public rindex_
rindex_:
	push	b
	lxi	h,4
	dad	sp
	mov	e,m		;DE = destination
	inx	h
	mov	d,m
	inx	h
	mov	l,m
	xchg		;e has char to look for
	lxi	b,0
	xra	a
toend:
	cmp	m		;scan for end of string
	jz	scan
	inx	h
	inx	b
	jmp	toend

scan:
	mov	a,b
	ora	c
	jz	noluck
	dcx	b
	dcx	h
	mov	a,m
	cmp	e
	jnz	scan
	mov a,h
	ora l
	pop b
	ret
noluck:
	lxi h,0
	xra a
	pop b
	ret
;
	end
