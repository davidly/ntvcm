;Copyright (C) 1981,1982 by Manx Software Systems
;Copyright (C) 1983,1984 by Manx Software Systems
; :ts=8
	extrn	$MEMRY, sbot
;
; rsvstk(size): reserve size bytes of stack space
;
	public	rsvstk_
rsvstk_:
	lxi	h,2
	dad	sp
	mov	a,l
	sub	m
	mov	e,a
	mov	a,h
	inx	h
	sbb	m
	mov	d,a
	xchg
	shld	sbot
	ret
	end
