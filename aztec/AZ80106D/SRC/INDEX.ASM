;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	public index_
index_:
	lxi	h,2
	dad	sp
	mov	e,m		;DE = destination
	inx	h
	mov	d,m
	inx	h
	mov	l,m
	xchg		;e has char to look for
scan:
	mov	a,m
	cmp	e
	jz	foundit
	ora	a
	jz	noluck
	inx	h
	jmp	scan
;
noluck:
	lxi h,0
	xra a
	ret
;
foundit:
	mov a,h
	ora l
	ret
	end
