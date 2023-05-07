;Copyright (C) 1981,1982 by Manx Software Systems
; :ts=8
	public toupper_
toupper_:
	lxi	h,2
	dad	sp
	mov	a,m
	cpi	'a'
	jc	skip
	cpi	'z'+1
	jnc	skip
	sui	'a'-'A'
skip:
	mov	l,a
	mvi	h,0
	ora	a
	ret
	end
