;Copyright (C) 1981,1982 by Manx Software Systems
; :ts=8
;
	public tolower_
;
tolower_:
	lxi	h,2
	dad	sp
	mov	a,m
	cpi	'A'
	jc	skip2
	cpi	'Z'+1
	jnc	skip2
	adi	'a'-'A'
skip2:
	mov	l,a
	mvi	h,0
	ora	a
	ret
	end
