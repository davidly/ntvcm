	public .lt,.gt
.lt:
	XCHG
.gt:	mov a,h
	xra	d
	jm	.gtdiff	; signs differ
				; signs alike
	mov a,l
	sub e
	mov a,h
	sbb d
	mvi a,0
	aci 0
	mov l,a
	mvi h,0
	ret
.gtdiff: mov a,h
	rlc
	ani 1
	mov l,a
	mvi h,0
	ret
;
	end
