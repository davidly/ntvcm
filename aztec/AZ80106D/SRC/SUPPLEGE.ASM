	public .le,.ge
.ge:		; ge
	XCHG
.le:	mov a,h
	xra	d
	jm	.lediff	; signs differ
				; signs alike
	mov a,l
	sub e
	mov a,h
	sbb d
	cmc
	mvi a,0
	aci 0
	mov l,a
	mvi h,0
	ret
.lediff: mov a,d
	rlc
	ani 1
	mov l,a
	mvi h,0
	ret
;
	end
