	public .ue,.uf
.uf:		; uge
	XCHG
.ue: mov a,l	; ule
	sub e
	mov a,h
	sbb d
	mvi a,0
	cmc
	aci 0
	mov l,a
	mvi h,0
	ret
;
	end
