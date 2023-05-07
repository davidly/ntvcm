	public .sb
.sb: XCHG
	mov a,l
	sub e
	mov l,a
	mov a,h
	sbb d
	mov h,a
	ora l
	ret
;
	end
