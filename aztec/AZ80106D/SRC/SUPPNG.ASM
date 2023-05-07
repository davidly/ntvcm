	public .ng
.ng:	MOV A,L
	CMA
	MOV L,A
	MOV A,H
	CMA
	MOV H,A
	INX H
	mov a,l
	ora h
	RET
;
	end
