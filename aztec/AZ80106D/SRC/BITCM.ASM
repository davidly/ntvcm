	public .cm
.cm:	MOV	A,H
	CMA
	MOV	H,A
	MOV	A,L
	CMA
	MOV	L,A
	ora h
	RET
;
	end
