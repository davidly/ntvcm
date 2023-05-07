	public .xr
.xr: MOV A,H
	XRA	D
	MOV	H,A
	MOV	A,L
	XRA	E
	MOV	L,A
	ora h
	RET
	end
