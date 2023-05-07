;Copyright (C) 1981,1982 by Manx Software Systems
	public .an
.an: MOV A,H
	ANA	D
	MOV	H,A
	MOV	A,L
	ANA	E
	MOV	L,A
	ora h
	RET
;
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
	public .or
.or: MOV A,H
	ORA	D
	MOV	H,A
	MOV	A,L
	ORA	E
	MOV	L,A
	ora h
	RET
;
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
