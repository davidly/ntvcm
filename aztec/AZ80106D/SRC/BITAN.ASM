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
	end
