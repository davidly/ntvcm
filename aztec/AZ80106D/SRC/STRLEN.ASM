;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	public strlen_
strlen_: LXI H,2
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	LXI D,0
	XRA A
.stl:	CMP M
	JZ	.stlx
	INX D
	INX H
	JMP .stl
.stlx:	XCHG
	mov a,l
	ora h
	RET
	end
