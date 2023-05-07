	public .swt
.swt:	xchg
	pop	h
	PUSH B
	MOV B,D
	MOV C,E
	MOV E,M
	INX H
	MOV D,M
swt.1: DCX D
	MOV A,D
	ORA A
	JM	swt.def
	INX H
	MOV A,C
	CMP M
	JZ	swt.3
	INX H
swt.2: INX H
	INX H
	JMP swt.1
swt.3: INX H
	MOV A,B
	CMP M
	JNZ swt.2
swt.def:	INX H
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	POP B
	PCHL
;
	end
