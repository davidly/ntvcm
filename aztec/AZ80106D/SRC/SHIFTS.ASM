;Copyright (C) 1981,1982 by Manx Software Systems
;
	public .ml
.ml: PUSH B
	MOV	B,H
	MOV	C,L	
	LXI H,0		;CLEAR RESULT
	MVI A,16	;ITERATION COUNT
.mlp: DAD H		;SHIFT LEFT
	XCHG		; NOW SHIFT DE LEFT
	DAD H
	XCHG
	JNC .msk
	DAD B
.msk: DCR A		;COUNT TIMES THRU
	JNZ .mlp	;go thru 16 times
	POP	B
	mov a,l
	ora h
	RET
;
	public .rs
.rs:	XCHG
	mov a,e
	ani	31
	mov	e,a
	jz	setcc
	MOV A,H
	ORA H
	JP .arloop
;
.sign:	MOV	A,H
	STC
	RAR
	MOV	H,A
	MOV	A,L
	RAR
	MOV	L,A
	DCR E
	JNZ	.sign
	ora h
	ret
;
	public .ls
.ls:	XCHG
	mov a,e
	ani	31
	mov	e,a
	jz	setcc
lslp:
	DAD H
	DCR E
	JNZ	lslp
setcc:
	mov a,l
	ora h
	ret
;
	public .ur
.ur: XCHG
	mov a,e
	ani	31
	mov	e,a
	jz	setcc
.arloop:	MOV	A,H
	ORA	A
	RAR
	MOV	H,A
	MOV	A,L
	RAR
	MOV	L,A
	DCR E
	JNZ	.arloop
	ora h
	ret
;
	end
