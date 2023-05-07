;Copyright (C) 1981,1982,1984 by Manx Software Systems
; :ts=8
	extrn	.begin
	public	.chl
.chl:	PCHL
;
	public zsave,zret
zsave: POP H
	PUSH	B
	MOV	B,H
	MOV	C,L
	LXI	H,0
	DAD	SP
	XCHG
	DAD	SP
	SPHL
	PUSH	D
	DB	221,229,253,229	;push ix ; push iy
	mov	h,b
	mov	l,c
	call	.chl
;
zret:
	DB	253,225,221,225	; pop iy ; pop ix
cret:
	XCHG
	POP	H
	SPHL
	POP	B
	XCHG
	MOV	A,H
	ORA	L
	RET
;
	public csave,cret
csave:	POP H
	PUSH	B
	MOV	B,H
	MOV	C,L
	LXI	H,0
	DAD	SP
	XCHG
	DAD	SP
	SPHL
	PUSH	D
	lxi	h,cret
	push	h
	mov	h,b
	mov	l,c
	pchl
;
;	move - move BC bytes from (HL) to (DE), used for struct assignment
;
	public .move
.move:
	mov a,m
	stax d
	inx h
	inx d
	dcx b
	mov a,b
	ora c
	jnz .move
	ret
	end
