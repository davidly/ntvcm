;Copyright (C) 1981,1982 by Manx Software Systems
; Copyright (C) 1981  Thomas Fenwick
; :ts=8
	public .nt
.nt:	MOV	A,H
	ORA	L
	jz .true
	jmp .false
;
	public .eq,.ne
.eq: mov a,l
	sub e
	jnz .false
	mov a,h
	sub d
	jz .true
.false: lxi h,0
	xra a
	ret
;
.ne: mov a,l
	sub e
	jnz .true
	mov a,h
	sub d
	jz .false
.true: lxi h,1
	mov a,l
	ora h
	RET
;
	public .le,.ge
.ge:		; ge
	XCHG
.le:	mov a,h
	xra	d
	jm	.lediff	; signs differ
				; signs alike
	mov a,l
	sub e
	mov a,h
	sbb d
	cmc
	mvi a,0
	aci 0
	mov l,a
	mvi h,0
	ret
.lediff: mov a,d
	rlc
	ani 1
	mov l,a
	mvi h,0
	ret
;
	public .lt,.gt
.lt:
	XCHG
.gt:	mov a,h
	xra	d
	jm	.gtdiff	; signs differ
				; signs alike
	mov a,l
	sub e
	mov a,h
	sbb d
	mvi a,0
	aci 0
	mov l,a
	mvi h,0
	ret
.gtdiff: mov a,h
	rlc
	ani 1
	mov l,a
	mvi h,0
	ret
;
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
	public .ue,.uf
.uf:		; uge
	XCHG
.ue: mov a,l	; ule
	sub e
	mov a,h
	sbb d
	mvi a,0
	cmc
	aci 0
	mov l,a
	mvi h,0
	ret
;
	public .ug,.ul
.ul:		; ult
	XCHG
.ug: mov a,l
	sub e
	mov a,h
	sbb d
	mvi a,0
	aci 0
	mov l,a
	mvi h,0
	ret
;
	end
