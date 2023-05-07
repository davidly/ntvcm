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
	end
