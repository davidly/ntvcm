; Copyright (C) 1983 by Manx Software Systems
; :ts=8
	public	swapmem_	;swapmem(s1,s2,len)
swapmem_:
	lxi	h,7
	dad	sp
	push	b
	mov	b,m
	dcx	h
	mov	c,m		;BC = len
	dcx	h
	mov	d,m
	dcx	h
	mov	e,m		;DE = s2
	dcx	h
	mov	a,m
	dcx	h
	mov	l,m
	mov	h,a		;HL = s1

	mov	a,c
	ora	a
	jnz	bok
	dcr	b
bok:
	push	b
swaploop:
	mov	b,m
	ldax	d
	mov	m,a
	mov	a,b
	stax	d
	inx	h
	inx	d
	dcr	c
	jnz	swaploop
	pop	psw
	ora	a
	jz	done
	dcr	a
	push	psw
	jmp	swaploop
done:
	pop	b
	ret
	end
