;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	public strncpy_
strncpy_:
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
	push	h		;save target for return
cpyloop:
	mov	a,b	;while (len) {
	ora	c
	jz	done
	ldax	d		;if (*s1 = *s2) ++s1
	mov	m,a
	ora	a
	jz	padding
	inx	d
padding:
	inx	h		;++s2
	dcx	b		;--len
	jmp	cpyloop	;}
done:
	pop	h		;return target address
	pop	b
	mov	a,h
	ora	l
	ret
	end
