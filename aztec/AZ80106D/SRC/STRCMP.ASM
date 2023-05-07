;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	public strcmp_
strcmp_:
	lxi	h,5
	dad	sp
	push	b
	lxi	b,32767
	jmp	same
;
	public strncmp_
strncmp_:
	lxi	h,7
	dad	sp
	push	b
	mov	b,m
	dcx	h
	mov	c,m		;BC = len
	dcx	h
same:
	mov	d,m
	dcx	h
	mov	e,m		;DE = s2
	dcx	h
	mov	a,m
	dcx	h
	mov	l,m
	mov	h,a		;HL = s1
	xchg			;now DE=s1, HL=s2
cmploop:
	mov	a,b	;while (len) {
	ora	c
	jz	done
	ldax	d		;if (*s1-*s2) break
	sub	m
	jnz	done
	ldax	d		;if (*s1 == 0) break
	ora	a
	jz	done
	inx	d		;++s1
	inx	h		;++s2
	dcx	b		;--len
	jmp	cmploop	;}
done:
	pop	b
	mov	l,a
	sbb	a
	mov	h,a
	ora	l
	ret
	end
