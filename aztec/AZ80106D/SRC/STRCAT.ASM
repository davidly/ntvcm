;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	public strcat_		;strcat(s1,s2)
strcat_:
	lxi	h,5
	dad	sp
	push	b
	lxi	b,32767
	jmp	same
;
	public strncat_		;strncat(s1,s2,len)
strncat_:
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
	push	h		;save destination for return value
	xra	a
eloop:
	cmp	m		;while (*s1) ++s1;
	jz	cpyloop
	inx	h
	jmp	eloop	;}
cpyloop:			;while (len) {
	mov	a,b
	ora	c
	jz	done
	ldax	d			;if ((*s1 = *s2) == 0) break
	mov	m,a
	ora	a
	jz	done
	inx	d			;++s1
	inx	h			;++s2
	dcx	b			;--len
	jmp	cpyloop		;}
done:
	mov	m,a		;guarantee null termination
	pop	h
	pop	b
	mov	a,h
	ora	l
	ret
	end
