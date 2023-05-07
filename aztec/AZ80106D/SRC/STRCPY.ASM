;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
	public strcpy_
strcpy_:
	lxi	h,5
	dad	sp
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
	ldax	d		;while (*s1++ = *s2++) ;
	mov	m,a
	ora	a
	jz	done
	inx	d
	inx	h		;++s2
	jmp	cpyloop	;}
done:
	pop	h		;return target address
	mov	a,h
	ora	l
	ret
	end
