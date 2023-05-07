; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public .lst		;store long at addr in HL
.lst:
	lxi	d,lnprm
	ldax	d
	mov	m,a
	inx	h
	inx	d
	ldax	d
	mov	m,a
	inx	h
	inx	d
	ldax	d
	mov	m,a
	inx	h
	inx	d
	ldax	d
	mov	m,a
	ret
	end
