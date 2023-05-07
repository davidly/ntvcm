; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.utox
.utox:
	shld	lnprm
posconv:
	lxi	h,0
	shld	lnprm+2
	ret
;
	public	.itox
.itox:
	shld	lnprm
	mov	a,h
	ora	a
	jp	posconv
	lxi	h,-1
	shld	lnprm+2
	ret
;
	public	.xtoi
.xtoi:
	lhld	lnprm
	ret
	end
