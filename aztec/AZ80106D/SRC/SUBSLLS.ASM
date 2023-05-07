; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
;
	public	.lls		;shift primary left by secondary
.lls:
	lda	lnsec
	ani	03fH		;restrict to 63 bits
	rz
	lhld	lnprm
	xchg
	lhld	lnprm+2		;DE has low word, HL has high word
lsloop:
	dad	h		;shift high word
	xchg
	dad	h		;shift low word
	xchg
	jnc	lsnc
	inr	l		;carry into high word
lsnc:
	dcr	a
	jnz	lsloop
	shld	lnprm+2		;put back high word
	xchg
	shld	lnprm
	ret
	end
