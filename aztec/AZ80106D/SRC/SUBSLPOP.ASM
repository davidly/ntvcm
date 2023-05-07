; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
	public .lpsh		;push long onto the stack
.lpsh:				;from the primary accumulator
	pop	d		;get return address
	lxi	h,lnprm+3
	lhld	lnprm+2
	push	h
	lhld	lnprm
	push	h
	xchg
	pchl
;
	public	.lpop		;pop long into secondary accum
.lpop:
	pop	d		;get return address
	pop	h		;bytes 0 and 1
	shld	lnsec
	pop	h
	shld	lnsec+2
	xchg
	pchl
;
	public	.lswap		;exchange primary and secondary
.lswap:
	lhld	lnsec
	xchg
	lhld	lnprm
	shld	lnsec
	xchg
	shld	lnprm
	lhld	lnsec+2
	xchg
	lhld	lnprm+2
	shld	lnsec+2
	xchg
	shld	lnprm+2
	ret
	end
