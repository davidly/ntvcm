; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
;
	public	.llis		;load long immediate secondary
.llis:
	pop	d		;get return addr
	lxi	h,4		;size of long
	dad	d
	push	h		;put back correct return addr
	xchg
			;fall through into .llds
;
	public	.llds		;load long into secondary accum
.llds:
	lxi	d,lnsec
	jmp	lload
;
	public	.llip		;load long immediate primary
.llip:
	pop	d		;get return addr
	lxi	h,4		;size of long
	dad	d
	push	h		;put back correct return addr
	xchg
			;fall through into .lldp
;
	public .lldp		;load long into primary accum
.lldp:
	lxi	d,lnprm
lload:
	mov	a,m
	stax	d
	inx	d
	inx	h
	mov	a,m
	stax	d
	inx	d
	inx	h
	mov	a,m
	stax	d
	inx	d
	inx	h
	mov	a,m
	stax	d
	ret
	end
