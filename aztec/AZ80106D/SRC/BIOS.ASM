;Copyright (C) 1981,1982 by Manx Software Systems
BASE	equ	0
BDOS	equ	5

	extrn	.ARG1,.ARG2,.ARG3,.asave
;
	public	bios_
bios_:
	call	.asave
	call	combios
	mov	l,a
	mvi	h,0
	ret
;
	public	bioshl_
bioshl_:
	call	.asave
combios:
	lhld	.ARG1
	xchg
	lhld	BASE+1
	dcx	h
	dcx	h
	dcx	h
	dad	d
	dad	d
	dad	d
	xchg			;bios jump addr in DE

	lhld	.ARG2
	mov	b,h
	mov	c,l
	lhld	.ARG3
	xchg			;now arg3 in DE, and bios jump in HL
	pchl
	end
