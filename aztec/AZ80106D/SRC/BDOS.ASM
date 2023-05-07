;Copyright (C) 1981,1982 by Manx Software Systems
; :ts=8
BASE	equ	0
BDOS	equ	5

	extrn	.ARG1,.ARG2,.ARG3,.asave
;
	public	bdoshl_
bdoshl_:
	call	.asave
	call	combdos
	xchg			;get back original hl value
	ret
;
	public	bdos_,CPM_
bdos_:
CPM_:
	call	.asave
combdos:
	lhld	.ARG1
	mov	b,h
	mov	c,l
	lhld	.ARG2
	xchg
	CALL	BDOS
	xchg		;save for bdoshl call
	mov	l,a
	xra	a		;set zero flag
	mov	h,a
	RET
	end
