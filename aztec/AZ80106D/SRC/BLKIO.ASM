; Copyright (C) 1982, 1983 by Manx Software Systems
; :ts=8
BDOS	equ	5
	extrn	errno_
	extrn	.asave,.ARG1,.ARG2,.ARG3
	public blkrd_
blkrd_:
	call	.asave
	mvi	c,33		;set function to read sequential
	jmp	rdwrt
;
	public	blkwr_
blkwr_:
	call	.asave
	mvi	c,34		;set function to write sequential
rdwrt:
	push	b
ioloop:
	lhld	.ARG2
	xchg
	lxi	h,128
	dad	d			;bump address to next sector
	shld	.ARG2
	mvi	c,26		;set DMA address
	call	BDOS
	pop	b
	push	b
	lhld	.ARG1
	xchg
	call	BDOS	;read or write sector
	ora	a
	jnz	ioerr
	lhld	.ARG1
	lxi	d,33
	dad	d
	inr	m
	jnz	nocarry
	inx	h
	inr	m
nocarry:
	lhld	.ARG3
	dcx	h
	shld	.ARG3
	mov	a,l
	ora	h
	jnz	ioloop
	pop	b		;pull function code from stack
	ret				;all done, return number remaining
;
ioerr:
	cpi	1
	jz	dontset
	cpi	4
	jz	dontset
	mov	l,a
	mvi	h,0
	shld	errno_
dontset:
	pop	b		;pull function code from stack
	lhld	.ARG3
	ret				;return number remaining
	end
