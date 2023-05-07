; Copyright (C) 1984 by Manx Software Systems
; :ts=8
;	The C routine execl() in exec.c knows that this function is
;	less than 70 bytes long.  If this code is changed, then execl
;	must be changed also.
;
;	This routine is copied into an automatic array and invoked
;	there by execl().  The code is self relocating and must
;	remain so.
;
bdos	equ	5
defdma	equ	80h
tpa	equ	100h
	public	ldr__		; ldr_(&fcb, ouser)
ldr__:
	pop	d		;throw away return
	pop	b		;set up fcb address
	lxi	d,9
	dad	d		;fix hl to point to head of loop
	lxi	d,tpa
;
;	bc = fcb address
;	de = tpa address
;	hl = address of this routine
;	old user # pushed onto stack
;
	push	h		;save loop address
	push	d
	push	b
	mvi	c,26
	call	bdos
	pop	d
	push	d
	mvi	c,20
	call	bdos
	pop	b		;restore fcb address
	pop	d		;and loading addr.
	lxi	h,80h
	dad	d		;bump loading addr
	xchg
	pop	h
	push	h		;restore loop address
	ora	a		;check if eof
	rz			;if not, return to top of loop
	pop	h		;throw away return addr
	pop	d		;get old user #
	mvi	c,32
	call	bdos		;restore user #
	lxi	d,defdma
	mvi	c,26
	call	bdos
	lhld	bdos+1
	sphl
	lxi	h,0
	push	h		;set for proper return from program
	jmp	tpa
	end
