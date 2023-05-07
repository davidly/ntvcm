;Copyright (C) 1981,1982,1983 by Manx Software Systems
; :ts=8
BDOS	equ	5
	extrn Croot_
	extrn _Uorg_, _Uend_
;
	public	lnprm, lntmp, lnsec
;	
;	The 3 "bss" statements below must remain in EXACTLY the same order,
;	with no intervening statements!
;
	bss	lnprm,4
	bss	lntmp,4
	bss	lnsec,4
;
	global	sbot,2
	global	errno_,2
	global	_mbot_,2
	dseg
	public	Sysvec_
	public	_exit_
Sysvec_:	dw	0
	dw	0
	dw	0
	dw	0
	public	$MEMRY
$MEMRY:	dw	0ffffh
;
fcb:	db	0,'???????????',0,0,0,0
	ds	16
	cseg
	public	.begin
.begin:
	lxi	h,_Uorg_
	lxi	b,_Uend_-_Uorg_
	mvi	e,0
clrbss:
	mov	m,e
	inx	h
	dcx	b
	mov	a,c
	ora	b
	jnz	clrbss
;
	LHLD	BDOS+1
	SPHL
;	lxi	d,-2048
	lxi	d,-600		;set heap limit at 600 bytes below stack
	dad	d
	shld	sbot
	lhld	$MEMRY
	shld	_mbot_
	CALL	Croot_
_exit_:
	mvi	c,17		;search for first (used to flush deblock buffer)
	lxi	d,fcb
	call	BDOS
	lxi	b,0
	call	BDOS
	jmp	_exit_
;
	end	.begin
