; Copyright (C) 1982, 1983, 1984 by Manx Software Systems
; :ts=8
	extrn	lnprm,lntmp,lnsec
	extrn	.lcmp,.ulcmp
;
;
	public	.ltst		;test if primary is zero
.ltst:
	lxi	h,lnprm
	mvi	d,4
tstlp:
	mov	a,m
	ora	a
	jnz	true
	inx	h
	dcr	d
	jnz	tstlp
	jmp	false
;
;
	public .leq
.leq:
	call	.lcmp
	jz	true
false:
	lxi	h,0
	xra	a
	ret
;
	public .lne
.lne:
	call	.lcmp
	jz	false
true:
	lxi	h,1
	xra	a
	inr	a
	ret
;
	public .llt
.llt:
	call	.lcmp
	jm	true
	jmp	false
;
	public .lle
.lle:
	call	.lcmp
	jm	true
	jz	true
	jmp	false
;
	public .lge
.lge:
	call	.lcmp
	jm	false
	jmp	true
;
	public .lgt
.lgt:
	call	.lcmp
	jm	false
	jz	false
	jmp	true
;
	public .lul
.lul:
	call	.ulcmp
	jm	true
	jmp	false
;
	public .lue
.lue:
	call	.ulcmp
	jm	true
	jz	true
	jmp	false
;
	public .luf
.luf:
	call	.ulcmp
	jm	false
	jmp	true
;
	public .lug
.lug:
	call	.ulcmp
	jm	false
	jz	false
	jmp	true
	end
