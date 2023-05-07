;Copyright (C) 1981,1982 by Manx Software Systems
; :ts=8
	public	fcbinit_
fcbinit_:
	push	b
	lxi	h,4
	dad	sp
	mov	c,m		; BC contains name
	inx	h
	mov	b,m
	inx	h
	mov	e,m		; DE contains fcb address
	inx	h
	mov	d,m
;				clear name to blanks
	mov	l,e		;copy fcb address into HL
	mov	h,d
	mvi	m,0		;clear drive #
	inx	h
	mvi	a,11		;clear name and ext to blanks
clrlp:
	mvi	m,' '
	inx	h
	dcr	a
	jnz	clrlp
	mvi	a,4
zrlp:
	mvi	m,0
	inx	h
	dcr	a
	jnz	zrlp
	xchg			; now HL contains fcb addr
;
	mov	a,c
	ora	b
	jz	badname
skipbl:
	ldax	b
	cpi	' '
	jz	skip
	cpi	9
	jnz	skipdone
skip:	inx	b
	jmp	skipbl
skipdone:
;
;	push	b		;save address of name
;	mvi	d,0		;init user #
;userloop:
;	ldax	b
;	call	isdig
;	jc	userdone
;	sui	'0'
;	mov	e,a
;	mov	a,d
;	add	a		;*2
;	add	a		;*4
;	add	a		;*8
;	add	d		;*9
;	add	d		;*10
;	add	e		;add in digit
;	mov	d,a
;	inx	b
;	jmp	userloop
;userdone:
;	cpi	'/'
;	jnz	nouser
;	inx	b
;	pop	psw		;throw away saved address
;	jmp	setuser
;nouser:
;	pop	b		;restore original address
;	mvi	d,255		;set user # to default
;setuser:
;	inx	b
;	ldax	b
;	cpi	':'
;	dcx	b
;	mvi	a,0
;	jnz	nodrive
;
;	ldax	b
;	ani	127
;	cpi	'A'
;	jc	badname
;	cpi	'Z'+1
;	jnc	lowerc
;	sui	'A'-1
;	jmp	setdrive
;
;lowerc:
;	cpi	'a'
;	jc	badname
;	cpi	'z'+1
;	jnc	badname
;	sui	'a'-1
;setdrive:
;	mov	m,a
;	inx	b
;	inx	b
;nodrive:
	inx	h
;				move name in mapping to upper case
	mvi	e,8
nameskp:
	inr	e
namelp:
	ldax	b
	inx	b
	cpi	'.'
	jz	namedn
	ora	a
	jz	alldone
	dcr	e
	jz	nameskp
	call	toupper
	mov	m,a
	inx	h
	jmp	namelp
;
namedn:
	dcr	e
	mov	a,e
	add	l
	mov	l,a
	mov	a,h
	aci	0
	mov	h,a
;					move extension mapping to upper case
	mvi	e,3
extlp:
	ldax	b
	inx	b
	ora	a
	jz	alldone
	call	toupper
	mov	m,a
	inx	h
	dcr	e
	jnz	extlp
;
alldone:
;	mvi	h,0
;	mov	l,d		;return user # prefix
;	mov	a,d
;	ora	a
	lxi	h,0
	xra	a
	pop	b
	ret
;
badname:
	lxi	h,-1
	mov	a,h
	ora	a
	pop	b
	ret
;
toupper:
	cpi	'*'
	jnz	nostar
	dcx	b		;back up so we see star again
	mvi	a,'?'		;and map into question
	ret
nostar:
	cpi	'a'
	rc
	cpi	'z'+1
	rnc
	sui	'a'-'A'
	ret
;
;isdig:
;	cpi	'0'
;	rc
;	cpi	'9'+1
;	jnc	notdig
;	ora	a
;	ret
;notdig:
;	stc
;	ret
;
	end
