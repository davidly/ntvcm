;Copyright (C) 1981,1982 by Manx Software Systems
;Copyright (C) 1983,1984 by Manx Software Systems
; :ts=8
	extrn	$MEMRY, sbot
;
; sbrk(size): return address of current top & bump by size bytes
;
	public	sbrk_
sbrk_:
	lxi	h,2
	dad	sp
	mov	e,m		; get size to allocate
	inx	h
	mov	d,m
	lhld	$MEMRY
	dad	d
	jc	sbrk.ov
	xchg		;save for compare
	lhld	sbot
	mov	a,l		;check for stack/heap overflow
	sub	e
	mov	a,h
	sbb	d
	jc	sbrk.ov
	lhld	$MEMRY	;get old value
	xchg
	shld	$MEMRY	;new value is good so save it away
	xchg		;return original value
	mov	a,h
	ora	l
	ret
; no space left!!
sbrk.ov:
	lxi	h,-1
	xra	a
	dcr	a
	ret
	end
