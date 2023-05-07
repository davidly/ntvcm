;Copyright (C) 1981,1982 by Manx Software Systems
; Copyright (C) 1981  Thomas Fenwick
; :ts=8
	public .ug,.ul
.ul:		; ult
	XCHG
.ug: mov a,l
	sub e
	mov a,h
	sbb d
	mvi a,0
	aci 0
	mov l,a
	mvi h,0
	ret
;
	end
