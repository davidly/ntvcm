 .z80

		public movmem_

;*
;* movmem_() = copia un blocco di memoria
;*

movmem_:	push ix
		push af
		push bc
		push de
		push hl

		ld ix,12
		add ix,sp
		ld l,(ix+0)		;inizio memoria come sorgente
		ld h,(ix+1)
		ld e,(ix+2)		;inizio memoria come destinazione
		ld d,(ix+3)		
		ld c,(ix+4)		;estensione del blocco da trasferire
		ld b,(ix+5)
		ld a,b
		or c			;controllo se BC = 0 !
		jr z,$3

; dispatcher

		ld a,h
		cp d
		jr nz,$0
		ld a,l
		cp e
$0		jr z,$3
		jr nc,$2

; ( indirizzo sorgente < indirizzo destinazione )  ==>  lddr

$1		add hl,bc
		dec hl
		ex de,hl
		add hl,bc
		dec hl
		ex de,hl
		lddr
		jr $3

; ( indirizzo sorgente > indirizzo destinazione )  ==>  ldir

$2		ldir

$3		pop hl
		pop de
		pop bc
		pop af
		pop ix
		ret
