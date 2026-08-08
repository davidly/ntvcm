; Simple printing module - CP/M port.
;
; Copyright (C) 2012-2023 Patrik Rak (patrik@raxoft.cz)
;
; This source code is released under the MIT license, see included license.txt.
;
; The only change from the original Spectrum version: printinit (was the
; ROM's CHAN-OPEN) and printchr (was RST 0x10, the ROM's print-a-character
; routine, with an ei/di dance around it since the ROM needs interrupts
; enabled for its own internal timing) are replaced with CP/M BDOS calls.
; Everything else here is pure logic with no host dependency.


printinit:  ret         ; no channel-open step needed under CP/M


print:      ex      (sp),hl
            call    printhl
            ex      (sp),hl
            ret

printhl:
.loop       ld      a,(hl)
            inc     hl
            or      a
            ret     z
            call    printchr
            jr      .loop


printdeca:  ld      h,a
            ld      b,-100
            call    .digit
            ld      b,-10
            call    .digit
            ld      b,-1

.digit      ld      a,h
            ld      l,'0'-1
.loop       inc     l
            add     a,b
            jr      c,.loop
            sub     b
            ld      h,a
            ld      a,l
            jr      printchr


printcrc:   ld      b,4

printhexs:
.loop       ld      a,(hl)
            inc     hl
            call    printhexa
            djnz    .loop
            ret


printhexa:  push    af
            rrca
            rrca
            rrca
            rrca
            call    .nibble
            pop     af

.nibble     or      0xf0
            daa
            add     a,0xa0
            adc     a,0x40

printchr:   push    af
            push    bc
            push    de
            push    hl
            ld      e,a
            ld      c,2
            call    5
            pop     hl
            pop     de
            pop     bc
            pop     af
            ret

; EOF ;
