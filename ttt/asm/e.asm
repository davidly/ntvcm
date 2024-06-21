; 8080 assembly to compute e to n digits.
; When DIGITS is 200, the first 192 digits are displayed.
;
; equivalent to:
;     program e;
;     const
;        DIGITS = 200;
;     type
;         arrayType = array[ 0..DIGITS ] of integer;
;     var 
;         high, n, x : integer;
;         a : arrayType;
;     begin
;         high := DIGITS;
;         x := 0;
;         n := high - 1;
;         while n > 0 do begin
;             a[ n ] := 1;
;             n := n - 1;
;         end;
;     
;         a[ 1 ] := 2;
;         a[ 0 ] := 0;
;     
;         while high > 9 do begin
;             high := high - 1;
;             n := high;
;             while 0 <> n do begin
;                 a[ n ] := x MOD n;
;                 x := 10 * a[ n - 1 ] + x DIV n;
;                 n := n - 1;
;             end;
;     
;             Write( x );
;         end;
;     
;         writeln; writeln( 'done' );
;     end.

; cp/m-specific constants

BDOS  equ 5
WCONF equ 2
PRSTR equ 9

; app constants

true   equ 1
false  equ 0
digits equ 200

org     100h
        ; why is the loop backwards? no idea.
        ; while n > 0 do begin
        ;     a[ n ] := 1;
        ;     n := n - 1;
        ; end;

        ; register c is n. hl is a[]. b is a constant 1

        mvi        b, 1
        mvi        c, DIGITS - 1
        lxi        h, array + DIGITS - 1

  ainit:
        mov        m, b
        dcx        h
        dcr        c
        jnz        ainit

        ; a[ 1 ] := 2;
        ; a[ 0 ] := 0;

        lxi        h, array + 1
        mvi        m, 2
        dcx        h
        mvi        m, 0

        ; register c is n. register b is high. register hl is x at the start of each inner loop
       
        mvi        b, DIGITS

  outerloop:
        dcr        b                ; high := high - 1;
        mov        c, b             ; n := high;
        lxi        h, 0             ; x := 0;

  innerloop:
        xchg
        mvi        h, 0
        mov        l, c
        call       udiv             ; computes both x MOD n and x DIV n
        push       h                ; save result of the division on the stack
        lda        DIVREM           ; remainder is guaranteed to fit in one byte

        lxi        h, array
        mvi        d, 0
        mov        e, c
        dad        d                ; make hl point to a[ n ]
        mov        m, a             ; a[ n ] := x MOD n;

        dcx        h                ; make hl point to a[ n - 1 ]
        mov        e, m             ; put a[ n - 1 ] in de
        mvi        d, 0
        mov        h, d             ; put the typically smaller mul argument (10) in hl for better performance
        mvi        l, 10
        call       umul             ; hl = 10 * a[ n - 1 ]
        pop        d                ; restore the result of the division
        dad        d                ; add the two parts together. x (aka hl) := (mul result + div result)

        dcr        c                ; n := n - 1;
        jnz        innerloop        ; if n isn't 0 then loop again

        call       PrintNumber      ; show the next digit(s) of e
        
        mvi        a, 9
        cmp        b                ; while high > 9 do begin (re-written as a do until loop)
        jnz        outerloop

        lxi        h, CRLF
        call       display
        lxi        h, STRDONE
        call       display
        ret                         ; return to CP/M

; negate the hl register pair.
; negate using complement then add 1

negatehl:
        mov      a, h
        cma
        mov      h, a
        mov      a, l
        cma
        mov      l, a
        inx      h
        ret

DisplayOneCharacter:                ; display the character in a
        push    b ! push d ! push h
        mvi     c, WCONF
        mov     e, a
        call    BDOS

        pop     h ! pop d ! pop b
        ret

DisplayDigit:                       ; Argument # 0-9 is in register B
        push    b ! push d ! push h
        mvi     a, 48
        add     b
        call    DisplayOneCharacter

        pop     h ! pop d ! pop b
        ret

DISPLAY:                            ; display null-terminated string pointed to by hl
        push    b ! push d ! push h
        mov     b, h
        mov     c, l

  DNEXT:
        ldax    b
        cpi     0
        jz      DDONE
        call    DisplayOneCharacter
        inx     b
        jmp     DNEXT

  DDONE:
        pop     h ! pop d ! pop b
        ret

PrintNumber:
        push    b ! push d ! push h
        call    puthl
        pop     h ! pop d ! pop b
        ret

PUTHL:                              ; print the signed 16-bit number in HL
        mov     a, h                ; Get the sign bit of the integer,
        ral                         ; which is the top bit of the high byte
        sbb     a                   ; A=00 if positive, FF if negative
        sta     NEGF                ; Store it as the negative flag
        cnz     negatehl            ; And if HL was negative, make it positive
        lxi     d, NUM              ; Load pointer to end of number string
        push    d                   ; Onto the stack
        lxi     b, -10              ; Divide by ten (by trial subtraction)
  PUTHL$DIGIT:
        lxi     d, -1               ; DE = quotient. There is no 16-bit subtraction,
  PUTHL$DGTDIV:
        dad     b                   ; so we just add a negative value,
        inx     d                   
        jc      PUTHL$DGTDIV        ; while that overflows.
        mvi     a, '0'+10           ; The loop runs once too much so we're 10 out
        add     l                   ; The remainder (minus 10) is in L
        xthl                        ; Swap HL with top of stack (i.e., the string pointer)
        dcx     h                   ; Go back one byte
        mov     m, a                ; And store the digit
        xthl                        ; Put the pointer back on the stack
        xchg                        ; Do all of this again with the quotient
        mov     a, h                ; If it is zero, we're done
        ora     l
        jnz     PUTHL$DIGIT         ; But if not, there are more digits
        mvi     c, PRSTR            ; Prepare to call CP/M and print the string
        pop     d                   ; Put the string pointer from the stack in DE
        lda     NEGF                ; See if the number was supposed to be negative
        inr     a                   
        jnz     bdos                ; If not, print the string we have and return
        dcx     d                   ; But if so, we need to add a minus in front
        mvi     a, '-'              
        stax    d                   
        jmp     bdos                ; And only then print the string. bdos will return to caller

; unsigned multiply de by hl, result in hl
umul:
        mov     a, l
        ora     h
        rz
        push    b
        mov     b, h
        mov     c, l
        lxi     h, 0
  umul$loop:
        dad     d
        dcx     b
        mov     a, b
        ora     c
        jnz     umul$loop
        pop     b
        ret

; unsigned divide de by hl, result in hl. remainder in divRem. divide by 0 not checked.
udiv:
        mov     a, e
        ora     d
        jnz     udiv$notzero
        lxi     h, 0
        shld    divRem              ; 0 mod x is 0
        ret
  udiv$notzero:
        push    b
        xchg
        lxi     b, 0
  udiv$loop:
        mov     a, l
        sub     e
        mov     l, a
        mov     a, h
        sbb     d
        mov     h, a
        jc      udiv$done
        inx     b
        jmp     udiv$loop
  udiv$done:
        dad     d
        shld    divRem
        mov     l, c
        mov     h, b
        pop     b
        ret

NEGF:       db      0               ; Space for negative flag
            db      '-00000'            
NUM:        db      '$'             ; Space for number. cp/m strings end with a dollar sign
CRLF:       db      13,10,0
STRDONE:    db      'done.', 13, 10, 0

DIVREM:     dw      0
ARRAY:      ds      DIGITS

end


; unused code
IF 0
; negate the de register pair. handy for idiv and imul
; negate using complement then add 1
neg$de:
       mov     a, d
       cma
       mov     d, a
       mov     a, e
       cma
       mov     e, a
       inx     d
       ret

; negate the hl register pair. handy for idiv and imul
; negate using complement then add 1
neg$hl:
        mov     a, h
        cma
        mov     h, a
        mov     a, l
        cma
        mov     l, a
        inx     h
        ret

; signed multiply de by hl, result in hl
imul:
        mov     a, l
        cpi     0
        jnz     imul$notzero
        mov     a, h
        cpi     0
        jnz     imul$notzero
        ret
  imul$notzero:
        push    b ! push d
        mvi     b, 80h
        mov     a, h
        ana     b
        jz      imul$notneg
        call    neg$hl
        call    neg$de
  imul$notneg:
        mov     b, h
        mov     c, l
        lxi     h, 0
        shld    mulTmp
  imul$loop:
        dad     d
        jnc     imul$done
        push    h
        lhld    mulTmp
        inx     h
        shld    mulTmp
        pop     h
  imul$done:
        dcx     b
        mov     a, b
        ora     c
        jnz     imul$loop
        pop     d ! pop b
        ret

; signed divide de by hl, result in hl. remainder in divRem
idiv:
        xra     a
        cmp     e
        jnz     idiv$notzero
        cmp     d
        jnz     idiv$notzero
        lxi     h, 0
        shld    divRem              ; 0 mod x is 0
        ret

  idiv$notzero:
        push    b ! push d
        xchg
        mvi     c, 0
        mvi     b, 80h
        mov     a, d
        ana     b
        jz      idiv$denotneg
        inr     c
        call    neg$de
  idiv$denotneg:
        mov     a, h
        ana     b
        jz      idiv$hlnotneg
        inr     c
        call    neg$hl
  idiv$hlnotneg:
        push    b
        lxi     b, 0
  idiv$loop:
        mov     a, l
        sub     e
        mov     l, a
        mov     a, h
        sbb     d
        mov     h, a
        jc      idiv$done
        inx     b
        jmp     idiv$loop
  idiv$done:
        dad     d
        shld    divRem
        mov     l, c
        mov     h, b
        pop     b
        mov     a, c
        ani     1
        cnz     neg$hl
        pop     d ! pop b
        ret
endif





