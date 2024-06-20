; i8080 version of BYTE's sieve
; copy / assemble, link, and run on cp/m using:
;     r sieve.asm      -- cp/m emulators like Altair sometimes allow reading files like this.
;     asm sieve
;     load sieve
;     sieve
; replicate this:
;        #define TRUE 1
;        #define FALSE 0
;        #define SIZE 8190
;        
;        char flags[SIZE+1];
;        
;        int main()
;                {
;                int i,k;
;                int prime,count,iter;
;        
;                for (iter = 1; iter <= 10; iter++) {    /* do program 10 times */
;                        count = 0;                      /* initialize prime counter */
;                        for (i = 0; i <= SIZE; i++)     /* set all flags true */
;                                flags[i] = TRUE;
;                        for (i = 0; i <= SIZE; i++) {
;                                if (flags[i]) {         /* found a prime */
;                                        prime = i + i + 3;      /* twice index + 3 */
;                                        for (k = i + prime; k <= SIZE; k += prime)
;                                                flags[k] = FALSE;       /* kill all multiples */
;                                        count++;                /* primes found */
;                                        }
;                                }
;                        }
;                printf("%d primes.\n",count);           /*primes found in 10th pass */
;                return 0;
;                }
;

; cp/m-specific constants

BDOS  equ 5
WCONF equ 2
PRSTR equ 9

; app constants

true   equ 1
false  equ 0
loops  equ 10
size   equ 8190

sizehi equ size SHR 8
sizelo equ size AND 0ffh

sizep1hi equ ( size + 1 ) SHR 8     ; size plus 1 high
sizep1lo equ ( size + 1 ) AND 0ffh  ; size plus 1 low

org     100h
        mvi        c, ( afterFlags AND 0ffh ) ; constant for performance

  iteragain:                        ; for ( iter = 1; iter <= 10; iter++ )
        lxi        h, 0             ; count = 0
        shld       COUNT

        ; set all array entries to true:  for (i = 0; i <= SIZE; i++) flags[i] = TRUE;

        lxi        h, flags
        lxi        d, afterflags
        mvi        b, true          ; "mov m, b" is faster than "mvi m, true" by 3 cycles, so set this up

  nexttrue:
        mov        m, b
        inx        h
        mov        a, l
        cmp        e
        jnz        nexttrue
        mov        a, h
        cmp        d
        jnz        nexttrue

        ; iterate through array entries and count primes

        mvi        b, 0             ; handy place for 0 byte
        lxi        d, 0             ; d is "i" in the outer for loop

  nextprime:                        ; for (i = 0; i <= SIZE; i++) {
        xra        a
        lxi        h, flags
        dad        d
        cmp        m                ; if (flags[i]) 
        jz         flagisoff

        push       d                ; save "i" incrementing towards SIZE in outer for loop
        shld       PKFLAGS          ; temporarily save & ( flags[ k ] )
                                   
        lxi        h, 3             ; compute prime, which is added to k each inner loop
        dad        d                ; prime = i + i + 3
        dad        d
        xchg
        lhld       PKFLAGS

  kloop:                            ; hl = pointer to k'th element in flags. dc = prime
        dad        d                ; k += prime

        ; check if pointer to after last item in flags < pointer to k'th element in flags (hl)
        ; then exit k loop. both values are guaranteed to be positive numbers given where the app is loaded by cp/m

        mov        a, l
        sub        c                ; c has ( afterFlags AND 0ffh ). sub is faster than sui.
        mov        a, h
        sbi        ( ( afterFlags SHR 8 ) AND 0ffh )
        jnc        inccount

        mov        m, b             ; flags[ k ] = false
        jmp        kloop

  inccount:                         
        lhld       COUNT            ; count++
        inx        h
        shld       COUNT

        pop        d                ; restore "i" in outer for loop

  flagisoff:                        ; check if outer loop is done
        inx        d
        mvi        a, sizep1hi
        cmp        d
        jnz        nextprime
        mvi        a, sizep1lo
        cmp        e
        jnz        nextprime

  checknextiter:                    ; are we done iterating loops times?
        lda        ITER
        inr        a
        sta        ITER
        cpi        loops
        jnz        iteragain

        lhld       COUNT            ; print the count of primes and the string
        call       puthl
        lxi        h, STRPRIMES
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

PUTHL:                              ; print the signed 16-bit number in HL
        mov     a, h                ; Get the sign bit of the integer,
        ral                         ; which is the top bit of the high byte
        sbb     a                   ; A=00 if positive, FF if negative
        sta     NEGF                ; Store it as the negative flag
        cnz     negatehl            ; And if HL was negative, make it positive
        lxi     d, NUM              ; Load pointer to end of number string
        push    d                   ; Onto the stack
        lxi     b, -10              ; Divide by ten (by trial subtraction)
  DIGIT:
        lxi     d, -1               ; DE = quotient. There is no 16-bit subtraction,
  DGTDIV:
        dad     b                   ; so we just add a negative value,
        inx     d                   
        jc      DGTDIV              ; while that overflows.
        mvi     a, '0'+10           ; The loop runs once too much so we're 10 out
        add     l                   ; The remainder (minus 10) is in L
        xthl                        ; Swap HL with top of stack (i.e., the string pointer)
        dcx     h                   ; Go back one byte
        mov     m, a                ; And store the digit
        xthl                        ; Put the pointer back on the stack
        xchg                        ; Do all of this again with the quotient
        mov     a, h                ; If it is zero, we're done
        ora     l
        jnz     DIGIT               ; But if not, there are more digits
        mvi     c, PRSTR            ; Prepare to call CP/M and print the string
        pop     d                   ; Put the string pointer from the stack in DE
        lda     NEGF                ; See if the number was supposed to be negative
        inr     a                   
        jnz     bdos                ; If not, print the string we have and return
        dcx     d                   ; But if so, we need to add a minus in front
        mvi     a, '-'              
        stax    d                   
        jmp     bdos                ; And only then print the string. bdos will return to caller

PKFLAGS:    dw      0               ; address of flags[ k ]
COUNT:      dw      0               ; count in C code
ITER:       dw      0               ; iter in C code
NEGF:       db      0               ; Space for negative flag
            db      '-00000'            
NUM:        db      '$'             ; Space for number. cp/m strings end with a dollar sign
CRLF:       db      13,10,0
STRPRIMES:  db     ' primes.', 13, 10, 0

flags:      ds      size + 1
afterflags:

end

