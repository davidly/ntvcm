; i8080 version of an app that proves you can't win at tic-tac-toe
; this app takes one optional argument -- the # of iterations
;
; copy / assemble, link, and run on cp/m using:
; r tttcpm.asm      -- cp/m emulators sometimes allow reading files like this.
; asm tttcpm
; load tttcpm
; tttcpm
;
; The board positions:
;   0 1 2
;   3 4 5
;   6 7 8
;

; cp/m-specific constants

BDOS  equ 5
WCONF equ 2
PRSTR equ 9

; app constants

DEFLOOPS    equ     1               ; default # of times to run (max 32767)
XSCO        equ     9               ; maximum score
NSCO        equ     2               ; minimum score
WSCO        equ     6               ; winning score
TSCO        equ     5               ; tie score
LSCO        equ     4               ; losing score
XPIECE      equ     1               ; X move piece
OPIECE      equ     2               ; Y move piece
BLANKPIECE  equ     0               ; empty move piece
TAILCOUNT   equ     128             ; where the # of bytes in the command tail is located

org     100h
        lxi     h, DEFLOOPS         ; setup the default number of iterations
        shld    LOOPS               ; in the global variable LOOPS

        lda     TAILCOUNT           ; was there a command-line argument?
        cpi     0
        jz      Again               ; if not, use the default value for LOOPS
        mvi     d, 0                ; get ready to null-terminate the string
        mov     e, a                ; # of characters
        lxi     h, TAILCOUNT + 1    ; start of the string
        dad     d                   ; now hl points just beyond the argument
        mvi     m, 0                ; null-terminate the tail string
        lxi     h, TAILCOUNT + 1    ; convert the tail string in hl to a number in hl
        call    atou                ; null-terminated string in hl => unsigned integer in hl
        shld    LOOPS               ; store the value for use in the loop below

        mov     a, l                ; if they asked for 0 or it wasn't a number, do nothing
        cpi     0
        jnz     Again
        mov     a, h
        cpi     0
        jnz     Again
        lxi     h, STRINVARG        ; the value is 0 then so show an error and exit
        call    DISPLY
        jmp     0                    

  Again:
        lxi     h, 0
        shld    MOVES               ; set to 0 each iteration to avoid overflow
        xra     a
        sta     V
        sta     DEPTH
        sta     ALPHA
        sta     BETA

        mvi     a, 0
        call    RUNMINMAX           ; first of 3 unique board configurations

        mvi     a, 1
        call    RUNMINMAX           ; second

        mvi     a, 4
        call    RUNMINMAX           ; third
                                 
        lhld    ITERS               ; increment iteration count and loop until done
        inx     h
        shld    ITERS
        xchg
        lhld    LOOPS
        mov     a, h
        cmp     d
        jnz     AGAIN
        mov     a, l
        cmp     e
        jnz     AGAIN

        lxi     h, STRMOVES         ; all done -- display related information
        call    DISPLY
        lhld    MOVES
        call    PUTHL
        lxi     h, CRLF
        call    DISPLY

        lxi     h, STRITERS
        call    DISPLY
        lhld    LOOPS
        call    PUTHL
        lxi     h, CRLF
        call    DISPLY

        jmp     0                   ; cp/m call to terminate the app

RUNMINMAX:                          ; Run the MINMAX function for a given first move
        mvi     b, 0                ; store the first move
        mov     c, a
        lxi     h, BOARD
        dad     b
        mvi     m, XPIECE
        push    h                   ; save the pointer to the move location for later

        xra     a                   ; depth
        mov     b, c                ; move position
        mvi     l, NSCO             ; alpha
        mvi     h, XSCO             ; beta
        call    MINIMIZE

        pop     h                   ; restore the move location
        mvi     m, BLANKPIECE       ; restore a blank on the board

        ret

; The 8080 has no simple way to address arguments or local variables relative to sp.
; The approach here is to:
;   1) pass arguments in registers:
;           a - depth
;           b - move position
;           l - alpha
;           h - beta
;   2) store the arguments in global variables so they are easy to access
;   3) when making a call, push the global variables on the stack
;   4) when returning froma call, restore the global variables with popped stack values
;   5) for tail functions, just pass arguments in registers with no stack or global usage for arguments
; The extra copies to/from globals are expensive, but overall faster since accessing them is fast.

MAXIMIZE:                           ; the recursive scoring function
        sta     DEPTH
        shld    ALPHA               ; write alpha and beta

        lhld    MOVES               ; no 16-bit memory increment, so load in hl for that
        inx     h
        shld    MOVES

        cpi     4                   ; DEPTH - 4  (if 4 or fewer pieces played, no possible winner)
        jm      XSKIPWIN

        mov     a, b                ; where the move was taken 0..8
        mvi     b, OPIECE           ; the piece that took the move
        call    CALLSCOREPROC       ; look for a winning position

        cpi     OPIECE              ; see if O won
        mvi     a, LSCO             ; losing score since O won
        rz

XSKIPWIN:
        mvi     a, NSCO             ; maximizing odd depths, so start with minimum score
        sta     V
        lxi     d, 00ffh            ; the variable I will go from 0..8. start at -1

XLOOP:
        mov     a, e
        cpi     8
        jz      XDONE
        inr     e
        lxi     h, BOARD
        dad     d
        xra     a                   ; BLANKPIECE is 0
        cmp     m
        jnz     XLOOP

        mvi     m, XPIECE           ; make the move

        ; save state, recurse, and restore state

        push    d                   ; save i in the for loop
        lhld    V                   ; V in l and DEPTH in h
        push    h
        mov     b, e                ; board position of the latest move
        mov     a, h                ; depth
        inr     a
        lhld    ALPHA               ; alpha in l and beta in h
        push    h

        ; C = depth, B = move position, L = alpha, H = beta  ====> A = return score

        call    MINIMIZE

        pop     h
        shld    ALPHA               ; restore ALPHA and BETA
        pop     h                   ; restore state after recursion
        shld    V                   ; restore V and DEPTH
        pop     d                   ; restore i in the for loop

        lxi     h, BOARD
        dad     d
        mvi     m, BLANKPIECE       ; restore the 0 on the board where the turn was placed

        cpi     WSCO                ; SC - WSCO. If zero, can't do better.
        rz

        lxi     h, V
        cmp     m                   ; compare score with value
        jz      XLOOP               ; no j <= instruction on 8080
        jm      XLOOP

        mov     m, a                ; update value with score
        lxi     h, BETA
        cmp     m                   ; compare value with beta
        rp                          ; beta pruning

        lxi     h, ALPHA            ; save the address of alpha
        cmp     m                   ; compare value with alpha
        jz      XLOOP               ; no j <= instruction on 8080
        jm      XLOOP

        mov     m, a                ; update alpha with value
        jmp     XLOOP

XDONE:
        lda     V
        ret

MINIMIZE:                           ; the recursive scoring function
        sta     DEPTH
        shld    ALPHA               ; write alpha and beta

        lhld    MOVES               ; no 16-bit memory increment, so load in hl for that
        inx     h
        shld    MOVES

        cpi     4                   ; DEPTH - 4  (if 4 or fewer pieces played, no possible winner)
        jm      NSKIPWIN

        mov     a, b                ; where the move was taken 0..8
        mvi     b, XPIECE           ; the piece that took the move
        call    CALLSCOREPROC       ; look for a winning position

        cpi     XPIECE              ; see if X won
        mvi     a, WSCO             ; winning score. avoid branch by always loading
        rz

        lda     DEPTH               ; check if at the bottom of the recursion
        cpi     8
        mvi     a, TSCO             ; tie score. avoid branch by always loading
        rz

NSKIPWIN:
        mvi     a, XSCO             ; minimizing, so start with maximum score
        sta     V
        lxi     d, 00ffh            ; the variable I will go from 0..8. start at -1

NLOOP:
        mov     a, e
        cpi     8
        jz      NDONE
        inr     e
        lxi     h, BOARD
        dad     d
        xra     a                   ; BLANKPIECE is 0
        cmp     m
        jnz     NLOOP

        mvi     m, OPIECE           ; make the move

        ; save state, recurse, and restore state

        push    d                   ; save i in the for loop
        lhld    V                   ; V in l and DEPTH in h
        push    h
        mov     b, e                ; board position of the latest move
        mov     a, h                ; depth
        inr     a
        lhld    ALPHA               ; alpha in l and beta in h
        push    h

        ; A = depth, B = move position, L = alpha, H = beta  ====> A = return score

        call    MAXIMIZE

        pop     h
        shld    ALPHA               ; restore ALPHA and BETA
        pop     h                   ; restore state after recursion
        shld    V                   ; restore V and DEPTH
        pop     d                   ; restore i in the for loop

        lxi     h, BOARD
        dad     d
        mvi     m, BLANKPIECE       ; restore the 0 on the board where the turn was placed

        cpi     LSCO                ; SC - LSCO. If zero, can't do worse.
        rz

        lxi     h, V
        cmp     m                   ; compare score with value
        jp      NLOOP

        mov     m, a                ; update value with score
        lxi     h, ALPHA
        cmp     m                   ; compare value with alpha
        rz                          ; alpha pruning
        rmi                         ; alpha pruning

        lxi     h, BETA             ; save address of beta
        cmp     m                   ; compare value with beta
        jp      NLOOP

        mov     m, a                ; update beta with value
        jmp     NLOOP

NDONE:
        lda     V
        ret

; a = the proc to call 0..8
; b = the player who just took a move, O or X
CALLSCOREPROC:
        add      a                  ; double the move position because function pointers are two bytes
        lxi      h, WINPROCS        ; load the pointer to the list of function pointers 0..8

;        mvi      d, 0              ; to enable winprocs to span a 256-byte page, use this instead.
;        mov      e, a              ; this method results in overall 1.5% more cpu cycles.
;        dad      d                 ; mvi is 7 cycles. dad is 10, add is 4, mov is 7. 27 > 11
        add      l                  ; only add the lower-byte since winprocs fits in a single 256-byte page
        mov      l, a

        mov      e, m               ; load the low byte of the proc address
        inx      h                  ; increment the pointer to the next byte. inx and inr are both 5 cycles.
        mov      d, m               ; load the high byte of the proc address
        xchg                        ; exchange de and hl
        mov      a, b               ; put the player move (X or O) in a
        pchl                        ; move the winner proc address from hl to pc (jump to it)

proc0:
        lxi      h, BOARD + 1
        cmp      m
        jnz      proc0nextwin
        inx      h ; to 2
        cmp      m
        rz

  proc0nextwin:
        lxi      h, BOARD + 3
        cmp      m
        jnz      proc0nextwin2
        lxi      h, BOARD + 6
        cmp      m
        rz

  proc0nextwin2:
        lxi      h, BOARD + 4
        cmp      m
        jnz      proc0no
        lxi      h, BOARD + 8
        cmp      m
        rz

  proc0no:
        xra      a
        ret
        
proc1:
        lxi      h, BOARD + 0
        cmp      m
        jnz      proc1nextwin
        lxi      h, BOARD + 2
        cmp      m
        rz

  proc1nextwin:
        lxi      h, BOARD + 4
        cmp      m
        jnz      proc1no
        lxi      h, BOARD + 7
        cmp      m
        rz

  proc1no:
        xra      a
        ret
        
proc2:
        lxi      h, BOARD + 0
        cmp      m
        jnz      proc2nextwin
        inx      h ; to 1
        cmp      m
        rz

  proc2nextwin:
        lxi      h, BOARD + 5
        cmp      m
        jnz      proc2nextwin2
        lxi      h, BOARD + 8
        cmp      m
        rz

  proc2nextwin2:
        lxi      h, BOARD + 4
        cmp      m
        jnz      proc2no
        lxi      h, BOARD + 6
        cmp      m
        rz

  proc2no:
        xra      a
        ret
        
proc3:
        lxi      h, BOARD + 0
        cmp      m
        jnz      proc3nextwin
        lxi      h, BOARD + 6
        cmp      m
        rz

  proc3nextwin:
        lxi      h, BOARD + 4
        cmp      m
        jnz      proc3no
        lxi      h, BOARD + 5
        cmp      m
        rz

  proc3no:
        xra      a
        ret
        
proc4:
        lxi      h, BOARD + 0
        cmp      m
        jnz      proc4nextwin
        lxi      h, BOARD + 8
        cmp      m
        rz

  proc4nextwin:
        lxi      h, BOARD + 2
        cmp      m
        jnz      proc4nextwin2
        lxi      h, BOARD + 6
        cmp      m
        rz

  proc4nextwin2:
        lxi      h, BOARD + 1
        cmp      m
        jnz      proc4nextwin3
        lxi      h, BOARD + 7
        cmp      m
        rz

  proc4nextwin3:
        lxi      h, BOARD + 3
        cmp      m
        jnz      proc4no
        lxi      h, BOARD + 5
        cmp      m
        rz

  proc4no:
        xra      a
        ret

proc5:
        lxi      h, BOARD + 3
        cmp      m
        jnz      proc5nextwin
        inx      h ; to 4
        cmp      m
        rz

  proc5nextwin:
        lxi      h, BOARD + 2
        cmp      m
        jnz      proc5no
        lxi      h, BOARD + 8
        cmp      m
        rz

  proc5no:
        xra      a
        ret

proc6:
        lxi      h, BOARD + 4
        cmp      m
        jnz      proc6nextwin
        lxi      h, BOARD + 2
        cmp      m
        rz

  proc6nextwin:
        lxi      h, BOARD + 0
        cmp      m
        jnz      proc6nextwin2
        lxi      h, BOARD + 3
        cmp      m
        rz

  proc6nextwin2:
        lxi      h, BOARD + 7
        cmp      m
        jnz      proc6no
        inx      h ; to 8
        cmp      m
        rz

  proc6no:
        xra      a
        ret
        
proc7:
        lxi      h, BOARD + 1
        cmp      m
        jnz      proc7nextwin
        lxi      h, BOARD + 4
        cmp      m
        rz

  proc7nextwin:
        lxi      h, BOARD + 6
        cmp      m
        jnz      proc7no
        lxi      h, BOARD + 8
        cmp      m
        rz

  proc7no:
        xra      a
        ret
        
proc8:
        lxi      h, BOARD + 0
        cmp      m
        jnz      proc8nextwin
        lxi      h, BOARD + 4
        cmp      m
        rz

  proc8nextwin:
        lxi      h, BOARD + 2
        cmp      m
        jnz      proc8nextwin2
        lxi      h, BOARD + 5
        cmp      m
        rz

  proc8nextwin2:
        lxi      h, BOARD + 6
        cmp      m
        jnz      proc8no
        inx      h ; to 7
        cmp      m
        rz

  proc8no:
        xra      a
        ret

; negate the de register pair.
; negate using complement then add 1

negatede:
        mov      a, d
        cma
        mov      d, a
        mov      a, e
        cma
        mov      e, a
        inx      d
        ret

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

; multiply de by hl, result in hl
; incredibly slow iterative addition.

imul:
        mov      a, l               ; first check if hl is 0 and just return if so
        cpi      0
        jnz      mul$start
        mov      a, h
        cpi      0
        jnz      mul$start
        ret

  mul$start:
        push     b
        mvi      b, 80h
        mov      a, h
        ana      b
        jz       mul$notneg
        call     negatehl
        call     negatede
  mul$notneg:
        push     h
        pop      b
        lxi      h, 0
        shld     mulTmp
  mul$loop:
        dad      d
        jnc      mul$done
        push     h
        lhld     mulTmp
        inx      h
        shld     mulTmp
        pop      h
  mul$done:
        dcx      b
        mov      a, b
        ora      c
        jnz      mul$loop
        pop      b
        ret

atou:                               ; in: hl points to string. out: hl has integer value. positive base-10 is assumed
        push   b
        push   d
        lxi    b, 0                 ; running total is in bc

  atouSpaceLoop:                    ; skip past spaces
        mov    a, m
        cpi    ' '
        jnz    atouNext
        inx    h
        jmp    atouSpaceLoop

  atouNext:
        mov    a, m                 ; check if we're at the end of string or the data isn't a number
        cpi    '0'
        jm     atouDone             ; < '0' isn't a digit
        cpi    '9' + 1
        jp     atouDone             ; > '9' isn't a digit

        lxi    d, 10                ; multiply what we have so far by 10
        push   h
        mov    h, b
        mov    l, c
        call   imul
        mov    b, h
        mov    c, l
        pop    h

        mov    a, m                 ; restore the digit in a because imul trashed it
        sui    '0'                  ; change ascii to a number
        add    c                    ; add this new number to the running total in bc
        mov    c, a
        mov    a, b
        aci    0                    ; if there was a carry from the add, reflect that
        mov    b, a

        inx    h                    ; move to the next character
        jmp    atouNext             ; and process it

  atouDone:
        mov    h, b                 ; the result goes in hl
        mov    l, c
        pop    d
        pop    b
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

DISPLY:                             ; display null-terminated string pointed to by hl
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

org     500h                        ; make data 256-byte aligned so it all fits in one 256-byte area
                                    ; and math for winprocs and board can be on just the lower byte.
                                    ; the assembler won't warn if data and code overlap!
                                    ; there is no way to force alignment, which would be better.

WINPROCS:  dw      proc0, proc1, proc2, proc3, proc4, proc5, proc6, proc7, proc8
BOARD:     db      0,0,0,0,0,0,0,0,0
NEGF:      db      0                ; Space for negative flag
           db      '-00000'            
NUM:       db      '$'              ; Space for number. cp/m strings end with a dollar sign
CRLF:      db      10,13,0
STRITERS:  db     'iterations: ', 0
STRINVARG: db     'iteration argument invalid; it must be 1 - 32767', 10,13,0
STRMOVES:  db     'moves: ', 0
V:         db      0                ; value in minmax
DEPTH:     db      0                ; current depth of recursion. must be after V
ALPHA:     db      0                ; Alpha in a/b pruning
BETA:      db      0                ; Beta in a/b pruning. must be after ALPHA
MOVES:     dw      0                ; Count of moves examined (to validate the app)
ITERS:     dw      0                ; iterations of running the app so far
LOOPS:     dw      0                ; # of iterations to make
mulTmp:    dw      0

end

