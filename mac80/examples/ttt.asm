; PORTED TO MAC80 FROM CPM_COMPILERS/ASM/TTT.ASM, WRITTEN FOR CP/M ASM.COM.
; LABELS ARE SHORTENED TO MAC80'S FIVE SIGNIFICANT CHARACTERS, AND
; LOWERCASE TEXT IS GIVEN AS ASCII CODES. THE CODE IS BYTE-IDENTICAL.
;
; I8080 VERSION OF AN APP THAT PROVES YOU CAN'T WIN AT TIC-TAC-TOE
; THIS APP TAKES ONE OPTIONAL ARGUMENT -- THE # OF ITERATIONS
;
; THE BOARD POSITIONS:
;   0 1 2
;   3 4 5
;   6 7 8
;

; CP/M-SPECIFIC CONSTANTS

BDOS  EQU 5
WCONF EQU 2
PRSTR EQU 9

; APP CONSTANTS

DEFLP    EQU     1 ; DEFAULT # OF TIMES TO RUN (MAX 32767)
XSCO        EQU     9 ; MAXIMUM SCORE
NSCO        EQU     2 ; MINIMUM SCORE
WSCO        EQU     6 ; WINNING SCORE
TSCO        EQU     5 ; TIE SCORE
LSCO        EQU     4 ; LOSING SCORE
XPC      EQU     1 ; X MOVE PIECE
OPC      EQU     2 ; Y MOVE PIECE
BLANK  EQU     0 ; EMPTY MOVE PIECE
TAILC   EQU     128 ; WHERE THE # OF BYTES IN THE COMMAND TAIL IS LOCATED

ORG     100H
        LXI     H, DEFLP ; SETUP THE DEFAULT NUMBER OF ITERATIONS
        SHLD    LOOPS ; IN THE GLOBAL VARIABLE LOOPS

        LDA     TAILC ; WAS THERE A COMMAND-LINE ARGUMENT?
        CPI     0
        JZ      AGAIN ; IF NOT, USE THE DEFAULT VALUE FOR LOOPS
        MVI     D, 0 ; GET READY TO NULL-TERMINATE THE STRING
        MOV     E, A ; # OF CHARACTERS
        LXI     H, TAILC + 1 ; START OF THE STRING
        DAD     D ; NOW HL POINTS JUST BEYOND THE ARGUMENT
        MVI     M, 0 ; NULL-TERMINATE THE TAIL STRING
        LXI     H, TAILC + 1 ; CONVERT THE TAIL STRING IN HL TO A NUMBER IN HL
        CALL    ATOU ; NULL-TERMINATED STRING IN HL => UNSIGNED INTEGER IN HL
        SHLD    LOOPS ; STORE THE VALUE FOR USE IN THE LOOP BELOW

        MOV     A, L ; IF THEY ASKED FOR 0 OR IT WASN'T A NUMBER, DO NOTHING
        CPI     0
        JNZ     AGAIN
        MOV     A, H
        CPI     0
        JNZ     AGAIN
        LXI     H, SBADA ; THE VALUE IS 0 THEN SO SHOW AN ERROR AND EXIT
        CALL    DISP
        JMP     0

  AGAIN:
        LXI     H, 0
        SHLD    MOVES ; SET TO 0 EACH ITERATION TO AVOID OVERFLOW

        MVI     A, 0
        CALL    RUNMM ; FIRST OF 3 UNIQUE BOARD CONFIGURATIONS

        MVI     A, 1
        CALL    RUNMM ; SECOND

        MVI     A, 4
        CALL    RUNMM ; THIRD

        LHLD    ITERS ; INCREMENT ITERATION COUNT AND LOOP UNTIL DONE
        INX     H
        SHLD    ITERS
        XCHG
        LHLD    LOOPS
        MOV     A, H
        CMP     D
        JNZ     AGAIN
        MOV     A, L
        CMP     E
        JNZ     AGAIN

        LXI     H, SMOVE ; ALL DONE -- DISPLAY RELATED INFORMATION
        CALL    DISP
        LHLD    MOVES
        CALL    PUTHL
        LXI     H, CRLF
        CALL    DISP

        LXI     H, SITER
        CALL    DISP
        LHLD    LOOPS
        CALL    PUTHL
        LXI     H, CRLF
        CALL    DISP

        JMP     0 ; CP/M CALL TO TERMINATE THE APP

RUNMM: ; RUN THE MINMAX FUNCTION FOR A GIVEN FIRST MOVE
        LXI     H, BOARD
        ADD     L ; THE BOARD IS IN ONE PAGE: NO CARRY INTO H
        MOV     L, A
        MVI     M, XPC ; STORE THE FIRST MOVE
        PUSH    H ; SAVE THE POINTER TO THE MOVE LOCATION FOR LATER
        MOV     E, L ; THE MOVE, AS ITS BOARD ADDRESS LOW BYTE
        MVI     B, NSCO ; ALPHA
        MVI     C, XSCO ; BETA
        CALL    MINIM
        POP     H ; RESTORE THE MOVE LOCATION
        MVI     M, BLANK ; RESTORE A BLANK ON THE BOARD
        RET

; SEPARATE MAXIMIZE (X TO MOVE) AND MINIMIZE (O TO MOVE) ROUTINES.
; ARGUMENTS IN REGISTERS:
;   B - ALPHA     C - BETA
;   E - THE MOST RECENT MOVE, AS THE LOW BYTE OF ITS BOARD ADDRESS
; REGISTER USAGE:
;   D - THE BEST SCORE SO FAR (VALUE)
;   E - AFTER THE WIN CHECK, THE MOVE BEING TRIED. A CHILD RETURNS WITH E
;       UNCHANGED, SO THE PARENT REBUILDS HL FROM IT INSTEAD OF SAVING HL.
;   HL - THE BOARD POSITION BEING TRIED. THE BOARD IS PAGE-ALIGNED AND FOLLOWED
;        BY A ZERO SENTINEL, SO THE SCAN ONLY INCREMENTS L AND COMPARES WITH A = 0.
; A MAXIMIZE NODE ONLY RAISES ALPHA AND A MINIMIZE NODE ONLY LOWERS BETA, AND
; THE PARENT SAVES B/C AND D/E AROUND EACH CALL, SO NO GLOBALS ARE NEEDED.
; THERE IS NO DEPTH COUNTER. WITH FEWER THAN FIVE PIECES PLACED NOBODY HAS THREE
; IN A ROW, SO THE WIN CHECK SIMPLY FINDS NONE. A MINIMIZE NODE AT DEPTH 8 FINDS
; THE BOARD FULL: ITS VALUE STAYS AT THE MAXIMUM SCORE, WHICH NO OTHER NODE CAN
; PRODUCE, AND BECOMES THE TIE SCORE.

MAXIM: ; THE RECURSIVE SCORING FUNCTION
        LXI     H, MOVES ; COUNT THE MOVE
        INR     M
        JNZ     XCNT
        INR     L
        INR     M
XCNT:
        MVI     D, OPC ; THE PIECE THAT TOOK THE MOVE
        CALL    CLSCP ; LOOK FOR A WINNING POSITION
        CPI     OPC ; SEE IF O WON
        MVI     A, LSCO ; LOSING SCORE SINCE O WON
        RZ

        MVI     D, NSCO ; MAXIMIZING ODD DEPTHS, SO START WITH MINIMUM SCORE
        LXI     H, BOARD - 1

XNEXT:
        XRA     A ; BLANKPIECE IS 0
XLOOP:
        INR     L
        CMP     M ; IS THE BOARD SPACE FREE (OR THE SENTINEL)?
        JNZ     XLOOP
        MOV     A, L
        CPI     ( BOARD + 9 ) AND 0FFH ; THE SENTINEL: EVERY MOVE HAS BEEN TRIED
        JZ      XDONE

        MVI     M, XPC ; MAKE THE MOVE
        MOV     E, L ; THE MOVE FOR THE CHILD'S WIN CHECK AND OUR RESUME POINT
        PUSH    D ; SAVE VALUE IN D AND THE MOVE IN E
        PUSH    B ; SAVE ALPHA AND BETA
        CALL    MINIM
        POP     B
        POP     D
        MOV     L, E ; THE CHILD CLOBBERS HL; E HAS THE MOVE'S ADDRESS
        MVI     H, BOARD SHR 8
        MVI     M, BLANK ; RESTORE THE 0 ON THE BOARD WHERE THE TURN WAS PLACED

        CPI     WSCO ; SC - WSCO. IF ZERO, CAN'T DO BETTER.
        RZ
        CMP     D ; COMPARE SCORE WITH VALUE
        JZ      XNEXT ; SCORES ARE SMALL POSITIVE NUMBERS: UNSIGNED COMPARES
        JC      XNEXT
        CMP     C ; COMPARE SCORE WITH BETA
        RNC ; BETA PRUNING
        MOV     D, A ; UPDATE VALUE WITH SCORE
        CMP     B ; COMPARE VALUE WITH ALPHA
        JZ      XNEXT
        JC      XNEXT
        MOV     B, A ; UPDATE ALPHA WITH VALUE
        JMP     XNEXT

XDONE:
        MOV     A, D
        RET

MINIM: ; THE RECURSIVE SCORING FUNCTION
        LXI     H, MOVES ; COUNT THE MOVE
        INR     M
        JNZ     NCNT
        INR     L
        INR     M
NCNT:
        MVI     D, XPC ; THE PIECE THAT TOOK THE MOVE
        CALL    CLSCP ; LOOK FOR A WINNING POSITION
        CPI     XPC ; SEE IF X WON
        MVI     A, WSCO ; WINNING SCORE. AVOID BRANCH BY ALWAYS LOADING
        RZ

        MVI     D, XSCO ; MINIMIZING, SO START WITH MAXIMUM SCORE
        LXI     H, BOARD - 1

NNEXT:
        XRA     A ; BLANKPIECE IS 0
NLOOP:
        INR     L
        CMP     M ; IS THE BOARD SPACE FREE (OR THE SENTINEL)?
        JNZ     NLOOP
        MOV     A, L
        CPI     ( BOARD + 9 ) AND 0FFH ; THE SENTINEL: EVERY MOVE HAS BEEN TRIED
        JZ      NDONE

        MVI     M, OPC ; MAKE THE MOVE
        MOV     E, L ; THE MOVE FOR THE CHILD'S WIN CHECK AND OUR RESUME POINT
        PUSH    D ; SAVE VALUE IN D AND THE MOVE IN E
        PUSH    B ; SAVE ALPHA AND BETA
        CALL    MAXIM
        POP     B
        POP     D
        MOV     L, E ; THE CHILD CLOBBERS HL; E HAS THE MOVE'S ADDRESS
        MVI     H, BOARD SHR 8
        MVI     M, BLANK ; RESTORE THE 0 ON THE BOARD WHERE THE TURN WAS PLACED

        CPI     LSCO ; SC - LSCO. IF ZERO, CAN'T DO WORSE.
        RZ
        CMP     D ; COMPARE SCORE WITH VALUE
        JNC     NNEXT
        MOV     D, A ; UPDATE VALUE WITH SCORE
        CMP     B ; COMPARE VALUE WITH ALPHA
        RZ ; ALPHA PRUNING
        RC ; ALPHA PRUNING
        CMP     C ; COMPARE VALUE WITH BETA
        JNC     NNEXT
        MOV     C, A ; UPDATE BETA WITH VALUE
        JMP     NNEXT

NDONE:
        MOV     A, D
        CPI     XSCO ; NO MOVE WAS POSSIBLE: THE BOARD IS FULL
        RNZ
        MVI     A, TSCO ; TIE SCORE
        RET

; E = THE MOVE, AS THE LOW BYTE OF ITS BOARD ADDRESS
; D = THE PLAYER WHO JUST TOOK A MOVE, O OR X
; RETURNS THAT PIECE IN A IF IT COMPLETED A LINE, ELSE 0. PRESERVES B, C, D, E.
CLSCP:
        MOV      A, E ; THE BOARD IS AT 540H AND THE PROCS AT 580H,
        ADD      A ; SO TWICE THE BOARD ADDRESS LOW BYTE INDEXES WINPROCS
        MOV      L, A
        MVI      H, WPROC SHR 8
        MOV      A, M ; LOAD THE LOW BYTE OF THE PROC ADDRESS
        INR      L ; THE TABLE FITS IN ONE PAGE
        MOV      H, M ; LOAD THE HIGH BYTE OF THE PROC ADDRESS
        MOV      L, A
        MOV      A, D ; PUT THE PLAYER MOVE (X OR O) IN A
        PCHL ; JUMP TO THE WINNER PROC

PROC0:
        LXI      H, BOARD + 1
        CMP      M
        JNZ      P0NW
        INX      H ; TO 2
        CMP      M
        RZ

  P0NW:
        LXI      H, BOARD + 3
        CMP      M
        JNZ      P0NW2
        LXI      H, BOARD + 6
        CMP      M
        RZ

  P0NW2:
        LXI      H, BOARD + 4
        CMP      M
        JNZ      P0NO
        LXI      H, BOARD + 8
        CMP      M
        RZ

  P0NO:
        XRA      A
        RET

PROC1:
        LXI      H, BOARD + 0
        CMP      M
        JNZ      P1NW
        LXI      H, BOARD + 2
        CMP      M
        RZ

  P1NW:
        LXI      H, BOARD + 4
        CMP      M
        JNZ      P1NO
        LXI      H, BOARD + 7
        CMP      M
        RZ

  P1NO:
        XRA      A
        RET

PROC2:
        LXI      H, BOARD + 0
        CMP      M
        JNZ      P2NW
        INX      H ; TO 1
        CMP      M
        RZ

  P2NW:
        LXI      H, BOARD + 5
        CMP      M
        JNZ      P2NW2
        LXI      H, BOARD + 8
        CMP      M
        RZ

  P2NW2:
        LXI      H, BOARD + 4
        CMP      M
        JNZ      P2NO
        LXI      H, BOARD + 6
        CMP      M
        RZ

  P2NO:
        XRA      A
        RET

PROC3:
        LXI      H, BOARD + 0
        CMP      M
        JNZ      P3NW
        LXI      H, BOARD + 6
        CMP      M
        RZ

  P3NW:
        LXI      H, BOARD + 4
        CMP      M
        JNZ      P3NO
        LXI      H, BOARD + 5
        CMP      M
        RZ

  P3NO:
        XRA      A
        RET

PROC4:
        LXI      H, BOARD + 0
        CMP      M
        JNZ      P4NW
        LXI      H, BOARD + 8
        CMP      M
        RZ

  P4NW:
        LXI      H, BOARD + 2
        CMP      M
        JNZ      P4NW2
        LXI      H, BOARD + 6
        CMP      M
        RZ

  P4NW2:
        LXI      H, BOARD + 1
        CMP      M
        JNZ      P4NW3
        LXI      H, BOARD + 7
        CMP      M
        RZ

  P4NW3:
        LXI      H, BOARD + 3
        CMP      M
        JNZ      P4NO
        LXI      H, BOARD + 5
        CMP      M
        RZ

  P4NO:
        XRA      A
        RET

PROC5:
        LXI      H, BOARD + 3
        CMP      M
        JNZ      P5NW
        INX      H ; TO 4
        CMP      M
        RZ

  P5NW:
        LXI      H, BOARD + 2
        CMP      M
        JNZ      P5NO
        LXI      H, BOARD + 8
        CMP      M
        RZ

  P5NO:
        XRA      A
        RET

PROC6:
        LXI      H, BOARD + 4
        CMP      M
        JNZ      P6NW
        LXI      H, BOARD + 2
        CMP      M
        RZ

  P6NW:
        LXI      H, BOARD + 0
        CMP      M
        JNZ      P6NW2
        LXI      H, BOARD + 3
        CMP      M
        RZ

  P6NW2:
        LXI      H, BOARD + 7
        CMP      M
        JNZ      P6NO
        INX      H ; TO 8
        CMP      M
        RZ

  P6NO:
        XRA      A
        RET

PROC7:
        LXI      H, BOARD + 1
        CMP      M
        JNZ      P7NW
        LXI      H, BOARD + 4
        CMP      M
        RZ

  P7NW:
        LXI      H, BOARD + 6
        CMP      M
        JNZ      P7NO
        LXI      H, BOARD + 8
        CMP      M
        RZ

  P7NO:
        XRA      A
        RET

PROC8:
        LXI      H, BOARD + 0
        CMP      M
        JNZ      P8NW
        LXI      H, BOARD + 4
        CMP      M
        RZ

  P8NW:
        LXI      H, BOARD + 2
        CMP      M
        JNZ      P8NW2
        LXI      H, BOARD + 5
        CMP      M
        RZ

  P8NW2:
        LXI      H, BOARD + 6
        CMP      M
        JNZ      P8NO
        INX      H ; TO 7
        CMP      M
        RZ

  P8NO:
        XRA      A
        RET

; NEGATE THE DE REGISTER PAIR.
; NEGATE USING COMPLEMENT THEN ADD 1

NEGDE:
        MOV      A, D
        CMA
        MOV      D, A
        MOV      A, E
        CMA
        MOV      E, A
        INX      D
        RET

; NEGATE THE HL REGISTER PAIR.
; NEGATE USING COMPLEMENT THEN ADD 1

NEGHL:
        MOV      A, H
        CMA
        MOV      H, A
        MOV      A, L
        CMA
        MOV      L, A
        INX      H
        RET

; MULTIPLY DE BY HL, RESULT IN HL
; INCREDIBLY SLOW ITERATIVE ADDITION.

IMUL:
        MOV      A, L ; FIRST CHECK IF HL IS 0 AND JUST RETURN IF SO
        CPI      0
        JNZ      MSTRT
        MOV      A, H
        CPI      0
        JNZ      MSTRT
        RET

  MSTRT:
        PUSH     B
        MVI      B, 80H
        MOV      A, H
        ANA      B
        JZ       MNNEG
        CALL     NEGHL
        CALL     NEGDE
  MNNEG:
        PUSH     H
        POP      B
        LXI      H, 0
        SHLD     MTMP
  MLOOP:
        DAD      D
        JNC      MDONE
        PUSH     H
        LHLD     MTMP
        INX      H
        SHLD     MTMP
        POP      H
  MDONE:
        DCX      B
        MOV      A, B
        ORA      C
        JNZ      MLOOP
        POP      B
        RET

ATOU: ; IN: HL POINTS TO STRING. OUT: HL HAS INTEGER VALUE. POSITIVE BASE-10 IS ASSUMED
        PUSH   B
        PUSH   D
        LXI    B, 0 ; RUNNING TOTAL IS IN BC

  ATSP: ; SKIP PAST SPACES
        MOV    A, M
        CPI    ' '
        JNZ    ATNX
        INX    H
        JMP    ATSP

  ATNX:
        MOV    A, M ; CHECK IF WE'RE AT THE END OF STRING OR THE DATA ISN'T A NUMBER
        CPI    '0'
        JM     ATDN ; < '0' ISN'T A DIGIT
        CPI    '9' + 1
        JP     ATDN ; > '9' ISN'T A DIGIT

        LXI    D, 10 ; MULTIPLY WHAT WE HAVE SO FAR BY 10
        PUSH   H
        MOV    H, B
        MOV    L, C
        CALL   IMUL
        MOV    B, H
        MOV    C, L
        POP    H

        MOV    A, M ; RESTORE THE DIGIT IN A BECAUSE IMUL TRASHED IT
        SUI    '0' ; CHANGE ASCII TO A NUMBER
        ADD    C ; ADD THIS NEW NUMBER TO THE RUNNING TOTAL IN BC
        MOV    C, A
        MOV    A, B
        ACI    0 ; IF THERE WAS A CARRY FROM THE ADD, REFLECT THAT
        MOV    B, A

        INX    H ; MOVE TO THE NEXT CHARACTER
        JMP    ATNX ; AND PROCESS IT

  ATDN:
        MOV    H, B ; THE RESULT GOES IN HL
        MOV    L, C
        POP    D
        POP    B
        RET

PUTCH: ; DISPLAY THE CHARACTER IN A
        PUSH    B
        PUSH D
        PUSH H

        MVI     C, WCONF
        MOV     E, A
        CALL    BDOS

        POP     H
        POP D
        POP B
        RET

DISPD: ; ARGUMENT # 0-9 IS IN REGISTER B
        PUSH    B
        PUSH D
        PUSH H

        MVI     A, 48
        ADD     B
        CALL    PUTCH

        POP     H
        POP D
        POP B
        RET

DISP: ; DISPLAY NULL-TERMINATED STRING POINTED TO BY HL
        PUSH    B
        PUSH D
        PUSH H

        MOV     B, H
        MOV     C, L

  DNEXT:
        LDAX    B
        CPI     0
        JZ      DDONE
        CALL    PUTCH
        INX     B
        JMP     DNEXT

  DDONE:
        POP     H
        POP D
        POP B
        RET

PUTHL: ; PRINT THE SIGNED 16-BIT NUMBER IN HL
        MOV     A, H ; GET THE SIGN BIT OF THE INTEGER,
        RAL ; WHICH IS THE TOP BIT OF THE HIGH BYTE
        SBB     A ; A=00 IF POSITIVE, FF IF NEGATIVE
        STA     NEGF ; STORE IT AS THE NEGATIVE FLAG
        CNZ     NEGHL ; AND IF HL WAS NEGATIVE, MAKE IT POSITIVE
        LXI     D, NUM ; LOAD POINTER TO END OF NUMBER STRING
        PUSH    D ; ONTO THE STACK
        LXI     B, -10 ; DIVIDE BY TEN (BY TRIAL SUBTRACTION)
  DIGIT:
        LXI     D, -1 ; DE = QUOTIENT. THERE IS NO 16-BIT SUBTRACTION,
  DGDIV:
        DAD     B ; SO WE JUST ADD A NEGATIVE VALUE,
        INX     D
        JC      DGDIV ; WHILE THAT OVERFLOWS.
        MVI     A, '0'+10 ; THE LOOP RUNS ONCE TOO MUCH SO WE'RE 10 OUT
        ADD     L ; THE REMAINDER (MINUS 10) IS IN L
        XTHL ; SWAP HL WITH TOP OF STACK (I.E., THE STRING POINTER)
        DCX     H ; GO BACK ONE BYTE
        MOV     M, A ; AND STORE THE DIGIT
        XTHL ; PUT THE POINTER BACK ON THE STACK
        XCHG ; DO ALL OF THIS AGAIN WITH THE QUOTIENT
        MOV     A, H ; IF IT IS ZERO, WE'RE DONE
        ORA     L
        JNZ     DIGIT ; BUT IF NOT, THERE ARE MORE DIGITS
        MVI     C, PRSTR ; PREPARE TO CALL CP/M AND PRINT THE STRING
        POP     D ; PUT THE STRING POINTER FROM THE STACK IN DE
        LDA     NEGF ; SEE IF THE NUMBER WAS SUPPOSED TO BE NEGATIVE
        INR     A
        JNZ     BDOS ; IF NOT, PRINT THE STRING WE HAVE AND RETURN
        DCX     D ; BUT IF SO, WE NEED TO ADD A MINUS IN FRONT
        MVI     A, '-'
        STAX    D
        JMP     BDOS ; AND ONLY THEN PRINT THE STRING. BDOS WILL RETURN TO CALLER

ORG     500H ; THE BOARD AND WIN PROCEDURE TABLE SHARE ONE 256-BYTE PAGE.
; THE ASSEMBLER WON'T WARN IF DATA AND CODE OVERLAP!
; THERE IS NO WAY TO FORCE ALIGNMENT, WHICH WOULD BE BETTER.
; THE BOARD IS AT 540H WITH A ZERO SENTINEL AFTER IT; WINPROCS
; IS AT 580H, SO THE ENTRY FOR BOARD ADDRESS LOW BYTE L IS
; AT 500H + 2 * L.

ORG     540H
BOARD:     DB      0,0,0,0,0,0,0,0,0
SENTL:  DB      0 ; NEVER WRITTEN: STOPS THE MOVE SCAN
MOVES:     DW      0 ; COUNT OF MOVES EXAMINED (TO VALIDATE THE APP)

ORG     580H
WPROC:  DW      PROC0,PROC1,PROC2,PROC3,PROC4,PROC5,PROC6,PROC7,PROC8
NEGF:      DB      0 ; SPACE FOR NEGATIVE FLAG
           DB      '-00000'
NUM:       DB      '$' ; SPACE FOR NUMBER. CP/M STRINGS END WITH A DOLLAR SIGN
CRLF:      DB      13,10,0
SITER:  DB     105,116,101,114,97,116,105,111,110,115,': ', 0
SBADA: DB      105,116,101,114,97,116,105,111,110,' ',97,114,103,117,109
            DB      101,110,116,' ',105,110,118,97,108,105,100,'; ',105
            DB      116,' ',109,117,115,116,' ',98,101,' 1 - 32767',13
            DB      10,0
SMOVE:  DB     109,111,118,101,115,': ', 0
ITERS:     DW      0 ; ITERATIONS OF RUNNING THE APP SO FAR
LOOPS:     DW      0 ; # OF ITERATIONS TO MAKE
MTMP:    DW      0

END

