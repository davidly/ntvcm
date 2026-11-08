; PORTED TO MAC80 FROM CPM_COMPILERS/ASM/SIEVE.ASM, WRITTEN FOR CP/M ASM.COM.
; LABELS ARE SHORTENED TO MAC80'S FIVE SIGNIFICANT CHARACTERS, AND
; LOWERCASE TEXT IS GIVEN AS ASCII CODES. THE CODE IS BYTE-IDENTICAL.
;
; I8080 VERSION OF BYTE'S SIEVE
; REPLICATE THIS:
;        #DEFINE TRUE 1
;        #DEFINE FALSE 0
;        #DEFINE SIZE 8190
;
;        CHAR FLAGS SIZE+1 ;
;
;        INT MAIN()
;
;                INT I,K;
;                INT PRIME,COUNT,ITER;
;
;                FOR (ITER = 1; ITER <= 10; ITER++)      /* DO PROGRAM 10 TIMES */
;                        COUNT = 0;                      /* INITIALIZE PRIME COUNTER */
;                        FOR (I = 0; I <= SIZE; I++)     /* SET ALL FLAGS TRUE */
;                                FLAGS I  = TRUE;
;                        FOR (I = 0; I <= SIZE; I++)
;                                IF (FLAGS I )           /* FOUND A PRIME */
;                                        PRIME = I + I + 3;      /* TWICE INDEX + 3 */
;                                        FOR (K = I + PRIME; K <= SIZE; K += PRIME)
;                                                FLAGS K  = FALSE;       /* KILL ALL MULTIPLES */
;                                        COUNT++;                /* PRIMES FOUND */
;
;
;
;                PRINTF("%D PRIMES. N",COUNT);           /*PRIMES FOUND IN 10TH PASS */
;                RETURN 0;
;
;

; CP/M-SPECIFIC CONSTANTS

BDOS  EQU 5
WCONF EQU 2
PRSTR EQU 9

; APP CONSTANTS

TRUE  EQU 1
FALSE EQU 0
LOOPS EQU 10
SIZE  EQU 8190 ; LOTS OF ASSUMPTIONS ABOUT THIS SIZE BELOW / CAN'T EASILY CHANGE IT

SZHI EQU ( SIZE + 1 ) SHR 8 ; SIZE PLUS 1 HIGH
SZLO EQU ( SIZE + 1 ) AND 0FFH ; SIZE PLUS 1 LOW

ORG     100H

  ITAGN: ; FOR ( ITER = 1; ITER <= 10; ITER++ )
        LXI        H, 0 ; COUNT = 0
        SHLD       COUNT

; SET ALL ARRAY ENTRIES TO TRUE:  FOR (I = 0; I <= SIZE; I++) FLAGS I  = TRUE;
; USING SP FOR 2-BYTE WRITES IS MUCH FASTER THAN A LOOP OF WRITING BYTES.
; EXPAND THE ARRAY SIZE TO A POWER OF 2 FOR PERFORMANCE.

        LXI        H, 0
        DAD        SP
        SHLD       SAVSP ; SAVE STACK POINTER
        LXI        H, ENDAR
        SPHL ; MAKE SP POINT TO JUST BEYOND THE ARRAY

        LXI        H, 0101H ; TRUE TRUE
        LXI        B, 0002H ; 2 * 256 * 16 = SIZE OF THE ARRAY 8192

  FILL:
        PUSH       H ; WRITE 16 BYTES OF TRUE
        PUSH       H
        PUSH       H
        PUSH       H
        PUSH       H
        PUSH       H
        PUSH       H
        PUSH       H
        DCR        B
        JNZ        FILL ; DO THIS 256 TIMES
        DCR        C
        JNZ        FILL ; DO THIS 2 TIMES

        LHLD       SAVSP ; RESTORE STACK POINTER
        SPHL

        LXI        H, ENDAR ; ONE GUARANTEED-TRUE SENTINEL BYTE JUST PAST THE 8192
        MVI        M, TRUE ; FILLED BYTES; KLOOP CAN NEVER REACH OR CLEAR IT

; ITERATE THROUGH ARRAY ENTRIES AND COUNT PRIMES
; REGISTER USAGE:
;    HL:    SCAN POINTER, THEN & FLAGS  K
;    DE:    PRIME
;    A:     0 WHILE SCANNING; THE FLAGS PAGE LIMIT 22H IN KLOOP
;    C:     0 IN KLOOP, STORED TO CLEAR FLAGS
;    B:     HIGH BYTE BOUND FOR EIGHT MARKS PER CHECK (PRIMES BELOW 256)

        LXI        H, FLAGS - 1
  NSCAN:
        XRA        A
  NPSC:
        INX        H
        CMP        M ; IF (FLAGS I )
        JZ         NPSC

        MVI        A, ENDAR SHR 8 ; THE GUARANTEED-TRUE SENTINEL JUST PAST THE ARRAY?
        CMP        H
        JZ         NXTIT
        PUSH       H ; RESUME THE SCAN AFTER FLAGS  I

        MOV        D, H ; PRIME = I + I + 3 = 2 * & FLAGS  I   - 2 * FLAGS + 3
        MOV        E, L
        DAD        H
        LXI        B, 3 - 2 * FLAGS
        DAD        B
        XCHG ; DE IS PRIME, HL = & FLAGS  I
        DAD        D ; HL = & FLAGS  K  , K = I + PRIME
        MVI        C, 0 ; STORED BY KLOOP TO CLEAR FLAGS

; PRIMES BELOW 256 MAKE MOST OF THE MARKS. FOR THEM, B IS THE HIGH
; BYTE OF 22FFH - 7 * PRIME: WHILE H < B, EIGHT MORE MARKS STAY IN BOUNDS.
        MOV        A, D
        ORA        A
        JNZ        KCHK
        PUSH       H
        MOV        H, D ; HL = 7 * PRIME = ( ( 2 * PRIME + PRIME ) * 2 ) + PRIME
        MOV        L, E
        DAD        H
        DAD        D
        DAD        H
        DAD        D
        MVI        A, 22H ; HIGH( 22FFH - 7 * PRIME ): THE LOW BYTE NEVER BORROWS
        SUB        H
        MOV        B, A
        POP        H
        MOV        A, H
        CMP        B
        JNC        KCHK

  K8LP: ; EIGHT MARKS PER BOUNDS CHECK
        MOV        M, C
        DAD        D
        MOV        M, C
        DAD        D
        MOV        M, C
        DAD        D
        MOV        M, C
        DAD        D
        MOV        M, C
        DAD        D
        MOV        M, C
        DAD        D
        MOV        M, C
        DAD        D
        MOV        M, C
        DAD        D
        MOV        A, H
        CMP        B
        JC         K8LP

  KCHK:
; CHECK IF POINTER TO AFTER LAST ITEM IN FLAGS < POINTER TO K'TH ELEMENT IN FLAGS (HL)
        MVI        A, 22H ; 22H - 23H WILL SET THE CARRY BIT
        CMP        H
        JC         INCCT

  KLOOP: ; HL = POINTER TO K'TH ELEMENT IN FLAGS. DE = PRIME
        MOV        M, C ; FLAGS  K   = FALSE
        DAD        D ; K += PRIME
        CMP        H
        JNC        KLOOP

  INCCT:
        LXI        H, COUNT ; COUNT++
        INR        M
        JNZ        CNTED
        INX        H
        INR        M
  CNTED:
        POP        H
        JMP        NSCAN

  NXTIT: ; ARE WE DONE ITERATING LOOPS TIMES?
        LDA        ITER
        INR        A
        STA        ITER
        CPI        LOOPS
        JNZ        ITAGN

        LHLD       COUNT ; PRINT THE COUNT OF PRIMES AND THE STRING
        CALL       PUTHL
        LXI        H, SPRIM
        CALL       DISP

        RET ; RETURN TO CP/M

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

COUNT:      DW      0 ; COUNT IN C CODE
ITER:       DW      0 ; ITER IN C CODE
SAVSP:     DW      0 ; SAVE/RESTORE SP
NEGF:       DB      0 ; SPACE FOR NEGATIVE FLAG
            DB      '-00000'
NUM:        DB      '$' ; SPACE FOR NUMBER. CP/M STRINGS END WITH A DOLLAR SIGN
CRLF:       DB      13,10,0
SPRIM:  DB     ' ',112,114,105,109,101,115,'.', 13, 10, 0


; MAKE FLAGS START AT 300H END AT 2300H SO THE INNER LOOP CAN BE FASTER; ONE COMPARE NOT TWO

ORG 2300H-8192
FLAGS:      DS      SIZE + 1 ; 8191 BYTES
ENDFL: DB      69H ; WASTED BYTE SO INITIALIZATION OF ARRAY IS FASTER
ENDAR: ; FLAGS + 8192. 8K ARRAY

END

