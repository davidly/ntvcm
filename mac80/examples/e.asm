; PORTED TO MAC80 FROM CPM_COMPILERS/ASM/E.ASM, WRITTEN FOR CP/M ASM.COM.
; LABELS ARE SHORTENED TO MAC80'S FIVE SIGNIFICANT CHARACTERS, AND
; LOWERCASE TEXT IS GIVEN AS ASCII CODES. THE CODE IS BYTE-IDENTICAL.
;
; 8080 ASSEMBLY TO COMPUTE E TO N DIGITS.
; WHEN DIGITS IS 200, THE FIRST 192 DIGITS ARE DISPLAYED.
; BYTE COUNTERS, INLINED DIVISION BY ADDING -N, PER-QUOTIENT LOOP TAILS, AND
; PAGE-ALIGNED MULTIPLY-BY-TEN TABLES SPECIALIZE THE INNER LOOP WITHOUT
; CHANGING THE SPIGOT.
;
; EQUIVALENT TO:
;     PROGRAM E;
;     CONST
;        DIGITS = 200;
;     TYPE
;         ARRAYTYPE = ARRAY  0..DIGITS   OF INTEGER;
;     VAR
;         HIGH, N, X : INTEGER;
;         A : ARRAYTYPE;
;     BEGIN
;         HIGH := DIGITS;
;         X := 0;
;         N := HIGH - 1;
;         WHILE N > 0 DO BEGIN
;             A  N   := 1;
;             N := N - 1;
;         END;
;
;         A  1   := 2;
;         A  0   := 0;
;
;         WHILE HIGH > 9 DO BEGIN
;             HIGH := HIGH - 1;
;             N := HIGH;
;             WHILE 0 <> N DO BEGIN
;                 A  N   := X MOD N;
;                 X := 10 * A  N - 1   + X DIV N;
;                 N := N - 1;
;             END;
;
;             WRITE( X );
;         END;
;
;         WRITELN; WRITELN( 'DONE' );
;     END.

; CP/M-SPECIFIC CONSTANTS

BDOS  EQU 5
WCONF EQU 2
PRSTR EQU 9

; APP CONSTANTS

TRUE   EQU 1
FALSE  EQU 0
; BYTE INDICES AND THE PAGE-ALIGNED ARRAY REQUIRE 10 <= DIGITS <= 255.
NDIG EQU 200

ORG     100H
; WHY IS THE LOOP BACKWARDS? NO IDEA.
; WHILE N > 0 DO BEGIN
;     A  N   := 1;
;     N := N - 1;
; END;

; C COUNTS INITIALIZED BYTES; HL ADDRESSES THE PAGE-ALIGNED ARRAY.
; B IS THE BYTE VALUE 1.

        MVI        B, 1
        MVI        C, NDIG - 1
        LXI        H, ARRAY + NDIG - 1

  AINIT:
        MOV        M, B
        DCR        L
        DCR        C
        JNZ        AINIT

; A  1   := 2;
; A  0   := 0;

        LXI        H, ARRAY + 1
        MVI        M, 2
        DCR        L
        MVI        M, 0

; REGISTER B IS BOTH "HIGH" AND "N". HL HOLDS X; C HOLDS THE LOW BYTE
; OF -N (ITS HIGH BYTE IS 255). THE QUOTIENT IS 0..9 IN PRACTICE: AN
; UNROLLED SUBTRACTION CHAIN EXITS TO A TAIL SPECIALIZED FOR EACH
; QUOTIENT, SO NO COUNTER IS NEEDED. LARGER QUOTIENTS USE A COUNTING
; LOOP AND A GENERIC TAIL.

        MVI        B, NDIG

  OUTLP:
        DCR        B ; HIGH := HIGH - 1; N := HIGH;
        PUSH       B ; SAVE B/HIGH ON THE STACK
        LXI        H, 0 ; X := 0;
        XRA        A
        SUB        B
        MOV        C, A ; C = LOW BYTE OF -N

  INLP:
        MVI        D, 255
        MOV        E, C ; DE = -N, WITH 1 <= N <= 254
; DAD'S CARRY SIGNALS A SUCCESSFUL SUBTRACTION.
        DAD        D
        JNC        QUOT0
        DAD        D
        JNC        QUOT1
        DAD        D
        JNC        QUOT2
        DAD        D
        JNC        QUOT3
        DAD        D
        JNC        QUOT4
        DAD        D
        JNC        QUOT5
        DAD        D
        JNC        QUOT6
        DAD        D
        JNC        QUOT7
        DAD        D
        JNC        QUOT8
        DAD        D
        JNC        QUOT9
        MVI        A, 9 ; TEN SUBTRACTIONS SUCCEEDED: COUNT THE REST
  QMORE:
        INR        A
        DAD        D
        JC         QMORE
        STA        QUOT
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        LDA        QUOT
        ADD        M
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + QUOTIENT
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON

; ONE COPY OF THE REST OF THE INNER LOOP PER QUOTIENT Q:
; A N  = X MOD N, X = 10*A N-1  + Q.
  QUOT0:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        INR        H
        MOV        H, M
        MOV        L, A ; X = 10*A N-1
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT1:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        1
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 1
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT2:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        2
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 2
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT3:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        3
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 3
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT4:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        4
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 4
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT5:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        5
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 5
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT6:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        6
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 6
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT7:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        7
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 7
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT8:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        8
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 8
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON
  QUOT9:
        MOV        A, L
        ADD        B ; RESTORE ONLY THE BYTE REMAINDER
        MVI        D, ARRAY / 256 ; PAGE-ALIGNED BYTE ARRAY
        MOV        E, B
        STAX       D ; A N  = REMAINDER
        DCR        E
        LDAX       D ; A N-1 , USED AS A TABLE INDEX
        MOV        L, A
        MVI        H, M10LO / 256
        MOV        A, M
        ADI        9
        MOV        E, A
        INR        H
        MOV        A, M
        ACI        0
        MOV        H, A
        MOV        L, E ; X = 10*A N-1  + 9
        INR        C ; -N FOR THE NEXT, SMALLER N
        DCR        B ; N := N - 1
        JNZ        INLP
        JMP        INDON

  INDON:
        CALL       PUTU ; FINAL X IS THE NEXT OUTPUT DIGIT(S)

        MVI        A, 9
        POP        B ; RESTORE HIGH
        CMP        B ; WHILE HIGH > 9 DO BEGIN (RE-WRITTEN AS A DO UNTIL LOOP)
        JNZ        OUTLP

        LXI        H, CRLF
        CALL       PUTS
        LXI        H, SDONE
        CALL       PUTS

        RET ; RETURN TO CP/M

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

PUTS: ; DISPLAY NULL-TERMINATED STRING POINTED TO BY HL
        PUSH    B
        PUSH D
        PUSH H
        MOV     B, H
        MOV     C, L
  DSNX:
        LDAX    B
        CPI     0
        JZ      DSDN
        CALL    PUTCH
        INX     B
        JMP     DSNX
  DSDN:
        POP     H
        POP D
        POP B
        RET

PUTU: ; PRINT THE UNSIGNED 16-BIT NUMBER IN HL
        PUSH    B
        PUSH D
        PUSH H
        CALL    PRTU
        POP     H
        POP D
        POP B
        RET

PRTU: ; PRINT THE UNSIGNED 16-BIT NUMBER IN HL
        LXI     D, NUM ; LOAD POINTER TO END OF NUMBER STRING
        PUSH    D ; ONTO THE STACK
        LXI     B, -10 ; DIVIDE BY TEN (BY TRIAL SUBTRACTION)
  PUDG:
        LXI     D, -1 ; DE = QUOTIENT. THERE IS NO 16-BIT SUBTRACTION,
  PUDIV:
        DAD     B ; SO WE JUST ADD A NEGATIVE VALUE,
        INX     D
        JC      PUDIV ; WHILE THAT OVERFLOWS.
        MVI     A, '0'+10 ; THE LOOP RUNS ONCE TOO MUCH SO WE'RE 10 OUT
        ADD     L ; THE REMAINDER (MINUS 10) IS IN L
        XTHL ; SWAP HL WITH TOP OF STACK (I.E., THE STRING POINTER)
        DCX     H ; GO BACK ONE BYTE
        MOV     M, A ; AND STORE THE DIGIT
        XTHL ; PUT THE POINTER BACK ON THE STACK
        XCHG ; DO ALL OF THIS AGAIN WITH THE QUOTIENT
        MOV     A, H ; IF IT IS ZERO, WE'RE DONE
        ORA     L
        JNZ     PUDG ; BUT IF NOT, THERE ARE MORE DIGITS
        MVI     C, PRSTR ; PREPARE TO CALL CP/M AND PRINT THE STRING
        POP     D
        JMP     BDOS ; AND ONLY THEN PRINT THE STRING. BDOS WILL RETURN TO CALLER

            DB      '00000'
NUM:        DB      '$' ; SPACE FOR NUMBER. CP/M STRINGS END WITH A DOLLAR SIGN
CRLF:       DB      13,10,0
SDONE:    DB      100,111,110,101,'.', 13, 10, 0
QUOT:       DB      0 ; A QUOTIENT OF 10 OR MORE
; THE LOW AND HIGH MULTIPLICATION TABLES OCCUPY ADJACENT COMPLETE PAGES.
            ORG     ($+255) AND 0FF00H
M10LO:
            DB      0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150
            DB      160,170,180,190,200,210,220,230,240,250,4,14,24,34
            DB      44,54
            DB      64,74,84,94,104,114,124,134,144,154,164,174,184,194
            DB      204,214
            DB      224,234,244,254,8,18,28,38,48,58,68,78,88,98,108,118
            DB      128,138,148,158,168,178,188,198,208,218,228,238,248
            DB      2,12,22
            DB      32,42,52,62,72,82,92,102,112,122,132,142,152,162,172
            DB      182
            DB      192,202,212,222,232,242,252,6,16,26,36,46,56,66,76
            DB      86
            DB      96,106,116,126,136,146,156,166,176,186,196,206,216
            DB      226,236,246
            DB      0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150
            DB      160,170,180,190,200,210,220,230,240,250,4,14,24,34
            DB      44,54
            DB      64,74,84,94,104,114,124,134,144,154,164,174,184,194
            DB      204,214
            DB      224,234,244,254,8,18,28,38,48,58,68,78,88,98,108,118
            DB      128,138,148,158,168,178,188,198,208,218,228,238,248
            DB      2,12,22
            DB      32,42,52,62,72,82,92,102,112,122,132,142,152,162,172
            DB      182
            DB      192,202,212,222,232,242,252,6,16,26,36,46,56,66,76
            DB      86
            DB      96,106,116,126,136,146,156,166,176,186,196,206,216
            DB      226,236,246
M10HI:
            DB      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            DB      0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1
            DB      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
            DB      1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2
            DB      2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3
            DB      3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
            DB      3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4
            DB      4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
            DB      5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
            DB      5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6
            DB      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
            DB      6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7
            DB      7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8
            DB      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
            DB      8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9
            DB      9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
ARRAY:      DS      NDIG

END

; UNUSED CODE
IF 0

; UNSIGNED MULTIPLY DE BY L, RESULT IN HL. OVERFLOW IS LOST.
UMUL:
        MVI     H, 0
        MOV     A, L
        CPI     0
        RZ
        MOV     A, L
        MVI     L, 0
  UMLP:
        DAD     D
        DCR     A
        JNZ     UMLP
        RET

; NEGATE THE DE REGISTER PAIR. HANDY FOR IDIV AND IMUL
; NEGATE USING COMPLEMENT THEN ADD 1
NEGDE:
       MOV     A, D
       CMA
       MOV     D, A
       MOV     A, E
       CMA
       MOV     E, A
       INX     D
       RET

; NEGATE THE HL REGISTER PAIR. HANDY FOR IDIV AND IMUL
; NEGATE USING COMPLEMENT THEN ADD 1
NEGHL:
        MOV     A, H
        CMA
        MOV     H, A
        MOV     A, L
        CMA
        MOV     L, A
        INX     H
        RET

; SIGNED MULTIPLY DE BY HL, RESULT IN HL
IMUL:
        MOV     A, L
        CPI     0
        JNZ     IMNZ
        MOV     A, H
        CPI     0
        JNZ     IMNZ
        RET
  IMNZ:
        PUSH    B
        PUSH D
        MVI     B, 80H
        MOV     A, H
        ANA     B
        JZ      IMNN
        CALL    NEGHL
        CALL    NEGDE
  IMNN:
        MOV     B, H
        MOV     C, L
        LXI     H, 0
        SHLD    MTMP
  IMLP:
        DAD     D
        JNC     IMDON
        PUSH    H
        LHLD    MTMP
        INX     H
        SHLD    MTMP
        POP     H
  IMDON:
        DCX     B
        MOV     A, B
        ORA     C
        JNZ     IMLP
        POP     D
        POP B
        RET

; SIGNED DIVIDE DE BY HL, RESULT IN HL. REMAINDER IN DIVREM
IDIV:
        XRA     A
        CMP     E
        JNZ     IDNZ
        CMP     D
        JNZ     IDNZ
        LXI     H, 0
        SHLD    DVREM ; 0 MOD X IS 0
        RET

  IDNZ:
        PUSH    B
        PUSH D
        XCHG
        MVI     C, 0
        MVI     B, 80H
        MOV     A, D
        ANA     B
        JZ      IDDNN
        INR     C
        CALL    NEGDE
  IDDNN:
        MOV     A, H
        ANA     B
        JZ      IDHNN
        INR     C
        CALL    NEGHL
  IDHNN:
        PUSH    B
        LXI     B, 0
  IDLP:
        MOV     A, L
        SUB     E
        MOV     L, A
        MOV     A, H
        SBB     D
        MOV     H, A
        JC      IDDON
        INX     B
        JMP     IDLP
  IDDON:
        DAD     D
        SHLD    DVREM
        MOV     L, C
        MOV     H, B
        POP     B
        MOV     A, C
        ANI     1
        CNZ     NEGHL
        POP     D
        POP B
        RET

; NEGATE THE HL REGISTER PAIR.
; NEGATE USING COMPLEMENT THEN ADD 1

NGTHL:
        MOV      A, H
        CMA
        MOV      H, A
        MOV      A, L
        CMA
        MOV      L, A
        INX      H
        RET

PUTHL: ; PRINT THE SIGNED 16-BIT NUMBER IN HL
        MOV     A, H ; GET THE SIGN BIT OF THE INTEGER,
        RAL ; WHICH IS THE TOP BIT OF THE HIGH BYTE
        SBB     A ; A=00 IF POSITIVE, FF IF NEGATIVE
        STA     NEGF ; STORE IT AS THE NEGATIVE FLAG
        CNZ     NGTHL ; AND IF HL WAS NEGATIVE, MAKE IT POSITIVE
        LXI     D, NUM ; LOAD POINTER TO END OF NUMBER STRING
        PUSH    D ; ONTO THE STACK
        LXI     B, -10 ; DIVIDE BY TEN (BY TRIAL SUBTRACTION)
  PHDG:
        LXI     D, -1 ; DE = QUOTIENT. THERE IS NO 16-BIT SUBTRACTION,
  PHDIV:
        DAD     B ; SO WE JUST ADD A NEGATIVE VALUE,
        INX     D
        JC      PHDIV ; WHILE THAT OVERFLOWS.
        MVI     A, '0'+10 ; THE LOOP RUNS ONCE TOO MUCH SO WE'RE 10 OUT
        ADD     L ; THE REMAINDER (MINUS 10) IS IN L
        XTHL ; SWAP HL WITH TOP OF STACK (I.E., THE STRING POINTER)
        DCX     H ; GO BACK ONE BYTE
        MOV     M, A ; AND STORE THE DIGIT
        XTHL ; PUT THE POINTER BACK ON THE STACK
        XCHG ; DO ALL OF THIS AGAIN WITH THE QUOTIENT
        MOV     A, H ; IF IT IS ZERO, WE'RE DONE
        ORA     L
        JNZ     PHDG ; BUT IF NOT, THERE ARE MORE DIGITS
        MVI     C, PRSTR ; PREPARE TO CALL CP/M AND PRINT THE STRING
        POP     D ; PUT THE STRING POINTER FROM THE STACK IN DE
        LDA     NEGF ; SEE IF THE NUMBER WAS SUPPOSED TO BE NEGATIVE
        INR     A
        JNZ     BDOS ; IF NOT, PRINT THE STRING WE HAVE AND RETURN
        DCX     D ; BUT IF SO, WE NEED TO ADD A MINUS IN FRONT
        MVI     A, '-'
        STAX    D
        JMP     BDOS ; AND ONLY THEN PRINT THE STRING. BDOS WILL RETURN TO CALLER


NEGF:       DB      0, '-'

ENDIF





