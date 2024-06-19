         IDENTIFICATION DIVISION.
         PROGRAM-ID.  E.
      *  REMARKS. generate digits of e
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
         01 ARRAYS.
             02 A PIC 9(04) COMP OCCURS 200 TIMES.
         01 X PIC 9 COMP VALUE 0.
         01 TMOD PIC 9 COMP VALUE 0.
         01 TM PIC 9 COMP VALUE 0.
         01 TD PIC 9 COMP VALUE 0.
         01 N PIC 9 COMP VALUE 0.
         01 HIGH PIC 9 COMP VALUE 0.
         01 NUM-DISP PIC 9999.

         PROCEDURE DIVISION.
         MAIN.
             DISPLAY 'computing e'.
             PERFORM INITA-ROUTINE.
             PERFORM INITA-ROUTINE-B.
             PERFORM INITA-ROUTINE-C.
             PERFORM OUTER-LOOP.
             STOP RUN.

         INITA-ROUTINE.
             MOVE 200 TO HIGH.
             MOVE 0 TO X.
             MOVE 199 TO N.

         INITA-ROUTINE-B.
             MOVE 1 TO A( N + 1 ).
             SUBTRACT 1 FROM N.
             IF N > 0 GO TO INITA-ROUTINE-B.

         INITA-ROUTINE-C.
             MOVE 2 TO A( 2 ).
             MOVE 0 TO A( 1 ).
          
         OUTER-LOOP.
             ADD -1 TO HIGH.
             MOVE HIGH TO N.
             PERFORM INNER-LOOP.
             IF HIGH > 9 GO TO OUTER-LOOP.

         INNER-LOOP.
             DIVIDE X BY N GIVING TD.
             COMPUTE TMOD = ( X - ( TD * N ) )
             IF 0 = X MOVE 0 TO TMOD.
             MOVE TMOD TO A( N + 1 ).
             MULTIPLY 10 BY A( N ) GIVING TM.
             COMPUTE X = TM + TD.
             SUBTRACT 1 FROM N.
             IF N > 0 GO TO INNER-LOOP.
             MOVE X TO NUM-DISP.
             DISPLAY NUM-DISP.

