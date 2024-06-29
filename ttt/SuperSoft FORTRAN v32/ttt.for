C FORTRAN VERSION OF PROVING YOU CAN'T WIN AT TIC-TAC-TOE IF THE OPPONENT IS COMPETENT
C CONSTANTS:
C    SCORE WIN:   6
C    SCORE TIE:   5
C    SCORE LOSE:  4
C    SCORE MAX:   9
C    SCORE MIN:   2
C    PIECE X:     1
C    PIECE O:     2
C    PIECE BLANK: 0

#      PROGRAM TTT
      INTEGER*1 B(9), SP(10), SV(10), SA(10), SB(10), SM(10)
      INTEGER*2 MC, L
      INTEGER*1 ALPHA, BETA, WI, ST, SC, V, P, PM, M
      COMMON /AREA/ B,SP,SV,SA,SB,SM,MC,L,ALPHA,BETA,WI,ST,SC,V,P,PM,M

      DO 6 L = 1, 9, 1
          B( L ) = 0
 6    CONTINUE

      DO 10 L = 1, 10, 1
          MC = 0

          M = 1
          ALPHA = 2
          BETA = 9
          P = M
          B(M) = 1
          CALL MINMAX
          B(M) = 0

          M = 2
          ALPHA = 2
          BETA = 9
          P = M
          B(M) = 1
          CALL MINMAX
          B(M) = 0

          M = 5
          ALPHA = 2
          BETA = 9
          P = M
          B(M) = 1
          CALL MINMAX
          B(M) = 0
 10   CONTINUE

      WRITE( 1, 20 ) MC
 20   FORMAT( 1X, I6 )
      STOP
      END

 4000 SUBROUTINE MINMAX
      INTEGER*1 B(9), SP(10), SV(10), SA(10), SB(10), SM(10)
      INTEGER*2 MC, L
      INTEGER*1 ALPHA, BETA, WI, ST, SC, V, P, PM, M
      COMMON /AREA/ B,SP,SV,SA,SB,SM,MC,L,ALPHA,BETA,WI,ST,SC,V,P,PM,M

      ST = 0
      V = 0
 4100 MC = MC + 1
      IF ( ST .LT. 4 ) GO TO 4150
      GO TO ( 5010, 5020, 5030, 5040, 5050, 5060, 5070, 5080, 5090 ), P
 4110 IF ( WI .EQ. 0 ) GO TO 4140
      IF ( WI .NE. 1 ) GO TO 4130
      SC = 6
      GO TO 4280
 4130 SC = 4
      GO TO 4280
 4140 IF ( ST .NE. 8 ) GO TO 4150
      SC = 5
      GO TO 4280
 4150 IF ( B( P ) .EQ. 1 ) GO TO 4160
      V = 2
      PM = 1
      GO TO 4170
 4160 V = 9
      PM = 2
 4170 P = 1
 4180 IF ( B( P ) .NE. 0 ) GO TO 4500
      B( P ) = PM
 4182 ST = ST + 1
      SP( ST ) = P
      SV( ST ) = V
      SA( ST ) = ALPHA
      SB( ST ) = BETA
      SM( ST ) = PM
      GO TO 4100
 4280 P = SP( ST )
      V = SV( ST )
      ALPHA = SA( ST )
      BETA = SB( ST )
      PM = SM( ST )
      ST = ST - 1
      B( P ) = 0
      IF ( PM .EQ. 1 ) GO TO 4340
      IF ( SC .EQ. 4 ) GO TO 4530
      IF ( SC .LT.  V ) V = SC
      IF ( V .LT. BETA ) BETA = V
      IF ( BETA .LE. ALPHA ) GO TO 4520
      GO TO 4500
 4340 IF ( SC .EQ. 6 ) GO TO 4530
      IF ( SC .GT. V ) V = SC
      IF ( V .GT. ALPHA ) ALPHA = V
      IF ( ALPHA .GE. BETA ) GO TO 4520
 4500 P = P + 1
      IF ( P .LT. 10 ) GO TO 4180
 4520 SC = V
 4530 IF ( ST .EQ. 0 ) RETURN
      GO TO 4280
 4900 FORMAT( 1X, I6 )

 5010 WI = B(1)
      IF ( ( WI .EQ. B(2) ) .AND. ( WI .EQ. B(3) ) ) GOTO 4110
      IF ( ( WI .EQ. B(4) ) .AND. ( WI .EQ. B(7) ) ) GOTO 4110
      IF ( ( WI .EQ. B(5) ) .AND. ( WI .EQ. B(9) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5020 WI = B(2)
      IF ( ( WI .EQ. B(1) ) .AND. ( WI .EQ. B(3) ) ) GOTO 4110
      IF ( ( WI .EQ. B(5) ) .AND. ( WI .EQ. B(8) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5030 WI = B(3)
      IF ( ( WI .EQ. B(1) ) .AND. ( WI .EQ. B(2) ) ) GOTO 4110
      IF ( ( WI .EQ. B(6) ) .AND. ( WI .EQ. B(9) ) ) GOTO 4110
      IF ( ( WI .EQ. B(5) ) .AND. ( WI .EQ. B(7) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5040 WI = B(4)
      IF ( ( WI .EQ. B(5) ) .AND. ( WI .EQ. B(6) ) ) GOTO 4110
      IF ( ( WI .EQ. B(1) ) .AND. ( WI .EQ. B(7) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5050 WI = B(5)
      IF ( ( WI .EQ. B(1) ) .AND. ( WI .EQ. B(9) ) ) GOTO 4110
      IF ( ( WI .EQ. B(3) ) .AND. ( WI .EQ. B(7) ) ) GOTO 4110
      IF ( ( WI .EQ. B(2) ) .AND. ( WI .EQ. B(8) ) ) GOTO 4110
      IF ( ( WI .EQ. B(4) ) .AND. ( WI .EQ. B(6) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5060 WI = B(6)
      IF ( ( WI .EQ. B(4) ) .AND. ( WI .EQ. B(5) ) ) GOTO 4110
      IF ( ( WI .EQ. B(3) ) .AND. ( WI .EQ. B(9) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5070 WI = B(7)
      IF ( ( WI .EQ. B(8) ) .AND. ( WI .EQ. B(9) ) ) GOTO 4110
      IF ( ( WI .EQ. B(1) ) .AND. ( WI .EQ. B(4) ) ) GOTO 4110
      IF ( ( WI .EQ. B(5) ) .AND. ( WI .EQ. B(3) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5080 WI = B(8)
      IF ( ( WI .EQ. B(7) ) .AND. ( WI .EQ. B(9) ) ) GOTO 4110
      IF ( ( WI .EQ. B(2) ) .AND. ( WI .EQ. B(5) ) ) GOTO 4110
      WI = 0
      GO TO 4110

 5090 WI = B(9)
      IF ( ( WI .EQ. B(7) ) .AND. ( WI .EQ. B(8) ) ) GOTO 4110
      IF ( ( WI .EQ. B(3) ) .AND. ( WI .EQ. B(6) ) ) GOTO 4110
      IF ( ( WI .EQ. B(1) ) .AND. ( WI .EQ. B(5) ) ) GOTO 4110
      WI = 0
      GO TO 4110

      END

