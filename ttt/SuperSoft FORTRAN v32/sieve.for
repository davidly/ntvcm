C Eratosthenes Sieve from BYTE magazine
#      PROGRAM SIEVE
      LOGICAL FLAGS( 8191 )
      INTEGER*2 I, PRIME, K, COUNT
      INTEGER*1 ITER

      DO 92 ITER = 1, 10
      COUNT = 0
      DO 10 I = 0, 8190
 10   FLAGS( I ) = .TRUE.
      DO 91 I = 0, 8190
      IF ( FLAGS( I ) .EQ. .FALSE. ) GOTO 91
      PRIME = I + I + 3
      K = I + PRIME
 20   IF ( K .GT. 8190 ) GOTO 90
      FLAGS( K ) = .FALSE.
      K = K + PRIME
      GOTO 20
 90   COUNT = COUNT + 1
 91   CONTINUE
 92   CONTINUE
      WRITE( 1, 200 ) COUNT
 200  FORMAT( 1X, I6 )
      STOP
      END

