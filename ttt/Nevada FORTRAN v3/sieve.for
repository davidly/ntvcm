C Eratosthenes Sieve from BYTE magazine
C nevada fortran fails for an array of size 8191, so just run a subset
C should produce a count of 308 for an array size of 1023
C should produce a count of 551 for an array size of 2001
C      PROGRAM SIEVE
      INTEGER FLAGS( 2001 )
      INTEGER I, PRIME, K, COUNT
      INTEGER ITER

      DO 96 ITER = 1, 10
      COUNT = 0
      DO 10 I = 0, 2000
      FLAGS( I ) = 1
  10  CONTINUE
      DO 95 I = 0, 2000
      IF ( FLAGS( I ) .EQ. 0 ) GOTO 95
      PRIME = I + I + 3
      K = I + PRIME
 20   IF ( K .GT. 2000 ) GOTO 90
      FLAGS( K ) = 0
      K = K + PRIME
      GOTO 20
 90   COUNT = COUNT + 1
 95   CONTINUE
 96   CONTINUE
      WRITE (1, 100) COUNT
      STOP
 100  FORMAT( I6 )
      END

