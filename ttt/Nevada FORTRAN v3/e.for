C      PROGRAM E
      INTEGER HIGH, N, X
      INTEGER A(200)

      HIGH = 200
      X = 0
      N = HIGH - 1

  150 IF ( N .LE. 0 ) GOTO 200
      A( N + 1 ) = 1
      N = N - 1
      GOTO 150

  200 A( 2 ) = 2
      A( 1 ) = 0
  220 IF ( HIGH .LE. 9 ) GOTO 400
      HIGH = HIGH - 1
      N = HIGH
  240 IF ( N .EQ. 0 ) GOTO 300
      A( N + 1 ) = MOD( X, N )
      X = ( 10 * A( N ) ) + ( X / N )
      N = N - 1
      GOTO 240
  300 IF ( X .GE. 10 ) GOTO 320
      WRITE( 1, 1001 ) X
      GOTO 220
  320 WRITE( 1, 1002 ) X
      GOTO 220
  400 STOP
 1001 FORMAT( I1, Z )
 1002 FORMAT( I2, Z )
      END



