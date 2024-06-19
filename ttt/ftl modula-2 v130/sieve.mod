(*********************************************************************)
(*                  The Sieve Benchmark                              *)
(*                                                                   *)
(*  Compile with test, overflow switches off and native code on.     *)
(*                                                                   *)
(*********************************************************************)
MODULE sieve;

FROM Djl IMPORT Write;

CONST
  size = 8190;

VAR
  flags : ARRAY [ 0 .. size ] OF BOOLEAN;
  i, prime, k, count, iter : CARDINAL;
  ch : CHAR;

PROCEDURE WriteCardinal( x : CARDINAL );
VAR
    val : ARRAY[0..63] OF CHAR;
    i, l : CARDINAL;
BEGIN
    IF ( x = 0 ) THEN
        Write( '0' );
    ELSE
        l := 0;
        REPEAT
            i := x MOD 10;
            val[ l ] := CHAR( i + 48 );
            l := l + 1;
            x := x DIV 10;
        UNTIL x = 0;
        i := l;
        REPEAT
            Write( val[ i - 1 ] );
            i := i - 1;
        UNTIL i = 0;
    END;
END WriteCardinal;

PROCEDURE WriteLn;
BEGIN
    Write( CHAR( 13 ) );
    Write( CHAR( 10 ) );
END WriteLn;

PROCEDURE WriteString(s:ARRAY OF CHAR);
VAR i : CARDINAL;
BEGIN
    i:=0;
    LOOP
        IF i>HIGH(s) THEN EXIT END;
        IF s[i]=0c THEN EXIT END;
        Write(s[i]);
        i:=i+1;
    END;
END WriteString;

BEGIN

  FOR iter := 1 TO 10 DO
    count := 0;
    FOR i := 0 TO size DO flags[i] := TRUE END;
    FOR i := 0 TO size DO
      IF flags[i] THEN
        prime := i + i + 3;
        k := i + prime;
        WHILE k <= size DO
          flags[k] := FALSE;
          k := k + prime;
        END;
        count := count + 1;
      END;
    END;
  END;
  WriteString( "count of primes: " );
  WriteCardinal( count );
  WriteLn;
END sieve.
