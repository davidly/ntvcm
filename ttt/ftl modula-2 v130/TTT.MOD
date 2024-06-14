(*$L-*)

MODULE ttt;

FROM Djl IMPORT Write;

(*
FROM ComLine IMPORT commandLine;
FROM Convert IMPORT StrToCard;
*)

CONST
    scoreWin = 6;
    scoreTie = 5;
    scoreLose = 4;
    scoreMax = 9;
    scoreMin = 2;
    scoreInvalid = 0;
  
    pieceBlank = 0;
    pieceX = 1;
    pieceO = 2;

    defaultIterations = 10;

TYPE
    boardType = ARRAY[ 0..8 ] OF CARDINAL;
    scoreProc = PROCEDURE() : CARDINAL;

VAR
    evaluated: CARDINAL;
    board: boardType;
    procs : ARRAY[ 0..8 ] OF scoreProc;

PROCEDURE lookForWinner() : CARDINAL;
VAR t : CARDINAL;
BEGIN
    t := board[ 0 ];
    IF pieceBlank <> t THEN
        IF ( ( ( t = board[1] ) AND ( t = board[2] ) ) OR
           ( ( t = board[3] ) AND ( t = board[6] ) ) ) THEN
            RETURN t;
        END;
    END;

    t := board[1];
    IF ( pieceBlank <> t ) AND ( t = board[4] ) AND ( t = board[7] ) THEN RETURN t; END;

    t := board[2];
    IF ( pieceBlank <> t ) AND ( t = board[5] ) AND ( t = board[8] ) THEN RETURN t; END;

    t := board[3];
    IF ( pieceBlank <> t ) AND ( t = board[4] ) AND ( t = board[5] ) THEN RETURN t; END;

    t := board[6];
    IF ( pieceBlank <> t ) AND ( t = board[7] ) AND ( t = board[8] ) THEN RETURN t; END;

    t := board[4];
    IF pieceBlank <> t THEN
        IF ( ( ( t = board[0] ) AND ( t = board[8] ) ) OR
           ( ( t = board[2] ) AND ( t = board[6] ) ) ) THEN
                RETURN t;
        END;
    END;

    RETURN pieceBlank;
END lookForWinner;

PROCEDURE proc0() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[0];
    IF ( ( ( x = board[1] ) AND ( x = board[2] ) ) OR
         ( ( x = board[3] ) AND ( x = board[6] ) ) OR
         ( ( x = board[4] ) AND ( x = board[8] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc0;

PROCEDURE proc1() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[1];
    IF ( ( ( x = board[0] ) AND ( x = board[2] ) ) OR
         ( ( x = board[4] ) AND ( x = board[7] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc1;

PROCEDURE proc2() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[2];
    IF ( ( ( x = board[0] ) AND ( x = board[1] ) ) OR
         ( ( x = board[5] ) AND ( x = board[8] ) ) OR
         ( ( x = board[4] ) AND ( x = board[6] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc2;

PROCEDURE proc3() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[3];
    IF ( ( ( x = board[4] ) AND ( x = board[5] ) ) OR
         ( ( x = board[0] ) AND ( x = board[6] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc3;

PROCEDURE proc4() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[4];
    IF ( ( ( x = board[0] ) AND ( x = board[8] ) ) OR
         ( ( x = board[2] ) AND ( x = board[6] ) ) OR
         ( ( x = board[1] ) AND ( x = board[7] ) ) OR
         ( ( x = board[3] ) AND ( x = board[5] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc4;

PROCEDURE proc5() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[5];
    IF ( ( ( x = board[3] ) AND ( x = board[4] ) ) OR
         ( ( x = board[2] ) AND ( x = board[8] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc5;

PROCEDURE proc6() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[6];
    IF ( ( ( x = board[7] ) AND ( x = board[8] ) ) OR
         ( ( x = board[0] ) AND ( x = board[3] ) ) OR
         ( ( x = board[4] ) AND ( x = board[2] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc6;

PROCEDURE proc7() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[7];
    IF ( ( ( x = board[6] ) AND ( x = board[8] ) ) OR
         ( ( x = board[1] ) AND ( x = board[4] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc7;

PROCEDURE proc8() : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[8];
    IF ( ( ( x = board[6] ) AND ( x = board[7] ) ) OR
         ( ( x = board[2] ) AND ( x = board[5] ) ) OR
         ( ( x = board[0] ) AND ( x = board[4] ) ) )
        THEN RETURN x; END;
    RETURN pieceBlank;
END proc8;

PROCEDURE winner2( move: CARDINAL ) : CARDINAL;
VAR x : CARDINAL;
BEGIN
    x := board[ move ];
    CASE move OF
        0:  IF NOT ( ( ( x = board[1] ) AND ( x = board[2] ) ) OR
                     ( ( x = board[3] ) AND ( x = board[6] ) ) OR
                     ( ( x = board[4] ) AND ( x = board[8] ) ) )
                THEN RETURN pieceBlank; END; |
        1:  IF NOT ( ( ( x = board[0] ) AND ( x = board[2] ) ) OR
                     ( ( x = board[4] ) AND ( x = board[7] ) ) )
                THEN x := pieceBlank; END; |
        2:  IF NOT ( ( ( x = board[0] ) AND ( x = board[1] ) ) OR
                     ( ( x = board[5] ) AND ( x = board[8] ) ) OR
                     ( ( x = board[4] ) AND ( x = board[6] ) ) )
                THEN x := pieceBlank; END; |
        3:  IF NOT ( ( ( x = board[4] ) AND ( x = board[5] ) ) OR
                     ( ( x = board[0] ) AND ( x = board[6] ) ) )
                THEN x := pieceBlank; END; |
        4:  IF NOT ( ( ( x = board[0] ) AND ( x = board[8] ) ) OR
                     ( ( x = board[2] ) AND ( x = board[6] ) ) OR
                     ( ( x = board[1] ) AND ( x = board[7] ) ) OR
                     ( ( x = board[3] ) AND ( x = board[5] ) ) )
                THEN x := pieceBlank; END; |
        5:  IF NOT ( ( ( x = board[3] ) AND ( x = board[4] ) ) OR
                     ( ( x = board[2] ) AND ( x = board[8] ) ) )
                THEN x := pieceBlank; END; |
        6:  IF NOT ( ( ( x = board[7] ) AND ( x = board[8] ) ) OR
                     ( ( x = board[0] ) AND ( x = board[3] ) ) OR
                     ( ( x = board[4] ) AND ( x = board[2] ) ) )
                THEN x := pieceBlank; END; |
        7:  IF NOT ( ( ( x = board[6] ) AND ( x = board[8] ) ) OR
                     ( ( x = board[1] ) AND ( x = board[4] ) ) )
                THEN x := pieceBlank; END; |
        8:  IF NOT ( ( ( x = board[6] ) AND ( x = board[7] ) ) OR
                     ( ( x = board[2] ) AND ( x = board[5] ) ) OR
                     ( ( x = board[0] ) AND ( x = board[4] ) ) )
                THEN x := pieceBlank; END;
    END;

    RETURN x;
END winner2;

PROCEDURE minmax( alpha: CARDINAL; beta: CARDINAL; move: CARDINAL; depth: CARDINAL ): CARDINAL;
VAR p, value, pieceMove, score : CARDINAL;
BEGIN
    evaluated := evaluated + 1;
    value := scoreInvalid;
    IF depth >= 4 THEN
        (*
            Times in seconds on a 4Mhz Z80 for one iteration.

            Turbo Modula-2 v1.00 (1986):
                - procs:   5.6059
                - lfw:     5.0526
                - winner2: 4.1878
            FTL Modula-2 v1.30 (1988):
                - procs:   5.4724
                - lfw:     7.0369
                - winner2: 6.0425
            Hochstrasser Computing AG Modula-2 System v2.01
                - procs:   6.0563
                - lfw:     8.1932
                - winner2: 6.4538
        *)

        p := procs[ move ]();
        (* p := lookForWinner(); *)
        (* p := winner2( move ); *)

        IF p <> pieceBlank THEN
            IF p = pieceX THEN
                RETURN scoreWin;
            ELSE
                RETURN scoreLose;
            END;
        ELSIF depth = 8 THEN
            RETURN scoreTie;
        END;
    END;

    IF ODD( depth ) THEN
        value := scoreMin;
        pieceMove := pieceX;
    ELSE
        value := scoreMax;
        pieceMove := pieceO;
    END;

    p := 0;
    REPEAT
        IF board[ p ] = pieceBlank THEN
            board[ p ] := pieceMove;
            score := minmax( alpha, beta, p, depth + 1 );
            board[ p ] := pieceBlank;
      
            IF ODD( depth ) THEN
                IF ( score = scoreWin ) THEN RETURN scoreWin; END;
                IF ( score > value ) THEN
                    value := score;
                    IF ( value >= beta ) THEN RETURN value; END;
                    IF ( value > alpha ) THEN alpha := value; END;
                END;
            ELSE
                IF ( score = scoreLose ) THEN RETURN scoreLose; END;
                IF ( score < value ) THEN
                    value := score;
                    IF ( value <= alpha ) THEN RETURN value; END;
                    IF ( value < beta ) THEN beta := value; END;
                END;
            END;
        END;
        p := p + 1
    UNTIL p > 8;

    RETURN value;
END minmax;

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

PROCEDURE runit( move : CARDINAL );
VAR score : CARDINAL;
BEGIN
    board[move] := pieceX;
    score := minmax( scoreMin, scoreMax, move, 0 );
    board[move] := pieceBlank;
END runit;

VAR
    i, loops : CARDINAL;
    result : BOOLEAN;
    cmd : ARRAY[0..127] OF CHAR;
BEGIN
    WriteString( 'running' );
    WriteLn;

    procs[ 0 ] := proc0;
    procs[ 1 ] := proc1;
    procs[ 2 ] := proc2;
    procs[ 3 ] := proc3;
    procs[ 4 ] := proc4;
    procs[ 5 ] := proc5;
    procs[ 6 ] := proc6;
    procs[ 7 ] := proc7;
    procs[ 8 ] := proc8;

    loops := 0;
(*
    READ( commandLine, cmd );
    result := StrToCard( cmd, loops );
*)
    IF ( loops = 0 ) THEN loops := defaultIterations; END;

    FOR i := 0 TO 8 DO
        board[i] := pieceBlank;
    END;

    FOR i := 1 TO loops DO
        evaluated := 0;  (* once per loop to prevent overflow *)
        runit( 0 );
        runit( 1 );
        runit( 4 );
    END;

    WriteString( "moves evaluated: " );
    WriteCardinal( evaluated );
    WriteLn;

    WriteString( "iterations:      " );
    WriteCardinal( loops );
    WriteLn;
(*
    WRITELN( "moves evaluated: ", evaluated );
    WRITELN( "iterations:      ", loops );
*)
END ttt.

