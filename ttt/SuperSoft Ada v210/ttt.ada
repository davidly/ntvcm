pragma optimize(time);
--    SuperSoft Ada v2.1 of an app that proves you can't win at tic-tac-toe if the opponent is competent.
--    Build like this:
--        ntvcm ada %1

procedure tttada is

ScoreWin : Constant integer := 6;
ScoreTie : Constant integer := 5;
ScoreLose : Constant integer :=  4;
ScoreMax : Constant integer := 9;
ScoreMin : Constant integer := 2;
DefaultIterations : Constant integer := 1;

PieceX : Constant integer := 1;
PieceO : Constant integer:= 2;
PieceBlank : Constant integer := 0;

moves : integer;
board : Array(0..8) of integer;
i, iterations : integer;

function LookForWinner return integer is
p : integer;
begin
    p := board(0);
    if ( PieceBlank /= p ) then
        if ( ( p = board(1) ) and ( p = board(2) ) ) then return p; end if;
        if ( ( p = board(3) ) and ( p = board(6) ) ) then return p; end if;
    end if;

    p := board(3);
    if ( ( PieceBlank /= p ) and ( p = board(4) ) and ( p = board(5) ) ) then return p; end if;

    p := board(6);
    if ( ( PieceBlank /= p ) and ( p = board(7) ) and ( p = board(8) ) ) then return p; end if;

    p := board(1);
    if ( PieceBlank /= p and p = board(4) and p = board(7) ) then return p; end if;

    p := board(2);
    if ( PieceBlank /= p and p = board(5) and p = board(8) ) then return p; end if;

    p := board(4);
    if ( PieceBlank /= p ) then
        if ( ( p = board(0) ) and ( p = board(8) ) ) then return p; end if;

        if ( ( p = board(2) ) and ( p = board(6) ) ) then return p; end if;
    end if;

    return PieceBlank;
end LookForWinner;

function pos0func return integer is
x : integer;
begin
    x := board( 0 );
    if ( ( x = board(1) and x = board(2) ) or
         ( x = board(3) and x = board(6) ) or
         ( x = board(4) and x = board(8) ) ) then return x; end if;
    return PieceBlank;
end pos0func;

function pos1func return integer is
x : integer;
begin
    x := board( 1 );
    if ( ( x = board(0) and x = board(2) ) or
         ( x = board(4) and x = board(7) ) ) then return x; end if;
    return PieceBlank;
end pos1func;

function pos2func return integer is
x : integer;
begin
    x := board( 2 );
    if ( ( x = board(0) and x = board(1) ) or
         ( x = board(5) and x = board(8) ) or
         ( x = board(4) and x = board(6) ) ) then return x; end if;
    return PieceBlank;
end pos2func;

function pos3func return integer is
x : integer;
begin
    x := board( 3 );
    if ( ( x = board(4) and x = board(5) ) or
         ( x = board(0) and x = board(6) ) ) then return x; end if;
    return PieceBlank;
end pos3func;

function pos4func return integer is
x : integer;
begin
    x := board( 4 );
    if ( ( x = board(0) and x = board(8) ) or
         ( x = board(2) and x = board(6) ) or
         ( x = board(1) and x = board(7) ) or
         ( x = board(3) and x = board(5) ) ) then return x; end if;
    return PieceBlank;
end pos4func;

function pos5func return integer is
x : integer;
begin
    x := board( 5 );
    if ( ( x = board(3) and x = board(4) ) or
         ( x = board(2) and x = board(8) ) ) then return x; end if;
    return PieceBlank;
end pos5func;

function pos6func return integer is
x : integer;
begin
    x := board( 6 );
    if ( ( x = board(7) and x = board(8) ) or
         ( x = board(0) and x = board(3) ) or
         ( x = board(4) and x = board(2) ) ) then return x; end if;
    return PieceBlank;
end pos6func;

function pos7func return integer is
x : integer;
begin
    x := board( 7 );
    if ( ( x = board(6) and x = board(8) ) or
         ( x = board(1) and x = board(4) ) ) then return x; end if;
    return PieceBlank;
end pos7func;

function pos8func return integer is
x : integer;
begin
    x := board( 8 );
    if ( ( x = board(6) and x = board(7) ) or
         ( x = board(2) and x = board(5) ) or
         ( x = board(0) and x = board(4) ) ) then return x; end if;
    return PieceBlank;
end pos8func;

function MinMax( alphaarg : in integer; betaarg : in integer; depth : in integer; move : in integer ) return integer is
alpha, beta, p, value, score, pieceMove : integer;
begin
    moves := moves + 1;

    if ( depth >= 4 ) then
        --p := LookForWinner();  -- this is much slower than the posXfunc solution

        -- syntax error trying to use this: type scoreproc is access function return integer;
        case move is
            when 0 => p := pos0func();
            when 1 => p := pos1func();
            when 2 => p := pos2func();
            when 3 => p := pos3func();
            when 4 => p := pos4func();
            when 5 => p := pos5func();
            when 6 => p := pos6func();
            when 7 => p := pos7func();
            when 8 => p := pos8func();
        end case;

        if ( PieceBlank /= p ) then
            if ( PieceX = p ) then return ScoreWin; end if;
            return ScoreLose;
        end if;

        if ( 8 = depth ) then return ScoreTie; end if;
    end if;

    alpha := alphaarg;
    beta := betaarg;

    if ( pieceO = board( move ) ) then -- a bitwise operator on depth would be faster
        value := ScoreMin;
        pieceMove := PieceX;
    else
        value := ScoreMax;
        pieceMove := PieceO;
    end if;

    for p in 0..8 loop
        if ( PieceBlank = board( p ) ) then
            board( p ) := pieceMove;
            score := MinMax( alpha, beta, depth + 1, p );
            board( p ) := pieceBlank;

            if ( PieceX = pieceMove ) then
                if ( score = ScoreWin ) then return ScoreWin; end if;
                if ( score > value ) then
                    if ( score >= beta ) then return score; end if;
                    value := score;
                    if ( value > alpha ) then alpha := value; end if;
                end if;
            else
                if ( score = ScoreLose ) then return ScoreLose; end if;
                if ( score < value ) then
                    if ( score <= alpha ) then return score; end if;
                    value := score;
                    if ( value < beta ) then beta := value; end if;
                end if;
            end if;
        end if;
    end loop;

    return value;
end MinMax;

procedure FindSolution( move : in integer ) is
score : integer;
begin
    board( move ) := PieceX;
    score := MinMax( ScoreMin, ScoreMax, 0, move );
    board( move ) := PieceBlank;
end FindSolution;

begin
    iterations := DefaultIterations;

    for i in 0..8 loop
        board( i ) := PieceBlank;
    end loop;

    Put( "starting..." ); New_line;

    for i in 1..iterations loop
        moves := 0;
        FindSolution( 0 );
        FindSolution( 1 );
        FindSolution( 4 );
    end loop;

    Put( "Moves:      " ); Put( moves ); New_line;
    Put( "Iterations: " ); Put( iterations ); New_line;
end tttada;
