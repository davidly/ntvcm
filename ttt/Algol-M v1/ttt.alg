begin

comment An Algol-M implementation of proving you can't win at tic-tac-toe
        if the opponent is competent.
        Build and run like this on CP/M or with the NTVCM emulator:
            ntvcm algolm ttt
            ntvcm /p runalg ttt
;

comment I can't find a way to declare constants in Algol-M
    scoreWin      6
    scoreTie      5
    scoreLose     4
    scoreMax      9
    scoreMin      2
    scoreInvalid  0
    pieceX        1
    pieceY        2
    pieceBlank    0
;

integer movecount;
integer array board[ 0:8 ];

integer function winner;
begin
    integer t, p;

    p := 0;
    t := board[ 0 ];
    if 0 <> t then begin
        if ( ( ( t = board[1] ) and ( t = board[2] ) ) or
             ( ( t = board[3] ) and ( t = board[6] ) ) ) then
            p := t;
    end;

    if 0 = p then begin
        t := board[1];
        if ( 0 <> t ) and ( t = board[4] ) and ( t = board[7] ) then
            p := t
        else begin
            t := board[2];
            if ( 0 <> t ) and ( t = board[5] ) and ( t = board[8] ) then
                p := t
            else begin
                t := board[3];
                if ( 0 <> t ) and ( t = board[4] ) and ( t = board[5] ) then
                    p := t
                else begin
                    t := board[6];
                    if ( 0 <> t ) and ( t = board[7] ) and ( t = board[8] ) then
                        p := t
                    else begin
                        t := board[4];
                        if ( 0 <> t ) then begin
                            if ( ( ( t = board[0] ) and ( t = board[8] ) ) or
                                 ( ( t = board[2] ) and ( t = board[6] ) ) ) then
                                p := t;
                        end;
                    end;
                end;
            end;
        end;
    end;
  
    winner := p;
end;

integer function winner2( move );
    integer move;
begin
    integer x;
    x := board[ move ];
    case move of begin
        begin
        if not ( ( ( x = board[1] ) and ( x = board[2] ) ) or
                 ( ( x = board[3] ) and ( x = board[6] ) ) or
                 ( ( x = board[4] ) and ( x = board[8] ) ) )
            then x := 0;
        end;
        begin
        if not ( ( ( x = board[0] ) and ( x = board[2] ) ) or
                 ( ( x = board[4] ) and ( x = board[7] ) ) )
            then x := 0;
        end;
        begin
        x := board[ 2 ];
        if not ( ( ( x = board[0] ) and ( x = board[1] ) ) or
                 ( ( x = board[5] ) and ( x = board[8] ) ) or
                 ( ( x = board[4] ) and ( x = board[6] ) ) )
            then x := 0;
        end;
        begin
        x := board[ 3 ];
        if not ( ( ( x = board[4] ) and ( x = board[5] ) ) or
                 ( ( x = board[0] ) and ( x = board[6] ) ) )
            then x := 0;
        end;
        begin
        x := board[ 4 ];
        if not ( ( ( x = board[0] ) and ( x = board[8] ) ) or
                 ( ( x = board[2] ) and ( x = board[6] ) ) or
                 ( ( x = board[1] ) and ( x = board[7] ) ) or
                 ( ( x = board[3] ) and ( x = board[5] ) ) )
            then x := 0;
        end;
        begin
        x := board[ 5 ];
        if not ( ( ( x = board[3] ) and ( x = board[4] ) ) or
                 ( ( x = board[2] ) and ( x = board[8] ) ) )
            then x := 0;
        end;
        begin
        x := board[ 6 ];
        if not ( ( ( x = board[7] ) and ( x = board[8] ) ) or
                 ( ( x = board[0] ) and ( x = board[3] ) ) or
                 ( ( x = board[4] ) and ( x = board[2] ) ) )
            then x := 0;
        end;
        begin
        x := board[ 7 ];
        if not ( ( ( x = board[6] ) and ( x = board[8] ) ) or
                 ( ( x = board[1] ) and ( x = board[4] ) ) )
            then x := 0;
        end;
        begin
        x := board[ 8 ];
        if not ( ( ( x = board[6] ) and ( x = board[7] ) ) or
                 ( ( x = board[2] ) and ( x = board[5] ) ) or
                 ( ( x = board[0] ) and ( x = board[4] ) ) )
            then x := 0;
        end;
    end;

    winner2 := x;
end;

integer function minmax( alpha, beta, depth, move );
    integer alpha, beta, depth, move;
begin
    integer value, p, score, pieceMove;
    value := 0;

    movecount := movecount + 1;

    if depth >= 4 then begin
comment winner2 is much faster than winner      p := winner;
        p := winner2( move );
        if p <> 0 then begin
            if p = 1 then value := 6
            else value := 4;
        end
        else begin
            if depth = 8 then value := 5;
        end;
    end;

    if value = 0 then begin
        if 2 = board[ move ] then begin
            value := 2;
            pieceMove := 1;
        end
        else begin
            value := 9;
            pieceMove := 2;
        end;

        p := 0;
        while p <= 8 do begin
            if board[ p ] = 0 then begin
                board[ p ] := pieceMove;
                score := minmax( alpha, beta, depth + 1, p );
                board[ p ] := 0;
            
                if 2 = board[ move ] then begin
                    if score > value then begin
                        value := score;
                        if ( ( value = 6 ) or ( value >= beta ) ) then
                            p := 10
                        else begin
                            if ( value > alpha ) then alpha := value;
                        end;
                    end;
                end
                else begin
                    if score < value then begin
                        value := score;
                        if ( value = 4 ) or ( value <= alpha ) then
                            p := 10
                        else begin
                            if ( value < beta ) then beta := value;
                        end;
                    end;
                end;
            end;
    
            p := p + 1;
        end;
    end;

    minmax := value;
end;

procedure findsolution( move );
    integer move;
begin
    integer result;

    board[ move ] := 1;
    result := minmax( 2, 9, 0, move );
    board[ move ] := 0;
end;

procedure main;
begin
    integer i, j;

    for i := 0 step 1 until 8 do board[ i ] := 0;

    for j := 0 step 1 until 1 do begin
        movecount := 0;

        findsolution( 0 );
        findsolution( 1 );
        findsolution( 4 );
    end;

    write( "moves: " );
    write( movecount );
end;

main;

end
