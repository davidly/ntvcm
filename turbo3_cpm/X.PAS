{ App to prove you can't win at Tic-Tac-Toe }

{ If this flag isn't set, recursion doesn't work on CP/M }
{$A-}
{ This flag turns off checking if the recursion stack and heap collide }
{$K-}

program ttt;

const
  scoreWin = 6;
  scoreTie = 5;
  scoreLose = 4;
  scoreMax = 9;
  scoreMin = 2;
  scoreInvalid = 0;

  pieceBlank = 0;
  pieceX = 1;
  pieceO = 2;

  iterations = 4;

type
  boardType = array[ 0..8 ] of Integer;

var
  evaluated: Integer;
  board: boardType;

function min( x: Integer; y: Integer ): Integer;
begin
  if y < x then min := y else min := x;
end;

function max( x: Integer; y: Integer ): Integer;
begin
  if y > x then max := y else max := x;
end;

procedure dumpBoard;
var
  i : Integer;
begin
  Write( '{' );
  for i := 0 to 8 do
    Write( board[i] );
  Write( '}' );
end;

function lookForWinner : Integer;
var
  t, p : Integer;
begin
  p := pieceBlank;
  t := board[ 0 ];
  if ( pieceBlank <> t ) and ( ( ( t = board[1] ) and ( t = board[2] ) ) or
                               ( ( t = board[3] ) and ( t = board[6] ) ) ) then
    p := t
  else
  begin
    t := board[1];
    if ( t <> pieceBlank ) and ( t = board[4] ) and ( t = board[7] ) then
      p := t
    else
    begin
      t := board[2];
      if ( t <> pieceBlank ) and ( t = board[5] ) and ( t = board[8] ) then
        p := t
      else
      begin
        t := board[3];
        if ( t <> pieceBlank ) and ( t = board[4] ) and ( t = board[5] ) then
          p := t
        else
        begin
          t := board[6];
          if ( t <> pieceBlank ) and ( t = board[7] ) and ( t = board[8] ) then
            p := t
          else
          begin
            t := board[4];
            if ( t <> pieceBlank ) and ( ( ( t = board[0] ) and ( t = board[8] ) ) or
                                         ( ( t = board[2] ) and ( t = board[6] ) ) ) then
              p := t;
          end;
        end;
      end;
    end;
  end;

  lookForWinner := p;
end;

function minmax( alpha: Integer; beta: Integer; depth: Integer ): Integer;
var
  p, value, pieceMove, score : Integer;
  maximize, done: boolean;
begin
  evaluated := evaluated + 1;
  value := scoreInvalid;

  if depth >= 4 then
  begin
    p := lookForWinner;
    if p = pieceX then
      value := scoreWin
    else if p = pieceO then
      value := scoreLose
    else if depth = 8 then
      value := scoreTie;
  end;

  if value = scoreInvalid then
  begin
    maximize := 0 <> ( depth AND 1 );
    if maximize then
    begin
        value := scoreMin;
        pieceMove := pieceX;
    end
    else
    begin
        value := scoreMax;
        pieceMove := pieceO;
    end;

    done := false;
    p := 0;
    repeat
      if board[ p ] = pieceBlank then
      begin
        board[ p ] := pieceMove;
        score := minmax( alpha, beta, depth + 1 );
        board[ p ] := pieceBlank;

        if maximize then
        begin
          value := max( value, score );
          if value = scoreWin then
            done := true
          else
          begin
            alpha := max( alpha, value );
            if alpha >= beta then
              done := true;
          end
        end
        else
        begin
          value := min( value, score );
          if value = scoreLose then
            done := true
          else
          begin
            beta := min( value, beta );
            if beta <= alpha then
              done := true;
          end;
        end;
      end;

      p := p + 1;
      if p > 8 then
        done := true;

    until done;
  end;

  minmax := value;
end;

procedure runit( move : Integer );
var
  score: Integer;
begin
  board[move] := pieceX;
  score := minmax( scoreMin, scoreMax, 0 );
  board[move] := pieceBlank;

  if 5 <> score then
  begin
    Write( 'unexpected score: ' ); Write( score ); WriteLn;
  end;
end;

var
  i: Integer;
begin
  for i := 0 to 8 do
    board[i] := pieceBlank;

  for i := 1 to Iterations do
  begin
    evaluated := 0;  { once per loop to prevent overflow of 16-bit int }
    runit( 0 );
    runit( 1 );
    runit( 4 );
    Write( '.' );
  end;

  Write( 'moves evaluated: ' ); Write( evaluated ); WriteLn;
end.
