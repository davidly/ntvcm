#define LINT_ARGS

#define true 1
#define false 0

#define ABPrune true    
#define WinLosePrune true
#define ScoreWin 6
#define ScoreTie 5
#define ScoreLose  4
#define ScoreMax 9
#define ScoreMin 2
#define DefaultIterations 10

#define PieceX 1
#define PieceO 2
#define PieceBlank 0

#define ttype int

ttype winner2( move, board ) ttype move; ttype * board;
{
    int x;  /* faster than ttype x on the stack */

    switch( move ) /* msc v3 from 1985 generates a jump table! */
    {
        case 0:
        {
            x = board[ 0 ];
            if ( ( ( x == board[1] ) && ( x == board[2] ) ) ||
                 ( ( x == board[3] ) && ( x == board[6] ) ) ||
                 ( ( x == board[4] ) && ( x == board[8] ) ) )
               return x;
            break;
        }
        case 1:
        {
            x = board[ 1 ];
            if ( ( ( x == board[0] ) && ( x == board[2] ) ) ||
                 ( ( x == board[4] ) && ( x == board[7] ) ) )
                return x;
            break;
        }
        case 2:
        {
            x = board[ 2 ];
            if ( ( ( x == board[0] ) && ( x == board[1] ) ) ||
                 ( ( x == board[5] ) && ( x == board[8] ) ) ||
                 ( ( x == board[4] ) && ( x == board[6] ) ) )
                return x;
            break;
        }
        case 3:
        {
            x = board[ 3 ];
            if ( ( ( x == board[4] ) && ( x == board[5] ) ) ||
                 ( ( x == board[0] ) && ( x == board[6] ) ) )
                return x;
            break;
        }
        case 4:
        {
            x = board[ 4 ];
            if ( ( ( x == board[0] ) && ( x == board[8] ) ) ||
                 ( ( x == board[2] ) && ( x == board[6] ) ) ||
                 ( ( x == board[1] ) && ( x == board[7] ) ) ||
                 ( ( x == board[3] ) && ( x == board[5] ) ) )
                return x;
            break;
        }
        case 5:
        {
            x = board[ 5 ];
            if ( ( ( x == board[3] ) && ( x == board[4] ) ) ||
                 ( ( x == board[2] ) && ( x == board[8] ) ) )
                return x;
            break;
        }
        case 6:
        {
            x = board[ 6 ];
            if ( ( ( x == board[7] ) && ( x == board[8] ) ) ||
                 ( ( x == board[0] ) && ( x == board[3] ) ) ||
                 ( ( x == board[4] ) && ( x == board[2] ) ) )
                return x;
            break;
        }
        case 7:
        {
            x = board[ 7 ];
            if ( ( ( x == board[6] ) && ( x == board[8] ) ) ||
                 ( ( x == board[1] ) && ( x == board[4] ) ) )
                return x;
            break;
        }
        case 8:
        {
            x = board[ 8 ];
            if ( ( ( x == board[6] ) && ( x == board[7] ) ) ||
                 ( ( x == board[2] ) && ( x == board[5] ) ) ||
                 ( ( x == board[0] ) && ( x == board[4] ) ) )
                return x;
            break;
         }
    }

    return PieceBlank;
} /*winner2*/

ttype LookForWinner( board ) int * board;
{
    int p;
    p = board[0]; /* faster as int than ttype on 8086 and Z80 */
    if ( PieceBlank != p )
    {
        if ( p == board[1] && p == board[2] )
            return p;

        if ( p == board[3] && p == board[6] )
            return p;
    }

    p = board[3];
    if ( PieceBlank != p && p == board[4] && p == board[5] )
        return p;

    p = board[6];
    if ( PieceBlank != p && p == board[7] && p == board[8] )
        return p;

    p = board[1];
    if ( PieceBlank != p && p == board[4] && p == board[7] )
        return p;

    p = board[2];
    if ( PieceBlank != p && p == board[5] && p == board[8] )
        return p;

    p = board[4];
    if ( PieceBlank != p )
    {
        if ( ( p == board[0] ) && ( p == board[8] ) )
            return p;

        if ( ( p == board[2] ) && ( p == board[6] ) )
            return p;
    }

    return PieceBlank;
} /*LookForWinner*/

ttype MinMax( alpha, beta, depth, move, moves, board )
    ttype alpha; ttype beta; ttype depth; ttype move; int * moves; int *board;
{
    ttype pieceMove, score;   /* better perf with char than int. out of registers so use stack */
    int p, value;    /* better perf with these as an int on Z80, 8080, and 8086 */

    (*moves)++;

    if ( depth >= 4 )
    {
        p = winner2( move, board );
/*
        p = LookForWinner( board );
*/

        if ( PieceBlank != p )
        {
            if ( PieceX == p )
                return ScoreWin;

            return ScoreLose;
        }

        if ( 8 == depth )
            return ScoreTie;
    }

    if ( depth & 1 ) 
    {
        value = ScoreMin;
        pieceMove = PieceX;
    }
    else
    {
        value = ScoreMax;
        pieceMove = PieceO;
    }

    for ( p = 0; p < 9; p++ )
    {
        if ( PieceBlank == board[ p ] )
        {
            board[p] = pieceMove;
            score = MinMax( alpha, beta, depth + 1, p, moves, board );
            board[p] = PieceBlank;

            if ( depth & 1 ) 
            {
                if ( ScoreWin == score )
                    return ScoreWin;

                if ( score > value )
                {
                    value = score;

                    if ( value >= beta )
                        return value;
                    if ( value > alpha )
                        alpha = value;
                }
            }
            else
            {
                if ( ScoreLose == score )
                    return ScoreLose;

                if ( score < value )
                {
                    value = score;

                    if ( value <= alpha )
                        return value;
                    if ( value < beta )
                        beta = value;
                }
            }
        }
    }

    return value;
}  /*MinMax*/

int FindSolution( position ) ttype position;
{
    int i, moves;
    ttype board[ 9 ];

    for ( i = 0; i < 9; i++ )
        board[ i ] = PieceBlank;

    board[ position ] = PieceX;

    for ( i = 0; i < DefaultIterations; i++ )
    {
        moves = 0;
        MinMax( ScoreMin, ScoreMax, 0, position, &moves, board );
    }

    return moves;
} /*FindSolution*/

int strlen( p ) char * p;
{
    int l;
    l = 0;

    while ( 0 != p[l] )
        l++;

    return l;
}

int reverse( p ) char * p;
{
    int r, l;
    char t;

    r = strlen( p );
    if ( 0 == r )
        return 0;

    r--;
    l = 0;
    while ( l < r )
    {
        t = p[ l ];
        p[ l ] = p[ r ];
        p[ r ] = t;
        l++;
        r--;
    }

    return 0;
}

int itoa( p, i ) char * p; int i;
{
    int x, len;

    if ( 0 == i )
    {
        p[0] = '0';
        p[1] = 0;
        return 1;
    }

    len = 0;
    while ( 0 != i )
    {
        x = i % 10;
        p[ len ] = '0' + x;
        len++;
        i /= 10;
    }

    p[ len ] = 0;

    reverse( p );
    return len;
}

int putstring( p ) char * p;
{
    int l;
    l = strlen( p );
    write( 0, p, l );
}

int main( argc, argv ) int argc; char * argv[];
{
    int moves, len;
    char acmoves[ 10 ];

    moves = FindSolution( 0 );
    moves += FindSolution( 1 );
    moves += FindSolution( 4 );

    write( 0, "hello from ttt!\n", 16 );

    putstring( "moves: " );
    len = itoa( acmoves, moves );
    putstring( acmoves );
    putstring( "\n" );

    putstring( "iterations: " );
    len = itoa( acmoves, DefaultIterations );
    putstring( acmoves );
    putstring( "\n" );
/*
    printf( "move count:      %d\n", moves );
    printf( "iteration count: %d\n", g_Iterations );
*/
    return 0;
} /*main*/

