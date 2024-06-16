/*
   This version builds with old compilers including:
       Aztec C 1.06 for 8080 & Z80 on CP/M.
       Microsoft C Compiler V1.04 for 8086 on DOS. (This is Lattice C)
       Microsoft C Compiler V2.03 for 8086 on DOS. (Still Lattice C)
       Microsoft C Compiler V3.00 for 8086 on DOS.
       QuickC 1.0
       Turbo C 2.0
   The syntax is old and reminds me of 7th grade summer vacation.
   Much of this code is awkward to satisfy the lowest common denominator of many compilers.
   unsigned long isn't supported in many older compilers, so long is used instead.
   Early DOS and CP/M require register variabes to be int, not char or other types.
   The perf improvement of using register-int instead of stack-char is worth it.
*/

#define LINT_ARGS

#include "printf.c"

#define true 1
#define false 0

#define ScoreWin 6
#define ScoreTie 5
#define ScoreLose  4
#define ScoreMax 9
#define ScoreMin 2
#define DefaultIterations 1

#define PieceX 1
#define PieceO 2
#define PieceBlank 0

#define ttype int

ttype g_board[ 9 ];
int g_Iterations;
int g_Moves;

ttype winner2( move ) ttype move;
{
    int x;  /* faster than ttype x on the stack */

    switch( move ) /* msc v3 from 1985 generates a jump table! */
    {
        case 0:
        {
            x = g_board[ 0 ];
            if ( ( ( x == g_board[1] ) && ( x == g_board[2] ) ) ||
                 ( ( x == g_board[3] ) && ( x == g_board[6] ) ) ||
                 ( ( x == g_board[4] ) && ( x == g_board[8] ) ) )
               return x;
            break;
        }
        case 1:
        {
            x = g_board[ 1 ];
            if ( ( ( x == g_board[0] ) && ( x == g_board[2] ) ) ||
                 ( ( x == g_board[4] ) && ( x == g_board[7] ) ) )
                return x;
            break;
        }
        case 2:
        {
            x = g_board[ 2 ];
            if ( ( ( x == g_board[0] ) && ( x == g_board[1] ) ) ||
                 ( ( x == g_board[5] ) && ( x == g_board[8] ) ) ||
                 ( ( x == g_board[4] ) && ( x == g_board[6] ) ) )
                return x;
            break;
        }
        case 3:
        {
            x = g_board[ 3 ];
            if ( ( ( x == g_board[4] ) && ( x == g_board[5] ) ) ||
                 ( ( x == g_board[0] ) && ( x == g_board[6] ) ) )
                return x;
            break;
        }
        case 4:
        {
            x = g_board[ 4 ];
            if ( ( ( x == g_board[0] ) && ( x == g_board[8] ) ) ||
                 ( ( x == g_board[2] ) && ( x == g_board[6] ) ) ||
                 ( ( x == g_board[1] ) && ( x == g_board[7] ) ) ||
                 ( ( x == g_board[3] ) && ( x == g_board[5] ) ) )
                return x;
            break;
        }
        case 5:
        {
            x = g_board[ 5 ];
            if ( ( ( x == g_board[3] ) && ( x == g_board[4] ) ) ||
                 ( ( x == g_board[2] ) && ( x == g_board[8] ) ) )
                return x;
            break;
        }
        case 6:
        {
            x = g_board[ 6 ];
            if ( ( ( x == g_board[7] ) && ( x == g_board[8] ) ) ||
                 ( ( x == g_board[0] ) && ( x == g_board[3] ) ) ||
                 ( ( x == g_board[4] ) && ( x == g_board[2] ) ) )
                return x;
            break;
        }
        case 7:
        {
            x = g_board[ 7 ];
            if ( ( ( x == g_board[6] ) && ( x == g_board[8] ) ) ||
                 ( ( x == g_board[1] ) && ( x == g_board[4] ) ) )
                return x;
            break;
        }
        case 8:
        {
            x = g_board[ 8 ];
            if ( ( ( x == g_board[6] ) && ( x == g_board[7] ) ) ||
                 ( ( x == g_board[2] ) && ( x == g_board[5] ) ) ||
                 ( ( x == g_board[0] ) && ( x == g_board[4] ) ) )
                return x;
            break;
         }
    }

    return PieceBlank;
} /*winner2*/

ttype LookForWinner()
{
    int p;
    p = g_board[0]; /* faster as int than ttype on 8086 and Z80 */
    if ( PieceBlank != p )
    {
        if ( p == g_board[1] && p == g_board[2] )
            return p;

        if ( p == g_board[3] && p == g_board[6] )
            return p;
    }

    p = g_board[3];
    if ( PieceBlank != p && p == g_board[4] && p == g_board[5] )
        return p;

    p = g_board[6];
    if ( PieceBlank != p && p == g_board[7] && p == g_board[8] )
        return p;

    p = g_board[1];
    if ( PieceBlank != p && p == g_board[4] && p == g_board[7] )
        return p;

    p = g_board[2];
    if ( PieceBlank != p && p == g_board[5] && p == g_board[8] )
        return p;

    p = g_board[4];
    if ( PieceBlank != p )
    {
        if ( ( p == g_board[0] ) && ( p == g_board[8] ) )
            return p;

        if ( ( p == g_board[2] ) && ( p == g_board[6] ) )
            return p;
    }

    return PieceBlank;
} /*LookForWinner*/

ttype MinMax( alpha, beta, depth, move ) ttype alpha; ttype beta; ttype depth; ttype move;
{
    ttype pieceMove, score;   /* better perf with char than int. out of registers so use stack */
    int p, value;    /* better perf with these as an int on Z80, 8080, and 8086 */

    g_Moves++;

    if ( depth >= 4 )
    {
        p = winner2( move );
/*
        p = LookForWinner();
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
        if ( PieceBlank == g_board[ p ] )
        {
            g_board[p] = pieceMove;
            score = MinMax( alpha, beta, depth + 1, p );
            g_board[p] = PieceBlank;

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
    int i;

    for ( i = 0; i < 9; i++ )
        g_board[ i ] = PieceBlank;

    g_board[ position ] = PieceX;

    for ( i = 0; i < g_Iterations; i++ )
    {
        g_Moves = 0;
        MinMax( ScoreMin, ScoreMax, 0, position );
    }

    return g_Moves;
} /*FindSolution*/

int main( argc, argv ) int argc; char * argv[];
{
    int moves;
    g_Iterations = DefaultIterations;
    g_Moves = 0;

    if ( 2 == argc )
        g_Iterations = atoi( argv[ 1 ] );

    moves = FindSolution( 0 );
    moves += FindSolution( 1 );
    moves += FindSolution( 4 );

    printf( "move count:      %d\n", moves ); /* 6493 * g_Iterations */
    printf( "iteration count: %d\n", g_Iterations );
    return 0;
} /*main*/

#include "stdlib.c"

