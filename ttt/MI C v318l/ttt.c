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
   Early DOS and CP/M require variabes to be int, not char or other types.
   The perf improvement of using register-int instead of stack-char is worth it.
*/

#define LINT_ARGS

#include "stdio.h"

#define true 1
#define false 0

/* Function Pointers are the fastest implementation for almost every compiler */
#define UseFunPointers 1
#define UseWinner2 2
#define UseLookForWinner 3
#define WinMethod UseFunPointers

#define ABPrune true         /* alpha beta pruning */
#define WinLosePrune true    /* stop early on win/lose */
#define ScoreWin 6
#define ScoreTie 5
#define ScoreLose  4
#define ScoreMax 9
#define ScoreMin 2
#define DefaultIterations 10

#define PieceX 1
#define PieceO 2
#define PieceBlank 0

typedef char ttype;

int g_Iterations = DefaultIterations;
long g_IMoves;
ttype g_board[ 9 ];

ttype pos0func()
{
    /* using "int" instead of "ttype" for x is faster on 8086 and Z80 */
    int x;
    x = g_board[0];
    
    if ( ( x == g_board[1] && x == g_board[2] ) ||
         ( x == g_board[3] && x == g_board[6] ) ||
         ( x == g_board[4] && x == g_board[8] ) )
        return x;
    return PieceBlank;
}

ttype pos1func()
{
    int x;
    x = g_board[1];
    
    if ( ( x == g_board[0] && x == g_board[2] ) ||
         ( x == g_board[4] && x == g_board[7] ) )
        return x;
    return PieceBlank;
} 

ttype pos2func()
{
    int x;
    x = g_board[2];
    
    if ( ( x == g_board[0] && x == g_board[1] ) ||
         ( x == g_board[5] && x == g_board[8] ) ||
         ( x == g_board[4] && x == g_board[6] ) )
        return x;
    return PieceBlank;
} 

ttype pos3func()
{
    int x;
    x = g_board[3];
    
    if ( ( x == g_board[4] && x == g_board[5] ) ||
         ( x == g_board[0] && x == g_board[6] ) )
        return x;
    return PieceBlank;
} 

ttype pos4func()
{
    int x;
    x = g_board[4];
    
    if ( ( x == g_board[0] && x == g_board[8] ) ||
         ( x == g_board[2] && x == g_board[6] ) ||
         ( x == g_board[1] && x == g_board[7] ) ||
         ( x == g_board[3] && x == g_board[5] ) )
        return x;
    return PieceBlank;
} 

ttype pos5func()
{
    int x;
    x = g_board[5];
    
    if ( ( x == g_board[3] && x == g_board[4] ) ||
         ( x == g_board[2] && x == g_board[8] ) )
        return x;
    return PieceBlank;
} 

ttype pos6func()
{
    int x;
    x = g_board[6];
    
    if ( ( x == g_board[7] && x == g_board[8] ) ||
         ( x == g_board[0] && x == g_board[3] ) ||
         ( x == g_board[4] && x == g_board[2] ) )
        return x;
    return PieceBlank;
} 

ttype pos7func()
{
    int x;
    x = g_board[7];
    
    if ( ( x == g_board[6] && x == g_board[8] ) ||
         ( x == g_board[1] && x == g_board[4] ) )
        return x;
    return PieceBlank;
} 

ttype pos8func()
{
    int x;
    x = g_board[8];
    
    if ( ( x == g_board[6] && x == g_board[7] ) ||
         ( x == g_board[2] && x == g_board[5] ) ||
         ( x == g_board[0] && x == g_board[4] ) )
        return x;
    return PieceBlank;
} 

typedef ttype pfunc_t();

pfunc_t * win_functions[9] =
{
    pos0func,
    pos1func,
    pos2func,
    pos3func,
    pos4func,
    pos5func,
    pos6func,
    pos7func,
    pos8func,
};

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

    g_IMoves++;

    if ( depth >= 4 )
    {
        p = ( * win_functions[ move ] )(); 
/*
        p = winner2( move );
*/
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

long g_Moves = 0;

int FindSolution( position ) ttype position;
{
    int i;

    for ( i = 0; i < 9; i++ )
        g_board[ i ] = PieceBlank;

    g_board[ position ] = PieceX;

    for ( i = 0; i < g_Iterations; i++ )
    {
        g_IMoves = 0;
        MinMax( ScoreMin, ScoreMax, 0, position );
        g_Moves += g_IMoves;  /* do the 4-byte long addition once per loop to save work */
    }

    return 0;
} /*FindSolution*/

int main( argc, argv ) int argc; char * argv[];
{
    g_IMoves = 0;

    if ( 2 == argc )
        sscanf( argv[ 1 ], "%d", &g_Iterations ); /* no atoi in MS C 1.0 */

    FindSolution( 0 );
    FindSolution( 1 );
    FindSolution( 4 );

    printf( "move count:      %ld\n", g_Moves ); /* 6493 * g_Iterations */
    printf( "iteration count: %d\n", g_Iterations );
    return 0;
} /*main*/

