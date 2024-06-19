#include stdio.h

typedef char * __my_char_ptr;
typedef void * __my_void_ptr;

/* 69 works */
#define allocs 66
__my_char_ptr ap[ allocs ];
int logging = 1;

char * memset_x( p, v, c ) char * p; int v; int c;
{
    char * pc;
    char val;
    int i;

    pc = p;
    val = ( v & 0xff );

    if ( 0 == p )
    {
        printf( "request to memset a null pointer\n" );
        exit( 1 );
    }

    if ( logging )
        printf( "  memset p %u, v %d, val %x, c %d\n", p, v, val, c );

    for ( i = 0; i < c; i++ )
        *pc++ = val;
    return p;
}

void chkmem( p, v, c ) char * p; int v; int c;
{
    int i, j;

    if ( logging )
        printf( "chkmem: p %u, val %u, count %u\n", p, v, c );

    if ( 0 == p )
    {
        printf( "request to chkmem a null pointer\n" );
        exit( 1 );
    }

    for ( i = 0; i < c; i++ )
    {
        if ( p[i] != v )
        {
            printf( "memory isn't as expected! p %u, v %d, c %d, *pc %d\n", p, v, c, p[i] );
            printf( "  i: %d, val: %u\n", i, v );
            for ( j = 0; j < c; j++ )
                printf( "    byte at offset %d is %u\n", j, p[ j ] );
            exit( 1 );
        }
    }
}

void * my_calloc( n, x ) int n; int x;
{
    /* hisoftc135's calloc() is broken -- it calls blt to copy a 0-byte at the first byte but the blt memcpys from high to low */
    int s;
    char * p;
    s = n * x;
    p = malloc( s );
    if ( p  )
        memset_x( p, 0, s );
    return p;
}

void main()
{
    int i, cb, calloc_cb, j;
    char * pc;

    logging = 0;

    for ( j = 0; j < 10; j++ )
    {
        if ( logging )
            printf( "in alloc mode\n" );
    
        for ( i = 0; i < allocs; i++ )
        {
            cb = 8 + ( i * 10 );
            if ( logging )
                printf( "  i, cb: %d %d\n", i, cb );

            calloc_cb = cb + 5;
            pc = cast(__my_char_ptr) my_calloc( calloc_cb, 1 );
            chkmem( pc, 0, calloc_cb );
            memset_x( pc, 0xcc, calloc_cb );
    
            ap[ i ] = cast(__my_char_ptr) malloc( cb );
            memset_x( ap[ i ], 0xaa, cb );
    
            chkmem( pc, 0xcc, calloc_cb );
            free( pc );
        }
    
        if ( logging )
            printf( "in free mode, even first\n" );
    
        for ( i = 0; i < allocs; i += 2 )
        {
            cb = 8 + ( i * 10 );
            if ( logging )
                printf( "  i, cb: %d %d\n", i, cb );

            calloc_cb = cb + 3;
            pc = cast(__my_char_ptr) my_calloc( calloc_cb, 1 );
            chkmem( pc, 0, calloc_cb );
            memset_x( pc, 0xcc, calloc_cb );
    
            chkmem( ap[ i ], 0xaa, cb );
            memset_x( ap[ i ], 0xff, cb );
            free( ap[ i ] );
    
            chkmem( pc, 0xcc, calloc_cb );
            free( pc );
        }
    
        if ( logging )
            printf( "in free mode, now odd\n" );
    
        for ( i = 1; i < allocs; i += 2 )
        {
            cb = 8 + ( i * 10 );
            if ( logging )
                printf( "  i, cb: %d %d\n", i, cb );

            calloc_cb = cb + 7;
            pc = cast(__my_char_ptr) my_calloc( calloc_cb, 1 );
            if ( logging )
                printf( "newly allocated calloc: %u\n", pc );

            chkmem( pc, 0, calloc_cb );
            chkmem( ap[ i ], 0xaa, cb );
            memset_x( ap[ i ], 0xff, cb );
            free( ap[ i ] );
    
            chkmem( pc, 0, calloc_cb );
            free( pc );
        }
    }

    printf( "success\n" );
    return 0;
}
