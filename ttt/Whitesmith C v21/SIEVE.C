/*  sieve.c */

/* Eratosthenes Sieve Prime Number Program in C from Byte Jan 1983
   to compare the speed. */

#define TRUE 1
#define FALSE 0
#define SIZE 8190

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

int main()
        {
        int i, k, l;
        int prime,count,iter;
        char account[ 10 ];

        char flags[SIZE+1];

        for (iter = 1; iter <= 10; iter++) {    /* do program 10 times */
                count = 0;                      /* initialize prime counter */
                for (i = 0; i <= SIZE; i++)     /* set all flags true */
                        flags[i] = TRUE;
                for (i = 0; i <= SIZE; i++) {
                        if (flags[i]) {         /* found a prime */
                                prime = i + i + 3;      /* twice index + 3 */
                                for (k = i + prime; k <= SIZE; k += prime)
                                        flags[k] = FALSE;       /* kill all multiples */
                                count++;                /* primes found */
                                }
                        }
                }
        l = itoa( account, count );
        write( 0, account, l );
        write( 0, " primes.\n", 9); 
        return 0;
        }
