#define DIGITS_TO_FIND 200 /*9009;*/

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

int main() {
  int N;
  int x;
  int n;
  int l;
  char ac[ 10 ];
  int a[ DIGITS_TO_FIND ];

  N = DIGITS_TO_FIND;
  x = 0;

  for (n = N - 1; n > 0; --n) {
      a[n] = 1;
  }

  a[1] = 2;
  a[0] = 0;
  while (N > 9) {
      n = N--;
      while (--n) {
          a[n] = x % n;

          x = 10 * a[n-1] + x/n;
      }
      l = itoa( ac, x );
      write( 0, ac, l );
  }

  write( 0, "\ndone\n", 6 );

  return 0;
}
