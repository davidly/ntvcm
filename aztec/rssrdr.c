#include <stdio.h>

#define ITEMS_SHOWN_BEFORE_RELOAD 200  /* 200 * 10 seconds average = ~30 minutes */

char * g_feeds[] =
{
    "https://rss.nytimes.com/services/xml/rss/nyt/World.xml",
    "https://www.theguardian.com/world/rss",
    "https://feeds.washingtonpost.com/rss/world",
    "https://feeds.npr.org/1001/rss.xml",
    0, /* mark the end of the list */
};

char g_rss_item[ 2048 ]; /* set to the bdos DMA address for storing an RSS item */
int g_sleep = 1;

void bdos_sleep( ms ) unsigned int ms;
{
    bdos( 106, ms );
} /*bdos_sleep*/

int bdos_feed_load_rss( pfeeds ) char * pfeeds[];
{
    return bdoshl( 107, pfeeds );
} /*bdos_feed_load_rss*/

int bdos_item_load_rss( item ) int item;
{
    bdos( 26, g_rss_item ); /* set dma address */
    return bdos( 108, item );
} /*bdos_item_load_rss*/

int bdos_kbhit()
{
    return bdos( 6, 0xff );
} /*bdos_kbhit*/

char * strchr( p, c ) char * p; char c;
{
    while ( *p )
    {
        if ( *p == c )
            return p;
        p++;
    }

    return 0;
} /*strchr*/

void slow_print( p ) char * p;
{
    int cols = 0;
    char * pnext, x;

    while ( *p )
    {
        if ( bdos_kbhit() )
            exit( 1 );

        x = *p;
        if ( x > 0 )  /* skip extended ascii characters */
        {
            if ( ' ' == x )
            {
                pnext = strchr( p + 1, ' ' );
                if ( ( pnext && ( cols + ( pnext - p ) > 80 ) ) ||
                     ( !pnext && ( cols + strlen( p  ) > 80 ) ) )
                {
                    cols = 0;
                    putchar( '\n' );
                    p++;
                    continue;
                }
            }
            cols++;
            putchar( x );
            p++;
            if ( g_sleep )
                bdos_sleep( 33 );
        }
    }

    putchar( '\n' );
} /*slow_print*/

int main( argc, argv ) int argc; char * argv[];
{
    int count, ok, i, j, shown;
    int feed_len, title_len;
    char * ptitle, * pdescription;

    if ( argc > 1 )
        g_sleep = 0;

    shown = ITEMS_SHOWN_BEFORE_RELOAD;

    do
    {
        if ( shown >= ITEMS_SHOWN_BEFORE_RELOAD )
        {
            shown = 0;
            puts( "<loading rss feeds>" );
            count = bdos_feed_load_rss( g_feeds );
        
            if ( 0 == count )
            {
                puts( "no rss results" );
                exit( 1 );
            }
        }

        for ( i = 0; i < count; i++ )
        {
            ok = bdos_item_load_rss( i );
            if ( ok )
            {
                shown++;
                feed_len = strlen( g_rss_item );
                ptitle = g_rss_item + feed_len + 1;
                title_len = strlen( ptitle );
                pdescription = ptitle + title_len + 1;
                slow_print( g_rss_item );
                slow_print( ptitle );
                slow_print( pdescription );
                putchar( '\n' );

                /* sleep for 2 seconds. exit if any key is pressed */

                for ( j = 0; g_sleep && j < 20; j++ )
                {
                    if ( bdos_kbhit() )
                        exit( 1 );
                    bdos_sleep( 100 );
                }
            }
        }
    } while ( 1 );

    return 0;
} /*main*/
