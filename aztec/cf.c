#define O_RDONLY        0
#define O_WRONLY        1
#define O_RDWR          2
#define O_CREAT         0x0100
#define O_TRUNC         0x0200
#define O_EXCL          0x0400
#define O_APPEND        0x0800

#define MAX_CMD_LEN 128
char g_acbuffer[ MAX_CMD_LEN ];
char * g_pbuf = g_acbuffer;

char * strchr( s, c ) char * s; char c;
{
    while ( *s )
    {
        if ( *s == c )
            return s;
        s++;
    }

    return 0;
}

int main( argc, argv ) int argc; char * argv[];
{
    int ret, i, all_1a;
    int inf = -1, outf = -1;

    if ( 3 == argc )
    {
        if ( strchr( argv[1], '?' ) || strchr( argv[1], '*' ) || strchr( argv[2], '?' ) || strchr( argv[2], '*' ) )
            puts( "wildcard copies are not implemented" );
        else
        {
            inf = open( argv[1], 0 /*O_BINARY*/ );
            if ( -1 == inf )
                puts( "unable to open source file" );
            else
            {
                outf = open( argv[2], /*O_BINARY | */ O_CREAT | O_RDWR, 0 /*S_IREAD | S_IWRITE */ );
                if ( -1 == outf )
                    puts( "unable to open destination file" );
                else
                {
                    while ( 1 )
                    {
                        printf( "reading...\n" );
                        ret = read( inf, g_acbuffer, sizeof g_acbuffer );
                        if ( 0 == ret )
                            break;

                        printf( "writing... %d bytes\n", ret );
                        write( outf, g_acbuffer, ret );
                    }
                }
            }
        }

        if ( -1 != inf )
            close( inf );
        if ( -1 != outf )
            close( outf );
    }
    else
        puts( "copy expects 2 arguments" );
}


