// NT Virtual CP/M Machine
// This app runs CP/M 2.2 .com programs on Windows.
// Written by David Lee
// Notes:   -- Only the subset of CP/M 2.2 required to run asm.com, load.com, and Turbo Pascal 1.00 & 3.01A is implemented.
//          -- Also tested with Wordstar Release 4 and mbasic.com BASIC-80 Ref 5.21.
//          -- Use tinst.com to configure Turbo Pascal 1.00 for vT100 and 3.01A for ANSI to get the screen to work.
//          -- pip.com runs for simple file copies. Not tested for other modes, which probably fail.
//          -- Can be run in 8080 or Z80 modes (the latter is required for Turbo Pascal).
//          -- Detects if an ESC character is output, and switches to 80,24 mode.
//          -- Uses x80.?xx for 8080 and Z80 emulation
//          -- Not tested for other CP/M apps, which means they probably don't work. If they fail, it's probably
//             not the CPU emulation (that's pretty well tested). It's probably the CP/M emulation in this file.
//          
#define UNICODE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <conio.h>
#include <direct.h>
#include <vector>
#include <cstring>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <djltrace.hxx>
#include <djl_con.hxx>
#include <djl_cycle.hxx>

#include "x80.hxx"

#define OPCODE_NOP       0x00
#define OPCODE_RET       0xC9
#define OPCODE_HALT      0x76

// CP/M constants for memory addresses where OS-global state is stored
#define FCB_ARG1_OFFSET    0x5c
#define FCB_ARG2_OFFSET    0x6c
#define ARGUMENTS_OFFSET   0x80
#define DEFAULT_DMA_OFFSET 0x80 // read arguments before doing I/O because it's the same address

#define CPM_FILENAME_LEN ( 8 + 3 + 1 + 1 ) // name + type + dot + null

template <class T> T get_max( T a, T b )
{
    if ( a > b )
        return a;
    return b;
}

template <class T> T get_min( T a, T b )
{
    if ( a < b )
        return a;
    return b;
}
    
// this is the first 16 bytes of an FCB used for the first two command-line arguments at 0x5c and 0x6c

struct FCB_ARGUMENT
{
    uint8_t dr;            // 0 = default drive, 1 = A ... 16 = P
    uint8_t f[8];          // file name. uppercase ascii or spaces
    uint8_t t[3];          // file type. uppercase ascii or spaces
    uint8_t ex;            // extent 0..31 during I/O
    uint8_t s1;            // reserved
    uint8_t s2;            // reserved
    uint8_t rc;            // record count for extent ex. 0..127
};

struct FCB
{
    uint8_t dr;            // 0 = default drive, 1 = A ... 16 = P
    uint8_t f[8];          // file name. uppercase ascii or spaces
    uint8_t t[3];          // file type. uppercase ascii or spaces
    uint8_t ex;            // extent 0..31 during I/O
    uint8_t s1;            // reserved for CP/M
    uint8_t s2;            // reserved for CP/M
    uint8_t rc;            // record count for extent ex. 0..127
    uint8_t dImage[16];    // second half of directory entry OR rename new name
    uint8_t cr;            // current record to read or write in sequential file operations
    uint8_t r0;            // low byte of random I/O offset.  Also for Compute File Size.
    uint8_t r1;            // high byte of random I/O offset. Also for Compute File Size.
    uint8_t r2;            // overflow of r0 and r1 (unused in CP/M 2.2)

    // r0 and r1 are a 16-bit count of 128 byte records

    uint16_t GetRandomIOOffset() { return ( (uint16_t) this->r1 << 8 ) | this->r0; }
    void SetRandomIOOffset( uint16_t o ) { this->r0 = 0xff & o; this->r1 = ( o & 0xff00 ) >> 8; this->r2 = 0; }
};

// this struct is used to cache FILE * objects to both avoid open/close each time and to preserve
// sequential I/O offsets from one call to the next.

struct FileEntry
{
    char acName[ CPM_FILENAME_LEN ];
    FILE * fp;
};

CDJLTrace tracer;

static bool g_haltExecuted;
static uint8_t * g_DMA = memory + DEFAULT_DMA_OFFSET;
static vector<FileEntry> g_fileEntries;
static HANDLE g_hFindFirst = INVALID_HANDLE_VALUE;
static bool g_forceConsole = false;
ConsoleConfiguration g_consoleConfig;

#ifdef _GNU_CPP
#if false
    struct termios orig_termios;

    void reset_terminal_mode()
    {
        tcsetattr( 0, TCSANOW, &orig_termios );
    }

    void set_conio_terminal_mode()
    {
        struct termios new_termios;

        tcgetattr( 0, &orig_termios );
        memcpy( &new_termios, &orig_termios, sizeof( new_termios ) );

        atexit( reset_terminal_mode );
        cfmakeraw( &new_termios );
        tcsetattr( 0, TCSANOW, &new_termios );
    } //set_conio_terminal_mode

    int _kbhit()
    {
        struct timeval tv = { 0L, 0L };
        fd_set fds;
        FD_ZERO( &fds );
        FD_SET( 0, &fds );
        return ( select( 1, &fds, NULL, NULL, &tv ) > 0 );
    }

    int _getch()
    {
        int r;
        unsigned char c;

        if ( ( r = read( 0, &c, sizeof( c ) ) ) < 0 )
            return r;

        return c;
    } // _getch
#endif

    void output_char( char ch )
    {
        if ( 0xa == ch )
            printf( "%c", 0xd );
        
        printf( "%c", ch );
        fflush( stdout );
    } //output_char

    char * portable_gets_s( char * buf, size_t bufsize )
    {
        // getline (hangs, no echo)
        // fscanf (works, no echo )
        // fgets (hangs, no echo)

        size_t len = 0;
        do
        {
            char ch = _getch();
            if ( '\n' == ch || '\r' == ch )
            {
                output_char ( 0xa );
                break;
            }

            if ( len >= ( bufsize - 1 ) )                
                break;

            if ( 0x7f == ch || 8 == ch ) // backspace (it's not 8 for some reason)
            {
                if ( len > 0 )
                {
                    output_char( 8 );
                    output_char( ' ' );
                    output_char( 8 );
                    len--;
                }
            }
            else
            {
                output_char( ch );
                buf[ len++ ] = ch;
            }
        } while( true );

        buf[ len ] = 0;
        return buf;
    } //gets_s
#else
    char * portable_gets_s( char * buf, size_t bufsize )
    {
        return gets_s( buf, bufsize );
    }
#endif


void trace_FCB( FCB * p )
{
    tracer.Trace( "FCB:\n" );
    tracer.Trace( "  drive:    %#x == %c\n", p->dr, ( 0 == p->dr ) ? 'A' : 'A' + p->dr - 1 );
    tracer.Trace( "  filename: '%c%c%c%c%c%c%c%c'\n", 0x7f & p->f[0], 0x7f & p->f[1], 0x7f & p->f[2], 0x7f & p->f[3],
                                                      0x7f & p->f[4], 0x7f & p->f[5], 0x7f & p->f[6], 0x7f & p->f[7] );
    tracer.Trace( "  filetype: '%c%c%c'\n", 0x7f & p->t[0], 0x7f & p->t[1], 0x7f & p->t[2] );
    tracer.Trace( "  R S A:    %d %d %d\n", 0 != ( 0x80 & p->t[0] ), 0 != ( 0x80 & p->t[1] ), 0 != ( 0x80 & p->t[2] ) );
    tracer.Trace( "  extent:   %d\n", p->ex );
    tracer.Trace( "  cr:       %u\n", p->cr );
    tracer.Trace( "  r0:       %u\n", p->r0 );
    tracer.Trace( "  r1:       %u\n", p->r1 );
    tracer.Trace( "  r2:       %u\n", p->r2 );
} //trace_FCB

bool parse_FCB_Filename( FCB * pfcb, char * pcFilename )
{
    char * orig = pcFilename;

    // note: the high bits are used for file attributes. Mask them away

    for ( int i = 0; i < 8; i++ )
    {
        if ( ' ' == ( 0x7f & pfcb->f[ i ] ) )
            break;
        *pcFilename++ = ( 0x7f & pfcb->f[ i ] );
    }

    if ( ' ' != pfcb->t[0] )
    {
        *pcFilename++ = '.';

        for ( int i = 0; i < 3; i++ )
        {
            if ( ' ' == ( 0x7f & pfcb->t[ i ] ) )
                break;
            *pcFilename++ = ( 0x7f & pfcb->t[ i ] );
        }
    }

    *pcFilename = 0;
    return ( pcFilename != orig );
} //parse_FCB_Filename

const char * low_address_names[] =
{
    "warm boot",
    "warm boot (bios) low",
    "warm boot (bios) high",
    "unknown",
    "unknown",
    "bdos call",
    "bdos low",
    "bdos high",
};

void x80_invoke_out( uint8_t x)
{
} //x80_invoke_out

void x80_invoke_in( uint8_t x)
{
} //x80_invoke_in

void x80_invoke_halt()
{
    tracer.Trace( "cpu_halt\n" );
    g_haltExecuted = true;
} //x80_invoke_halt

char printable_ch( char ch )
{
    if ( ch < ' ' || 127 == ch )
        return '.';

    return ch;
} //printable_ch

char * append_hex_nibble( char * p, uint8_t val )
{
    assert( val <= 15 );
    *p++ = ( val <= 9 ) ? val + '0' : val - 10 + 'a';
    return p;
} //append_hex_nibble

char * append_hex_byte( char * p, uint8_t val )
{
    p = append_hex_nibble( p, ( val >> 4 ) & 0xf );
    p = append_hex_nibble( p, val & 0xf );
    return p;
} //append_hex_byte

char * append_hex_word( char * p, uint16_t val )
{
    p = append_hex_byte( p, ( val >> 8 ) & 0xff );
    p = append_hex_byte( p, val & 0xff );
    return p;
} //append_hex_word

void DumpBinaryData( uint8_t * pData, uint32_t length, uint32_t indent )
{
    int64_t offset = 0;
    int64_t beyond = length;
    const int64_t bytesPerRow = 32;
    uint8_t buf[ bytesPerRow ];
    char acLine[ 200 ];

    while ( offset < beyond )
    {
        char * pline = acLine;

        for ( uint32_t i = 0; i < indent; i++ )
            *pline++ = ' ';

        pline = append_hex_word( pline, (uint16_t) offset );
        *pline++ = ' ';
        *pline++ = ' ';

        int64_t cap = get_min( offset + bytesPerRow, beyond );
        int64_t toread = ( ( offset + bytesPerRow ) > beyond ) ? ( length % bytesPerRow ) : bytesPerRow;

        memcpy( buf, pData + offset, toread );

        for ( int64_t o = offset; o < cap; o++ )
        {
            pline = append_hex_byte( pline, buf[ o - offset ] );
            *pline++ = ' ';
        }

        uint64_t spaceNeeded = ( bytesPerRow - ( cap - offset ) ) * 3;

        for ( uint64_t sp = 0; sp < ( 1 + spaceNeeded ); sp++ )
            *pline++ = ' ';

        for ( int64_t o = offset; o < cap; o++ )
        {
            char ch = buf[ o - offset ];

            if ( ch < ' ' || 127 == ch )
                ch = '.';

            *pline++ = ch;
        }

        offset += bytesPerRow;
        *pline = 0;

        tracer.TraceQuiet( "%s\n", acLine );
    }
} //DumpBinaryData

FILE * RemoveFileEntry( char * name )
{
    for ( size_t i = 0; i < g_fileEntries.size(); i++ )
    {
        if ( !strcmp( name, g_fileEntries[ i ].acName ) )
        {
            FILE * fp = g_fileEntries[ i ].fp;
            tracer.Trace( "removing file entry '%s'\n", name );
            g_fileEntries.erase( g_fileEntries.begin() + i );
            return fp;
        }
    }

    tracer.Trace( "ERROR: could not remove file entry for '%s'\n", name );
    return 0;
} //RemoveFileEntry

FILE * FindFileEntry( char * name )
{
    for ( size_t i = 0; i < g_fileEntries.size(); i++ )
    {
        if ( !strcmp( name, g_fileEntries[ i ].acName ) )
        {
            //tracer.Trace( "found file entry '%s': %p\n", name, g_fileEntries[ i ].fp );
            tracer.Trace( "found file entry '%s'\n", name );
            return g_fileEntries[ i ].fp;
        }
    }

    tracer.Trace( "could not find file entry for '%s'; that might be OK\n", name );
    return 0;
} //FindFileEntry

const char * bdos_functions[] =
{
    "system reset",
    "console input",
    "console output",
    "reader input",
    "punch output",
    "list output",  
    "direct console i/o",
    "get i/o byte",
    "set i/o byte",
    "print string",
    "read console buffer",
    "get console status",
    "return version number",
    "reset disk system",
    "select disk",
    "open file",
    "close file",
    "search for first",
    "search for next",
    "delete file",
    "read sequential",
    "write sequential",
    "make file",
    "rename file",
    "return login vector",
    "return current disk",
    "set dma address",
    "get addr(alloc)",
    "write protect disk",
    "get read-only vector",
    "set file attributes",
    "get addr(disk parms)",
    "get/set user code",
    "read random",
    "write random",
    "compute file size",
    "set random record",
    "reset drive",
    "write random with zero fill",
};

const char * get_bdos_function( uint16_t address )
{
    uint16_t id = address & 0xff;

    // the ids are shifted by 3 since I put code start at 0, not -3

    if ( 0 == id || 0x80 == id )
        return "cold sart";
    if ( 3 == id || 0x81 == id )
        return "warm boot (reload command processor)";
    if ( 6 == id || 0x82 == id )
        return "console status";
    if ( 9 == id || 0x83 == id )
        return "console input";
    if ( 12 == id || 0x84 == id )
        return "console output";
    if ( 15 == id )
        return "list: printer output";
    if ( 18 == id )
        return "punch: paper tape punch output";
    if ( 21 == id )
        return "reader: paper tape reader input";
    if ( 24 == id )
        return "home: move dic head to track 0";
    if ( 27 == id )
        return "seldsk: select disc drive";
    if ( 30 == id )
        return "settrk: set track number";
    if ( 33 == id )
        return "setsec: set sector number";
    if ( 36 == id )
        return "setdma: set dma id";
    if ( 39 == id )
        return "read: read a sector";
    if ( 42 == id )
        return "write: write a sector";
    if ( 45 == id )
        return "listst: stat of list device";
    if ( 48 == id )
        return "sectran: sector translation for skewing";

    return "unknown";
} //get_bdos_function

// _kbhit() does device I/O in Windows, which sleeps waiting for a reply, so compute-bound
// mbasic.com apps run 10x slower than they should because mbasic polls for keyboard input.
// Workaround: only call _kbhit() if 50 milliseconds has gone by since the last call.

bool throttled_kbhit()
{
    static ULONGLONG last_call = 0;

    ULONGLONG this_call = GetTickCount64();

    if ( ( this_call - last_call ) > 50 )
    {
        last_call = this_call;
        return _kbhit();
    }

    return false;
} //throttled_kbhit

uint8_t x80_invoke_hook()
{
    uint16_t address = reg.pc - 1; // the emulator has moved past this instruction already

    if ( address >= 0xff00 )
    {
        tracer.Trace( "bios function %#x: %s\n", address, get_bdos_function( address ) );
        //x80_trace_state();

        // BIOS call. 0xff00-0xff33 are for actual, documented BIOS calls.
        // 0xff80 and above are when apps like mbasic.com read the jump table, assume it's a series of
        // jmp <16-bit-address> sequences, and call the <16-bit-address> directly.

        if ( 0xff00 == address || 0xff03 == address || 0xff80 == address || 0xff81 == address )
        {
            // boot, which means exit the app

            return OPCODE_HALT;
        }
        else if ( 0xff06 == address || 0xff82 == address )
        {
            // const console status. A=0 if nothing available, A=0xff if a keystroke is available

            if ( throttled_kbhit() )
                reg.a = 0xff;
            else
                reg.a = 0;
        }
        else if ( 0xff09 == address || 0xff83 == address )
        {
            // conin

            reg.a = _getch();
            tracer.Trace( "conin is returning %02xh\n", reg.a );
        }
        else if ( 0xff0c == address || 0xff84 )
        {
            // conout
            // if the output character is ESC, assume the app wants 80x24. 

            if ( 0x1b == reg.c && !g_forceConsole && !g_consoleConfig.IsEstablished() )
                g_consoleConfig.EstablishConsole( 80, 24 );

            char ch = reg.c;
            tracer.Trace( "bios console out: %02x == '%c'\n", ch, printable_ch( ch ) );
            printf( "%c", reg.c );
        }
        else
        {
            tracer.Trace( "UNIMPLEMENTED BIOS CODE!!!!!!!!!!!!!!!: %#x\n", address );
            printf( "unhandled bios code!!!!!!!!!!!!!!! %#x\n", address );
        }

        return OPCODE_RET;
    }

    if ( 5 != address )
    {
        tracer.Trace( "hook call, but not bdos since address isn't 5: %04x\n", address );
        return OPCODE_NOP;
    }

    uint8_t function = reg.c;
    tracer.Trace( "bdos function %d: %s\n", function, function <= 40 ? bdos_functions[ function ] : "unknown" );
    //x80_trace_state();

    switch( reg.c )
    {
        case 0:
        {
            // system reset. end execution of the app.

            return OPCODE_HALT;
        }
        case 1:
        {
            // console input

            reg.a = _getch();

            break;
        }
        case 2:
        {
            // console output

            uint8_t ch = reg.e;
            if ( 0x0d != ch )             // skip carriage return because line feed turns into cr+lf
            {
                //tracer.Trace( "bdos console out: %02x == '%c'\n", ch, printable_ch( ch ) );

                printf( "%c", ch );
            }

            break;
        }
        case 4:
        {
            // punch output

            break;
        }
        case 5:
        {
            // list output

            break;
        }
        case 6:
        {
            // direct console I/O

            reg.a = 0;

            uint8_t e = reg.e;
            if ( 0xff == e )
            {
                if ( throttled_kbhit() )
                    reg.a = _getch();
            }

            break;
        }
        case 9:
        {
            uint16_t i = reg.D();
            uint32_t count = 0;

            //DumpBinaryData( memory + i, 0x20, 0 );
    
            while ( '$' != memory[i] )
            {
                if ( count++ > 2000 ) // arbitrary limit, but probably a bug if this long
                {
                    tracer.Trace( "ERROR: String to print is too long!\n", stderr );
                    break;
                }

                uint8_t ch = memory[ i++ ];
                if ( 0x0d != ch )              // skip carriage return because line feed turns into cr+lf
                    printf( "%c", ch );
            }

            break;
        }
        case 10:
        {
            // read console buffer. DE: buffer address. buffer:
            //   0      1       2       3
            //   in_len out_len char1   char2 ...

            // note: gets_s outputs both a cr and lf to the console. CP/M just outputs a cr.
            // short of implementing gets_s myself I don't know of a workaround.

            uint16_t offset = reg.D();
            char * pbuf = (char *) memory + offset;
            pbuf[ 1 ] = 0;
            uint16_t in_len = *pbuf;

            if ( in_len > 0 )
            {
                pbuf[ 2 ] = 0;
                portable_gets_s( pbuf + 2, in_len );
                uint8_t out_len = (uint8_t) strlen( pbuf + 2 );
                pbuf[ 1 ] = out_len;

                tracer.Trace( "  read console len %u, string '%s'\n", out_len, pbuf + 2 );
            }
            else
                tracer.Trace( "WARNING: read console buffer asked for input but provided a 0-length buffer\n" );

            break;
        }
        case 12:
        {
            // return version number

            reg.h = 0;      // CP/M
            reg.l = 0x22;   // version 2.2

            break;
        }
        case 13:
        {
            // reset disks. returns 0xff if a file exists that starts with a $ or 0 otherwise

            reg.a = 0;

            break;
        }
        case 14:
        {
            // select disk. Return 0 in A if OK, or 0xff otherwise.

            uint8_t disk = reg.e;
            tracer.Trace( "  selected disk %d == %c\n", disk, ( disk < 16 ) ? disk + 'A' : '?' );

            if ( 0 == disk )
                reg.a = 0;
            else
                reg.a = 0xff;

            break;
        }
        case 15:
        {
            // open file. return 255 if file not found and 0..3 directory code otherwise
    
            tracer.Trace( "open file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  opening file '%s' for pfcb %p\n", acFilename, pfcb );
    
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    // sometimes apps like the CP/M assembler ASM open files that are already open.
                    // rewind it to position 0.

                    fseek( fp, 0, SEEK_SET );
                    reg.a = 0;
                }
                else
                {
                    fp = fopen( acFilename, "r+b" );
                    if ( fp )
                    {
                        FileEntry fe;
                        strcpy( fe.acName, acFilename );
                        fe.fp = fp;
                        g_fileEntries.push_back( fe );
    
                        reg.a = 0;
                    }
                    else
                        tracer.Trace( "ERROR: can't open file '%s' error %d = %s\n", acFilename, errno, strerror( errno ) );
                }
            }
            else
                tracer.Trace( "ERROR: can't parse filename in FCB\n" );

            break;
        }
        case 16:
        {
            // close file. return 255 on error and 0..3 directory code otherwise
    
            tracer.Trace( "close file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = RemoveFileEntry( acFilename );
                if ( fp )
                {
                    int ret = fclose( fp );
    
                    if ( 0 == ret )
                        reg.a = 0;
                    else
                        tracer.Trace( "ERROR: file close failed, error %d = %s\n", errno, strerror( errno ) );
                }
                else
                    tracer.Trace( "ERROR: file close on file that's not open\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse filename in close call\n" );

            break;
        }
        case 17:
        {
            // search for first. Use the FCB in de and write directory entries to the DMA address, then point to
            // which of those entries is the actual one (0-3) or 0xff for not found in A.

            tracer.Trace( "search for first\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  searchinf for first match of '%s'\n", acFilename );

                if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                {
                    FindClose( g_hFindFirst );
                    g_hFindFirst = INVALID_HANDLE_VALUE;
                }

                WIN32_FIND_DATAA fd = {0};
                g_hFindFirst = FindFirstFileA( acFilename, &fd );
                if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                {
                    memset( g_DMA, 0, 128 );
                    for ( int i = 1; i < 12; i++ )
                        g_DMA[ i ] = ' ';

                    int len = strlen( fd.cFileName );
                    strupr( fd.cFileName );

                    for ( int i = 0; i < len; i++ )
                    {
                        if ( '.' == fd.cFileName[ i ] )
                        {
                            for ( int e = 0; e < 3; e++ )
                            {
                                if ( 0 == fd.cFileName[ i + 1 + e ] )
                                    break;

                                g_DMA[ 8 + e + 1 ] = fd.cFileName[ i + 1 + e ];
                            }

                            break;
                        }

                        g_DMA[ i + 1 ] = fd.cFileName[ i ];
                    }

                    tracer.Trace( "  search for first found '%c%c%c%c%c%c%c%c%c%c%c'\n",
                                  g_DMA[1], g_DMA[2], g_DMA[3], g_DMA[4], g_DMA[5], g_DMA[6], g_DMA[7], g_DMA[8],
                                  g_DMA[9], g_DMA[10],  g_DMA[11] );

                    reg.a = 0;
                }
                else
                    tracer.Trace( "WARNING: find first file failed, error %d\n", GetLastError() );
            }
            else
                tracer.Trace( "ERROR: can't parse filename for search for first\n" );

            break;
        }
        case 18:
        {
            // search for next. Use the FCB in de and write directory entries to the DMA address, then point to
            // which of those entries is the actual one (0-3) or 0xff for not found in A.

            tracer.Trace( "search for next\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  searchinf for next match of '%s'\n", acFilename );

                if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                {
                    WIN32_FIND_DATAA fd = {0};
                    BOOL found = FindNextFileA( g_hFindFirst, &fd );
                    if ( found )
                    {
                        memset( g_DMA, 0, 128 );
                        for ( int i = 1; i < 12; i++ )
                            g_DMA[ i ] = ' ';
    
                        int len = strlen( fd.cFileName );
                        strupr( fd.cFileName );
    
                        for ( int i = 0; i < len; i++ )
                        {
                            if ( '.' == fd.cFileName[ i ] )
                            {
                                for ( int e = 0; e < 3; e++ )
                                {
                                    if ( 0 == fd.cFileName[ i + 1 + e ] )
                                        break;
    
                                    g_DMA[ 8 + e + 1 ] = fd.cFileName[ i + 1 + e ];
                                }
    
                                break;
                            }
    
                            g_DMA[ i + 1  ] = fd.cFileName[ i ];
                        }
    
                        tracer.Trace( "  search for next found '%c%c%c%c%c%c%c%c%c%c%c'\n",
                                      g_DMA[1], g_DMA[2], g_DMA[3], g_DMA[4], g_DMA[5], g_DMA[6], g_DMA[7], g_DMA[8],
                                      g_DMA[9], g_DMA[10],  g_DMA[11] );
    
                        reg.a = 0;
                    }
                    else
                    {
                        tracer.Trace( "WARNING: find next file found no more, error %d\n", GetLastError() );
                        FindClose( g_hFindFirst );
                        g_hFindFirst = INVALID_HANDLE_VALUE;
                    }
                }
                else
                    tracer.Trace( "ERROR: search for next without a prior successful search for first\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse filename for search for first\n" );

            break;
        }
        case 19:
        {
            // delete file. return 255 if file not found and 0..3 directory code otherwise
    
            tracer.Trace( "delete file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  deleting file '%s'\n", acFilename );
                int removeok = ( 0 == remove( acFilename ) );
                if ( removeok )
                    reg.a = 0;
            }
            else
                tracer.Trace( "ERROR: can't parse filename for delete file\n" );

            break;
        }
        case 20:
        {
            // read sequential. return 0 on success or non-0 on failure (end of file)
            // reads 128 bytes from cr of the extent and increments cr.
            // if cr overflows, the extent is incremented and cr is set to 0 for the next read
    
            tracer.Trace( "read sequential file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    uint32_t curr = ftell( fp );
                    tracer.Trace( "reading at offset %u\n", curr );

                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
                    fseek( fp, curr, SEEK_SET );
                    tracer.Trace( "file size: %u, current %u\n", file_size, curr );

                    uint32_t to_read = get_min( file_size - curr, (uint32_t) 128 );
                    memset( g_DMA, 0x1a, 128 ); // fill with ^Z, the EOF marker in CP/M
        
                    size_t numread = fread( g_DMA, to_read, 1, fp );
                    if ( numread > 0 )
                    {
                        DumpBinaryData( g_DMA, 128, 0 );
        
                        reg.a = 0;
                    }
                    else
                    {
                        // likely end of file

                        reg.a = 1;
                    }
                }
                else
                    tracer.Trace( "ERROR: can't read from a file that's not open\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse filename in read sequential file\n" );
    
            break;
        }
        case 21:
        {
            // write sequential. return 0 on success or non-0 on failure (out of disk space)
            // reads 128 bytes from cr of the extent and increments cr.
            // if cr overflows, the extent is incremented and cr is set to 0 for the next read
    
            tracer.Trace( "write sequential file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    uint32_t curr = ftell( fp );
                    tracer.Trace( "writing at offset %#x = %u\n", curr, curr );
        
                    DumpBinaryData( g_DMA, 128, 0 );
                    size_t numwritten = fwrite( g_DMA, 128, 1, fp );
                    if ( numwritten > 0 )
                        reg.a = 0;
                    else
                        tracer.Trace( "ERROR: fwrite returned 0 bytes\n" );
                }
                else
                    tracer.Trace( "ERROR: can't write to a file that's not open\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse filename in write sequential file\n" );

            break;
        }
        case 22:
        {
            // make file. return 255 if out of space or the file exists. 0..3 directory code otherwise.
    
            tracer.Trace( "make file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  making file '%s'\n", acFilename );
                FILE * fp = fopen( acFilename, "w+b" );
                if ( fp )
                {
                    FileEntry fe;
                    strcpy( fe.acName, acFilename );
                    fe.fp = fp;
                    g_fileEntries.push_back( fe );

                    reg.a = 0;
                }
                else
                    tracer.Trace( "ERROR: unable to make file\n" );
            }

            break;
        }
        case 23:
        {
            // rename file. 0 for success, non-zero otherwise.

            tracer.Trace( "rename file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;

            char acOldName[ CPM_FILENAME_LEN ];
            if ( parse_FCB_Filename( pfcb, acOldName ) )
            {
                 // if the file is open, close it or the rename will fail.

                 if ( FindFileEntry( acOldName ) )
                     fclose( RemoveFileEntry( acOldName ) );

                char acNewName[ CPM_FILENAME_LEN ];
                if ( parse_FCB_Filename( (FCB *) ( ( (uint8_t *) pfcb ) + 16 ), acNewName ) )
                {
                    tracer.Trace( "  rename from '%s' to '%s'\n", acOldName, acNewName );

                    if ( !rename( acOldName, acNewName ) )
                        reg.a = 0;
                    else
                        tracer.Trace( "ERROR: can't rename file, errno %d = %s\n", errno, strerror( errno ) );
                }
                else
                    tracer.Trace( "ERROR: can't parse old filename in rename\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse old filename in rename\n" );
            break;
        }
        case 24:
        {
            // return bitmap of logged-in drives

            reg.h = 0;
            reg.l = 1;  // just set the lowest bit to indicate the A drive is available

            break;
        }
        case 25:
        {
            // return current disk. 0..15 corresponding to A..P
    
            reg.a = 0;

            break;
        }
        case 26:
        {
            // set the dma address (128 byte buffer for doing I/O)

            //tracer.Trace( "  updating DMA address; D %u = %#x, old %p, new %p\n", reg.D(), reg.D(), g_DMA, memory + reg.D() );
            tracer.Trace( "  updating DMA address; D %u = %#x\n", reg.D() );

            g_DMA = memory + reg.D();

            break;
        }
        case 27:
        {
            // get addr (alloc) point the allocation vector at 0-filled memory

            reg.h = 0xfe;
            reg.l = 0;

            break;
        }
        case 29:
        {
            // get read-only vector: return bitmap of read-only drives

            reg.h = 0xff;
            reg.l = 0xfe;

            break;
        }
        case 30:
        {
            // set file attributes

            tracer.Trace( "set file attributes \n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                bool readOnly = 0 != ( pfcb->t[0] & 0x80 );
                bool system = 0 != ( pfcb->t[1] & 0x80 );
                bool archive = 0 != ( pfcb->t[2] & 0x80 );
                tracer.Trace( "  setting attributes on '%s' to readonly %d, system %d, archive %d\n", acFilename, readOnly, system, archive );

                tracer.Trace( "  setting attributes is not implemented\n" );

                reg.a = 0;
            }

            break;
        }
        case 32:
        {
            // get/set current user

            reg.a = 0;

            break;
        }
        case 33:
        {
            // read random

            tracer.Trace( "read random\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 6; // seek past end of disk

            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    uint16_t record = pfcb->GetRandomIOOffset();
                    tracer.Trace( "read random record %#x\n", record );
                    uint32_t file_offset = record * 128;
                    memset( g_DMA, 0x1a, 128 ); // fill with ^Z, the EOF marker in CP/M
    
                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
    
                    // OS workaround for app bug: Turbo Pascal expects a read just past the end of file to succeed.
    
                    if ( file_size == file_offset )
                    {
                        reg.a = 0;
                        break;
                    }
    
                    if ( file_size > file_offset )
                    {
                        uint32_t to_read = get_min( file_size - file_offset, (uint32_t) 128 );
                        bool ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                        {
                            tracer.Trace( "reading random at offset %#x\n", file_offset );
                            size_t numread = fread( g_DMA, to_read, 1, fp );
                            if ( numread )
                            {
                                DumpBinaryData( g_DMA, to_read, 0 );
                                reg.a = 0;

                                // The CP/M spec says random read should set the file offset such
                                // that the following sequential I/O will be from the SAME location
                                // as this random read -- not 128 bytes beyond.

                                fseek( fp, file_offset, SEEK_SET );
                            }
                            else
                                tracer.Trace( "ERROR: can't read in read random\n" );
                        }
                        else
                            tracer.Trace( "ERROR: can't seek in read random\n" );
                    }
                    else
                        tracer.Trace( "ERROR: read random read at %u beyond end of file %u\n", file_offset, file_size );
                }
                else
                    tracer.Trace( "ERROR: read random on unopened file\n" );
            }
            else
                tracer.Trace( "ERROR: read random can't parse filename\n" );

            break;
        }
        case 34:
        {
            // write random

            tracer.Trace( "write random\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 6; // seek past end of disk

            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    uint16_t record = pfcb->GetRandomIOOffset();
                    tracer.Trace( "write random record %#x\n", record );
                    uint32_t file_offset = record * 128;
    
                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
    
                    if ( file_size >= file_offset )
                    {
                        bool ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                        {
                            tracer.Trace( "writing random at offset %#x\n", file_offset );
                            DumpBinaryData( g_DMA, 128, 0 );
                            size_t numwritten = fwrite( g_DMA, 128, 1, fp );
                            if ( numwritten )
                            {
                                reg.a = 0;

                                // The CP/M spec says random write should set the file offset such
                                // that the following sequential I/O will be from the SAME location
                                // as this random write -- not 128 bytes beyond.

                                fseek( fp, file_offset, SEEK_SET );
                            }
                            else
                                tracer.Trace( "ERROR: can't write in write random, error %d = %s\n", errno, strerror( errno ) );
                        }
                        else
                            tracer.Trace( "ERROR: can't seek in write random, offset %#x, size %#x\n", file_offset, file_size );
                    }
                    else
                        tracer.Trace( "ERROR: write random beyond end of file\n" );
                }
                else
                    tracer.Trace( "ERROR: write random on unopened file\n" );
            }
            else
                tracer.Trace( "ERROR: write random can't parse filename\n" );

            break;
        }
        case 35:
        {
            // Compute file size. A = 0 if ok, 0xff on failure. Sets r2 to 0 and r0/r1 to the number of 128 byte records

            tracer.Trace( "compute file size\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 0xff;

            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                pfcb->r2 = 0;

                FILE * fp = FindFileEntry( acFilename );
                bool found = false;
                if ( fp )
                    found = true;
                else
                    fp = fopen( acFilename, "r+b" );

                if ( fp )
                {
                    uint32_t curr = ftell( fp );
                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
                    fseek( fp, curr, SEEK_SET );
                    pfcb->SetRandomIOOffset( file_size / 128 );
                    reg.a = 0;

                    tracer.Trace( "  file size is %u == %u records; r1 %#x r0 %#x\n", file_size, file_size / 128, pfcb->r1, pfcb->r0 );

                    if ( !found )
                        fclose( fp );
                }
                else
                    tracer.Trace( "ERROR: compute file size can't find file '%s'\n", acFilename );
            }

            break;
        }
        case 36:
        {
            // Set Random Record. no documented return code. I'm using A=0 for success and A=ff for failure

            tracer.Trace( "set random record\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 0xff;

            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                pfcb->r2 = 0;

                FILE * fp = FindFileEntry( acFilename );

                if ( fp )
                {
                    uint32_t curr = ftell( fp );
                    pfcb->SetRandomIOOffset( curr / 128 );
                    reg.a = 0;

                    tracer.Trace( "  random record current is %u == %u records; r1 %#x r0 %#x\n", curr, curr / 128, pfcb->r1, pfcb->r0 );
                }
                else
                    tracer.Trace( "ERROR: set random record can't find file '%s'\n", acFilename );
            }

            break;
        }
        case 40:
        {
            // write random with zero fill

            tracer.Trace( "write random with zero fill\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 6; // seek past end of disk

            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    uint16_t record = pfcb->GetRandomIOOffset();
                    tracer.Trace( "write random record %#x\n", record );
                    uint32_t file_offset = record * 128;
    
                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
    
                    if ( file_size >= file_offset )
                    {
                        bool ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                        {
                            tracer.Trace( "writing random at offset %#x\n", file_offset );
                            uint8_t buf[ 128 ];
                            memset( buf, 0, sizeof buf );
                            size_t numwritten = fwrite( buf, 128, 1, fp );
                            if ( numwritten )
                                reg.a = 0;
                            else
                                tracer.Trace( "ERROR: can't write in write random with zero fill error %d = %s\n", errno, strerror( errno ) );
                        }
                        else
                            tracer.Trace( "ERROR: can't seek in write random with zero fill, offset %#x, size %#x\n", file_offset, file_size );
                    }
                    else
                        tracer.Trace( "ERROR: write random with zero fill beyond end of file\n" );
                }
                else
                    tracer.Trace( "ERROR: write random with zero fill on unopened file\n" );
            }
            else
                tracer.Trace( "ERROR: write random with zero fill can't parse filename\n" );

            break;
        }
        default:
        {
            tracer.Trace( "UNIMPLEMENTED BDOS FUNCTION!!!!!!!!!!!!!!!: %d = %#x\n", reg.c, reg.c );
            printf( "UNIMPLEMENTED BDOS FUNCTION!!!!!!!!!!!!!!!: %d = %#x\n", reg.c, reg.c );

            x80_trace_state();
            DebugBreak();
            reg.a = 0xff;
        }
    }

    return OPCODE_RET;
} //x80_invoke_hook

void usage( char const * perr = 0 )
{
    if ( perr )
        printf( "error: %s\n", perr );

    printf( "NT Virtual CP/M 2.2 Machine: emulates a CP/M 2.2 i8080/Z80 runtime environment\n" );
    printf( "usage: ntvcm [-c] [-p] [-s:X] [-t] <cp/m 2.2 .com file> [filearg1] [filearg2]\n" );
    printf( "  notes:    filearg1 and filearg2 optionally specify filename arguments for the command\n" );
    printf( "            -c     never auto-detect ESC characters and change to to 80x24 mode\n" );
    printf( "            -C     always switch to 80x24 mode\n" );
    printf( "            -i     trace 8080/Z80 instructions when tracing with -t\n" );
    printf( "            -p     show performance information at app exit\n" ); 
    printf( "            -s:X   speed in Hz. Default is 0, which is as fast as possible.\n" );
    printf( "                   for 4Mhz, use -s:4000000\n" );
    printf( "            -t     enable debug tracing to ntvcm.log\n" );
    printf( "            -8     emulate the i8080, not Z80\n" );
    printf( "            e.g. to assemble, load, and run test.asm:\n" );
    printf( "                 ntvcm asm.com test\n" );
    printf( "                 ntvcm load.com test\n" );
    printf( "                 ntvcm test.com\n" );
    printf( "            e.g. to run Star Trek in mbasic in 80x24 mode using i8080 emulation:\n" );
    printf( "                 ntvcm -8 -C mbasic startrek.bas\n" );
    exit( -1 );
} //usage

void write_arg( FCB_ARGUMENT * arg, char * pc )
{
    if ( ':' == pc[ 1 ] )
    {
        if ( pc[0] > 'P' || pc[0] < 'A' )
            usage( "invalid drive specified in argument" );

        arg->dr = 1 + pc[0] - 'A';
        pc += 2;
    }

    char * dot = strchr( pc, '.' );
    if ( dot )
    {
        memcpy( & ( arg->f ), pc, dot - pc );
        memcpy( & ( arg->t ), dot + 1, 3 );
    }
    else
    {
        memcpy( & ( arg->f ), pc, strlen( pc ) );
    }
} //write_arg

static bool load_file( char const * file_path, int & file_size, uint16_t offset, void * buffer )
{               
    bool ok = false;
    FILE * fp = fopen( file_path, "rb" );

    if ( 0 != fp )
    {
        fseek( fp, 0, SEEK_END );
        file_size = ftell( fp );

        if ( file_size > ( 65536 - 1024 ) )
            usage( "the input file can't be a cp/m com file" );

        fseek( fp, offset, SEEK_SET );
        ok = fread( buffer, file_size, 1, fp ) == 1;

        fclose( fp );
    }

    return ok;
} //load_file

static void setmword( uint16_t offset, uint16_t value ) { * (uint16_t *) & memory[ offset ] = value; }

static void RenderNumber( long long n, char * ac )
{
    if ( n < 0 )
    {
        strcat( ac, "-" );
        RenderNumber( -n, ac );
        return;
    }
   
    if ( n < 1000 )
    {
        sprintf( ac + strlen( ac ), "%lld", n );
        return;
    }

    RenderNumber( n / 1000, ac );
    sprintf( ac + strlen( ac ), ",%03lld", n % 1000 );
    return;
} //RenderNumber

static char * RenderNumberWithCommas( long long n, char * ac )
{
    ac[ 0 ] = 0;
    RenderNumber( n, ac );
    return ac;
} //RenderNumberWithCommas

int main( int argc, char * argv[] )
{
    memset( memory, 0, sizeof( memory ) );
    memset( &reg, 0, sizeof( reg ) );
    reg.fZ80Mode = true;

    char * pcCOM = 0;
    char * pcArg1 = 0;
    char * pcArg2 = 0;
    bool trace = false;
    bool traceInstructions = false;
    uint64_t clockrate = 0;
    bool showPerformance = false;
    bool force80x24 = false;

    for ( int i = 1; i < argc; i++ )
    {
        char *parg = argv[i];
        char c = *parg;

        if ( '-' == c || '/' == c )
        {
            char ca = tolower( parg[1] );

            if ( 's' == ca )
            {
                if ( ':' == parg[2] )
                    clockrate = _strtoui64( parg + 3 , 0, 10 );
                else
                    usage( "colon required after s argument" );
            }
            else if ( '8' == ca )
                reg.fZ80Mode = false;
            else if ( 'i' == ca )
                traceInstructions = true;
            else if ( 't' == ca )
                trace = true;
            else if ( 'p' == ca )
                showPerformance = true;
            else if ( 'c' == parg[1] )
                g_forceConsole = true;
            else if ( 'C' == parg[1] )
                force80x24 = true;
            else
                usage( "invalid argument specified" );
        }
        else
        {
            if ( 0 == pcCOM )
                pcCOM = parg;
            else if ( 0 == pcArg1 )
                pcArg1 = parg;
            else if ( 0 == pcArg2 )
                pcArg2 = parg;
            else
                usage( "too many arguments" );
        }
    }

    tracer.Enable( trace, L"ntvcm.log", true );
    tracer.SetQuiet( true );
    tracer.SetFlushEachTrace( false );
    x80_trace_instructions( traceInstructions );

    if ( 0 == pcCOM )
        usage( "no CP/M command specified" );

    char acCOM[ MAX_PATH ];
    strcpy( acCOM, pcCOM );
    strupr( acCOM );
    DWORD attr = GetFileAttributesA( acCOM );
    if ( INVALID_FILE_ATTRIBUTES == attr )
    {
        if ( strstr( acCOM, ".COM" ) )
            usage( "can't find command file" );
        else
        {
            strcat( acCOM, ".COM" );
            attr = GetFileAttributesA( acCOM );
            if ( INVALID_FILE_ATTRIBUTES == attr )
                usage( "can't find command file" );
        }
    }

    // setup command-line arguments

    FCB_ARGUMENT * arg1 = (FCB_ARGUMENT *) ( memory + FCB_ARG1_OFFSET );
    FCB_ARGUMENT * arg2 = (FCB_ARGUMENT *) ( memory + FCB_ARG2_OFFSET );
    memset( & ( arg1->f ), ' ', 11 );
    memset( & ( arg2->f ), ' ', 11 );

    if ( pcArg1 )
    {
        strupr( pcArg1 );
        if ( pcArg2 )
            strupr( pcArg2 );

        size_t cchars = strlen( pcArg1 );
        if ( pcArg2 )
            cchars += ( 1 + strlen( pcArg2 ) );

        char * args = (char *) memory + ARGUMENTS_OFFSET;

        *args++ = cchars;
        strcpy( args, pcArg1 );
        args += strlen( pcArg1 );

        if ( pcArg2 )
        {
            *args++ = ' ';
            strcpy( args, pcArg2 );
        }

        write_arg( arg1, pcArg1 );
        if ( pcArg2 )
            write_arg( arg2, pcArg2 );
    }

    // make memory look like CP/M 2.2

    memory[0] = OPCODE_HLT;
    memory[1] = 0x03; // low byte of BIOS jump table. boot is at -3 from this address. wboot is here.
    memory[2] = 0xff; // high byte of BIOS jump table

    memory[5] = OPCODE_HOOK;
    memory[6] = 0x00;
    memory[7] = 0xfe;

    // The real bios function table is a list of 3-byte entries containing jmp and the address of
    // each of bios functions 16 functions (17 including the -1 entry to exit).
    // Here, just hook the jmp instruction and put a pointer to the hook opcode in each address.
    // Apps like mbasic.com don't call bios functions; they take the address from the vector and
    // call it directly to save a tiny bit of performance.
    // The memory map is here (giving apps a little more RAM than standard CP/M):
    //   0000-00ff: CP/M global storage
    //   0100-????: App run space growing upward until it collides with the stack
    //   ????-fdff: Stack growing downward until it collides with the app
    //   fe00-ff00: reserved space for bdos; filled with 0s
    //   ff00-ff33: bios jump table of 3*17 bytes. (0xff03 is stored at addess 0x1)
    //   ff80-ff91: where jump table addresses point, filled with OPCODE_HOOK
    //
    // On a typical CP/M machine:
    //   0000-00ff: CP/M global storage
    //   0100-????: app space
    //   ????-e3a9: stack given to apps at start.
    //   e406-????: bdos
    //   f200-????: bios (0xf203 stored at address 0x1)

    memset( memory + 0xff00, OPCODE_HOOK, 0xff );
    for ( uint32_t v = 0; v < 17; v++ )
        setmword( 0xff01 + ( v * 3 ), 0xff80 + v );

    int file_size = 0;
    bool ok = load_file( acCOM, file_size, 0, memory + 0x100 );
    if ( !ok )
    {
        printf( "unable to load command %s\n", acCOM );
        exit( 1 );
    }

    memory[ 0x100 + file_size ] = OPCODE_HLT; // in case the app doesn't shutdown properly
    reg.powerOn();    // set default values of registers
    reg.pc = 0x100;
    reg.sp = 0xfe00;  // the stack is written to below this address
    reg.a = reg.b = reg.c = reg.d = reg.e = reg.h = reg.l = 0;
    reg.fp = reg.ap = 0; // apparently this is expected by CPUTEST
    reg.bp = reg.cp = reg.dp = reg.ep = reg.hp = reg.lp = 0;
    reg.ix = reg.iy = 0;
    g_haltExecuted = false;

    if ( force80x24 )
        g_consoleConfig.EstablishConsole( 80, 24 );

    high_resolution_clock::time_point tStart = high_resolution_clock::now();
    uint64_t total_cycles = 0;
    CPUCycleDelay delay( clockrate );

    do
    {
        total_cycles += x80_emulate( 1000 );

        if ( g_haltExecuted )
            break;

        delay.Delay( total_cycles );
    } while ( true );

    high_resolution_clock::time_point tDone = high_resolution_clock::now();
    g_consoleConfig.RestoreConsole();

    if ( showPerformance )
    {
        char ac[ 100 ];
        long long totalTime = duration_cast<std::chrono::milliseconds>( tDone - tStart ).count();
        printf( "\n" );
        printf( "elapsed milliseconds: %16s\n", RenderNumberWithCommas( totalTime, ac ) );
        printf( "%s cycles:      %20s\n", reg.fZ80Mode ? "Z80 " : "8080", RenderNumberWithCommas( total_cycles, ac ) );
        printf( "clock rate: " );
        if ( 0 == clockrate )
        {
            printf( "      %20s\n", "unbounded" );
            uint64_t total_ms = total_cycles / ( reg.fZ80Mode ? 4000 : 2000 );
            if ( reg.fZ80Mode )
                printf( "approx ms at 4Mhz: %19s == ", RenderNumberWithCommas( total_ms, ac ) );
            else
                printf( "approx ms at 2Mhz: %19s == ", RenderNumberWithCommas( total_ms, ac ) );

            uint16_t days = (uint16_t) ( total_ms / 1000 / 60 / 60 / 24 );
            uint16_t hours = (uint16_t) ( ( total_ms % ( 1000 * 60 * 60 * 24 ) ) / 1000 / 60 / 60 );
            uint16_t minutes = (uint16_t) ( ( total_ms % ( 1000 * 60 * 60 ) ) / 1000 / 60 );
            uint16_t seconds = (uint16_t) ( ( total_ms % ( 1000 * 60 ) ) / 1000 );
            uint64_t milliseconds = ( ( total_ms % 1000 ) );
            printf( "%u days, %u hours, %u minutes, %u seconds, %llu milliseconds\n", days, hours, minutes, seconds, milliseconds );
        }
        else
            printf( "      %20s Hz\n", RenderNumberWithCommas( clockrate, ac ) );
    }

    tracer.Shutdown();
} //main

