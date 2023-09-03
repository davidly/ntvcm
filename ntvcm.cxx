// NT Virtual CP/M Machine
// This app runs CP/M 2.2 .com programs on Windows, MacOS, and Linux
// Written by David Lee in late 2022
// Notes:   -- Only the subset of CP/M 2.2 required to run asm.com, load.com, and Turbo Pascal 1.00 & 3.01A is implemented.
//          -- Also tested with Wordstar Release 4 and mbasic.com BASIC-80 Ref 5.21.
//          -- Also tested with Aztec C 1.06. compiler, assembler, linker, and generated apps work.
//          -- Also tested with MBasic.
//          -- Use tinst.com to configure Turbo Pascal 1.00 for vT100 and 3.01A for ANSI to get the screen to work.
//          -- pip.com runs for simple file copies. Not tested for other modes, which probably fail.
//          -- Can be run in 8080 or Z80 modes (the latter is required for Turbo Pascal).
//          -- Optionally detects if an ESC character is output, and switches to 80,24 mode.
//          -- Uses x80.?xx for 8080 and Z80 emulation
//          -- Not tested for other CP/M apps, which means they probably don't work. If they fail, it's probably
//             not the CPU emulation (that's pretty well tested). It's probably the CP/M emulation in this file.
// To build on Windows debug and release:
//     cl /nologo /openmp ntvcm.cxx x80.cxx /Oti2 /Ob2 /Qpar /Fa /EHac /Zi /DDEBUG /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF
//     cl /nologo /openmp ntvcm.cxx x80.cxx /Oti2 /Ob2 /Qpar /Fa /EHac /Zi /DNDEBUG /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF
// To build on Linux debug and release:
//     g++ -ggdb -Ofast -fopenmp -fno-builtin -D DEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm
//     g++ -ggdb -Ofast -fopenmp -fno-builtin -D NDEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm
// To build on Windows with mingw64 & g++: (performance is >10% faster than the Microsoft compiler)
//     g++ -Ofast -ggdb -fopenmp -D _MSC_VER ntvcm.cxx x80.cxx -I ../djl -D DEBUG -o ntvcm.exe -static -lwininet
//     g++ -Ofast -ggdb -fopenmp -D _MSC_VER ntvcm.cxx x80.cxx -I ../djl -D NDEBUG -o ntvcm.exe -static -lwininet
// Note: openmp, wininet, ssl, crypto, and djl_rssrdr.hxx are only required for RSS support.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <cstring>

#include <djl_os.hxx>
#include <djltrace.hxx>
#include <djl_con.hxx>
#include <djl_cycle.hxx>

// On non-Windows platforms djl_rssrdr.hxx has a dependency on:
//     httplib.h from https://github.com/yhirose/cpp-httplib
//     openssl headers and libraries

#ifdef NTVCM_RSS_SUPPORT
    #include <djl_rssrdr.hxx>
    CRssFeed g_rssFeed;
#endif //NTVCM_RSS_SUPPORT

#include "x80.hxx"

// CP/M constants for memory addresses where OS-global state is stored

#define FCB_ARG1_OFFSET            0x5c
#define FCB_ARG2_OFFSET            0x6c
#define COMMAND_TAIL_LEN_OFFSET    0x80
#define COMMAND_TAIL_OFFSET        0x81
#define DEFAULT_DMA_OFFSET         0x80 // read arguments before doing I/O because it's the same address
#define DPB_HI_OFFSET              0xfe // where the Disk Parameter Block resides for BDOS 31.
#define DPB_LO_OFFSET              0x00 // lo part of DPB

#define CPM_FILENAME_LEN ( 8 + 3 + 1 + 1 ) // name + type + dot + null
  
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

struct CPMTime // non-standard time structure
{
    uint16_t hour;
    uint16_t minute;
    uint16_t second;
    uint16_t millisecond;
};

#pragma pack( push, 1 )
struct DiskParameterBlock // for BDOS 31. https://www.seasip.info/Cpm/format22.html
{
    uint16_t spt;    // Number of 128-byte records per track
    uint8_t  bsh;    // Block shift. 3 => 1k, 4 => 2k, 5 => 4k....
    uint8_t  blm;    // Block mask. 7 => 1k, 0Fh => 2k, 1Fh => 4k...
    uint8_t  exm;    // Extent mask, see later
    uint16_t dsm;    // (no. of blocks on the disc)-1
    uint16_t drm;    // (no. of directory entries)-1
    uint8_t  al0;    // Directory allocation bitmap, first byte
    uint8_t  al1;    // Directory allocation bitmap, second byte
    uint16_t cks;    // Checksum vector size, 0 for a fixed disc
    uint16_t off;    // Offset, number of reserved tracks
};
#pragma pack(pop)

CDJLTrace tracer;
ConsoleConfiguration g_consoleConfig;

static bool g_haltExecuted;
static uint8_t * g_DMA = memory + DEFAULT_DMA_OFFSET;
static vector<FileEntry> g_fileEntries;
static bool g_forceConsole = false;
static bool g_forceLowercase = false;
static bool g_backspaceToDel = false;
static bool g_kayproToCP437 = false;

enum terminal_escape: uint32_t { termVT100, termVT52, termKayproII };
static terminal_escape g_termEscape = termVT100;

bool ValidCPMFilename( char * pc )
{
    if ( !strcmp( pc, "." ) )
        return false;

    if ( !strcmp( pc, ".." ) )
        return false;

    const char * pcinvalid = "<>,;:=?[]%|()/\\";
    for ( size_t i = 0; i < strlen( pcinvalid ); i++ )
        if ( strchr( pc, pcinvalid[i] ) )
            return false;

    size_t len = strlen( pc );

    if ( len > 12 )
        return false;

    char * pcdot = strchr( pc, '.' );

    if ( !pcdot && ( len > 8 ) )
        return false;

    if ( pcdot && ( ( pcdot - pc ) > 8 ) )
        return false;

    return true;
} //ValidCPMFilename

#ifdef _MSC_VER
    static HANDLE g_hFindFirst = INVALID_HANDLE_VALUE;

    void CloseFindFirst()
    {
        if ( INVALID_HANDLE_VALUE != g_hFindFirst )
        {
            FindClose( g_hFindFirst );
            g_hFindFirst = INVALID_HANDLE_VALUE;
        }
    } //CloseFindFirst
#else
    #include <dirent.h>
    static DIR * g_FindFirst = 0;
    struct LINUX_FIND_DATA
    {
        char cFileName[ MAX_PATH ];
    };

    void CloseFindFirst()
    {
        if ( 0 != g_FindFirst )
        {
            closedir( g_FindFirst );
            g_FindFirst = 0;
        }
    } //CloseFindFirst

    void FindCloseLinux( DIR * pdir )
    {
        closedir( pdir );
    } //FindCloseLinux

    bool FindNextFileLinux( const char * pattern, DIR * pdir, LINUX_FIND_DATA & fd )
    {
        do
        {
            struct dirent * pent = readdir( pdir );
            if ( 0 == pent )
                return false;

            // ignore files CP/M just wouldn't understand

            if ( !ValidCPMFilename( pent->d_name ) )
                continue;

            // if the first character is ?, match any filename per CP/M rules

            if ( '?' != pattern[ 0 ] )
            {
                // If the pattern isn't a question mark, look for a literal match

                if ( strcmp( pattern, pent->d_name ) )
                    continue;
            }

            strcpy( fd.cFileName, pent->d_name );
            return true;
        } while ( true );

        return false;            
    } //FindNextFileLinux

    DIR * FindFirstFileLinux( const char * pattern, LINUX_FIND_DATA & fd )
    {
        DIR * pdir = opendir( "." );
        tracer.Trace( "  opendir returned %p, errno %d\n", pdir, errno );

        if ( 0 == pdir )
            return 0;

        bool found = FindNextFileLinux( pattern, pdir, fd );

        if ( !found )
        {
            closedir( pdir );
            return 0;
        }

        return pdir;
    } //FindFirstFileLinux

#endif

void ParseFoundFile( char * pfile )
{
    tracer.Trace( "  ParseFoundFile '%s'\n", pfile );
    memset( g_DMA, 0, 128 );
    memset( g_DMA + 1, ' ', 11 );

    size_t len = strlen( pfile );
    assert( len <= 12 ); // 8.3 only
    _strupr( pfile );

    for ( size_t i = 0; i < len; i++ )
    {
        if ( '.' == pfile[ i ] )
        {
            for ( size_t e = 0; e < 3; e++ )
            {
                if ( 0 == pfile[ i + 1 + e ] )
                    break;

                g_DMA[ 8 + e + 1 ] = pfile[ i + 1 + e ];
            }

            break;
        }

        g_DMA[ i + 1 ] = pfile[ i ];
    }

    tracer.Trace( "  search for first/next found '%c%c%c%c%c%c%c%c%c%c%c'\n",
                    g_DMA[1], g_DMA[2], g_DMA[3], g_DMA[4], g_DMA[5], g_DMA[6], g_DMA[7], g_DMA[8],
                    g_DMA[9], g_DMA[10],  g_DMA[11] );
} //ParseFoundFile

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

    // CP/M assumes all filenames are upper case. Linux users generally use all lowercase filenames

    if ( g_forceLowercase )
        _strlwr( orig );

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

void x80_hard_exit( const char * pcerror, uint8_t arg1, uint8_t arg2 )
{
    g_consoleConfig.RestoreConsole( false );

    tracer.Trace( pcerror, arg1, arg2 );
    printf( pcerror, arg1, arg2 );
    tracer.Trace( "  %s\n", build_string() );
    printf( "  %s\n", build_string() );

    exit( 1 );
} //x80_hard_exit

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

const char * get_bdos_function( uint8_t id )
{
    if ( id < _countof( bdos_functions ) )
        return bdos_functions[ id ];
    if ( 105 == id )
        return "get time";
    if ( 106 == id )
        return "sleep";
    if ( 107 == id )
        return "initialize rss feed";
    if ( 108 == id )
        return "fetch rss item";
    if ( 109 == id )
        return "rand";
    if ( 110 == id )
        return "enable/disable instruction tracing";

    return "unknown";
} //get_bdos_function

const char * get_bios_function( uint16_t address )
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
} //get_bios_function

char kaypro_to_cp437( uint8_t c )
{
#ifdef _MSC_VER

    // these are mostly box-drawing characters

    if ( 0xb0 == c )
        return (char) 0xcd;
    if ( 0xd0 == c )
        return (char) 0xc9;
    if ( 0xd5 == c )
        return (char) 0xba;
    if ( 0xdf == c )
        return (char) 0xbb;
    if ( 0x85 == c )
        return (char) 0xc8;
    if ( 0x8c == c )
        return (char) 0xcd;
    if ( 0x8a == c )
        return (char) 0xbc;
    if ( 0xbc == c )
        return (char) 0xdf;
    if ( 0xbf == c )
        return (char) 0xdb;

#else // for linux, use ascii-art

    if ( 0xb0 == c )
        return '-';
    if ( 0xd0 == c )
        return '+';
    if ( 0xd5 == c )
        return '|';
    if ( 0xdf == c )
        return '+';
    if ( 0x85 == c )
        return '+';
    if ( 0x8c == c )
        return '-';
    if ( 0x8a == c )
        return '+';
    if ( 0xbc == c )
        return '*';
    if ( 0xbf == c )
        return '*';

#endif

    if ( c >= 0x80 )
        tracer.Trace( "untranslated kaypro high character %u == %02x\n", c, c );

    return c;
 } //kaypro_to_cp437

// https://en.wikipedia.org/wiki/ANSI_escape_code              vt-100 in 1978, it existed at the same time as CP/M
// https://en.wikipedia.org/wiki/VT52                          VT52
// https://mdfs.net/Archive/info-cpm/1985/01/19/053100.htm     Kaypro II / Lear-Siegler ADM-3A

void output_character( uint8_t c )
{
    // for terminal emulation, I only implement translations for actual sequences apps use.
    // if the output character is ESC, assume the app wants 80x24.

    if ( 0x1b == c && !g_forceConsole && !g_consoleConfig.IsOutputEstablished() )
    {
        tracer.Trace( "  establishing 80x24\n" );
        g_consoleConfig.EstablishConsoleOutput( 80, 24 );
    }

    if ( g_kayproToCP437 )
        c = kaypro_to_cp437( c );

    static bool s_escaped = false;      // true if prior char was ESC
    static uint8_t s_row = 0xff;        // not 0xff if prior 3 chars were ESC Y row

    if ( termVT100 == g_termEscape )
        printf( "%c", c );
    else if ( termVT52 == g_termEscape )
    {
        static bool s_escapedY = false;     // true if prior two chars were ESC Y

        //tracer.Trace( "  output_character %02x, escaped %d, escapedY %d, s_row %d\n", c, s_escaped, s_escapedY, s_row );
        // only a subset are translated. CalcStar only uses Y cursor positioning and no other sequences.

        if ( s_escapedY )
        {
            if ( 0xff == s_row )
                s_row = c - 31;
            else
            {
                uint8_t col = c - 31;
                printf( "\x1b[%u;%uH", s_row, col );
                tracer.Trace( "  moved cursor to %d %d\n", s_row, col );
                s_escapedY = false;
                s_row = 0xff;
            }
            return;
        }

        if ( s_escaped )
        {
            tracer.Trace( "  escape command: %c\n", c );
            if ( 'Y' == c )
                s_escapedY = true;
            else if ( 'A' == c )      // cursor up
                printf( "\x1b[1A" );
            else if ( 'B' == c )      // cursor down
                printf( "\x1b[1B" );
            else if ( 'C' == c )      // cursor right
                printf( "\x1b[1C" );
            else if ( 'D' == c )      // cursor left
                printf( "\x1b[1D" );
            else if ( 'H' == c )      // cursor home
                printf( "\x1b[1;1H" );
            else
            {
                printf( "\x1b%c", c ); // send it out untranslated
                tracer.Trace( "  untranslated VT-52 command '%c' = %02x\n", printable_ch( c ), c );
            }

            s_escaped = false;
        }
        else if ( 0x1b == c )
        {
            s_escaped = true;
            s_escapedY = false; // just in case
            s_row = 0xff; // just in case
        }
        else
            printf( "%c", c );
    }
    else if ( termKayproII == g_termEscape )
    {
        static uint8_t s_escapedChar = 0;

        if ( s_escapedChar )
        {
            if ( '=' == s_escapedChar )
            {
                if ( 0xff == s_row )
                    s_row = c - 31;
                else
                {
                    uint8_t col = c - 31;
                    printf( "\x1b[%u;%uH", s_row, col );
                    tracer.Trace( "  moved cursor to %d %d\n", s_row, col );
                    s_escapedChar = 0;
                    s_row = 0xff;
                }
            }
            else if ( 'B' == s_escapedChar )
            {
                if ( '0' == c )
                    printf( "\x1b[7m" );           // reverse video start
                else if ( '1' == c )
                    printf( "\x1b[2m" );           // half intensity start
                else if ( '4' == c )
                    printf( "\x1b[?25h" );         // cursor on
                else
                    tracer.Trace( "unprocessed kaypro B escaped char %c\n", c );
                s_escapedChar = 0;
            }
            else if ( 'C' == s_escapedChar )
            {
                if ( '0' == c )
                    printf( "\x1b[27m" );          // reverse video stop
                else if ( '1' == c )
                    printf( "\x1b[22m" );          // half intensity stop
                else if ( '4' == c )
                    printf( "\x1b[?25l" );         // cursor off
                else
                    tracer.Trace( "unprocessed kaypro C escaped char %c\n", c );
                s_escapedChar = 0;
            }
            else
                tracer.Trace( "unprocessed kaypro escaped char '%c'\n", c );
            return;
        }

        if ( s_escaped )
        {
            tracer.Trace( "  escape command: %c\n", c );
            s_escapedChar = c;
            s_escaped = false;
        }
        else if ( 0x1b == c )
        {
            s_escaped = true;
            s_escapedChar = 0;
            s_row = 0xff; // just in case
        }
        else if ( c <= 30 || 127 == c )
        {
            if ( 8 == c ) // cursor left
                printf( "\x1b[D" );
            else if ( 8 == c ) // cursor right
                printf( "\x1b[C" );
            else if ( 10 == c ) // cursor down
                printf( "\x1b[B" );
            else if ( 11 == c ) // cursor up
                printf( "\x1b[A" );
            else if ( 12 == c ) // cursor right
                printf( "\x1b[C" );
            else if ( 23 == c ) // erase to end of screen
                printf( "\x1b[J" );
            else if ( 24 == c ) // erase to end of line
                printf( "\x1b[K" );
            else if ( 26 == c ) // clear screen, home cursor
            {
                tracer.Trace( "clear screen, home cursor\n" );
                printf( "\x1b[2J" ); // clear the screen
                printf( "\x1b[1G" ); // cursor to top line
                printf( "\x1b[1d" ); // cursor to left side
            }
            else if ( 30 == c ) // home cursor
                printf( "\x1b[H" );
            else if ( 127 == c ) // del: back + space + back
                printf( "\x1b[D \x1b[D" );
            else
                tracer.Trace( "ignored character in kaypro escape range: %02x\n", c );
        }
        else
            printf( "%c", c );
    }
} //output_character

uint8_t map_input( uint8_t input )
{
    uint8_t output = input;

#ifdef _MSC_VER
    if ( 0xe0 == input ) // likely Windows
    {
        uint8_t next = (uint8_t) ConsoleConfiguration::portable_getch();

        // map the arrow keys to ^ XSED used in many apps. map a handful of other characters

        if ( 'K' == next ) // left arrow
            output = 1 + 'S' - 'A';
        else if ( 'P' == next )              // down arrow
            output = 1 + 'X' - 'A';
        else if ( 'M' == next )              // right arrow
            output = 1 + 'D' - 'A';
        else if ( 'H' == next )              // up arrow
            output = 1 + 'E' - 'A';
        else if ( 'S' == next )              // del
            output = 0x7f;
        else
            tracer.Trace( "no mapping for e0 second character %02x\n", next );

        tracer.Trace( "    next character after 0xe0: %02x == '%c' mapped to %02x\n", next, printable_ch( next ), output );
    }
#else // Linux / MacOS
    if ( 0x1b == input )
    {
        if ( ConsoleConfiguration::portable_kbhit() )
        {
            tracer.Trace( "read an escape on linux... getting next char\n" );
            uint8_t nexta = ConsoleConfiguration::portable_getch();
            tracer.Trace( "read an escape on linux... getting next char again\n" );
            uint8_t nextb = ConsoleConfiguration::portable_getch();
            tracer.Trace( "  nexta: %02x. nextb: %02x\n", nexta, nextb );
        
            if ( '[' == nexta )
            {
                if ( 'A' == nextb )              // up arrow
                    output = 1 + 'E' - 'A';
                else if ( 'B' == nextb )         // down arrow
                    output = 1 + 'X' - 'A';
                else if ( 'C' == nextb )         // right arrow
                    output = 1 + 'D' - 'A';
                else if ( 'D' == nextb )         // left arrow
                    output = 1 + 'S' - 'A';
                else if ( '3' == nextb )         // DEL on linux?
                {
                    uint8_t nextc = ConsoleConfiguration::portable_getch();
                    tracer.Trace( "  nextc: %02x\n", nextc );
                    if ( '~' == nextc )
                        output = 0x7f;
                }
                else
                    tracer.Trace( "unhandled nextb %u == %02x\n", nextb, nextb ); // lots of other keys not on a cp/m machine here
            }
            else
                tracer.Trace( "unhandled linux keyboard escape sequence\n" );
        }
    }
#endif
    else if ( g_backspaceToDel && 0x08 == input )
        output = 0x7f;

    return output;
} //map_input

bool cpm_read_console( char * buf, size_t bufsize, uint8_t & out_len )
{
    char ch = 0;
    out_len = 0;
    while ( out_len < (uint8_t) bufsize )
    {
        ch = (char) ConsoleConfiguration::portable_getch();
        tracer.Trace( "  cpm_read_console read character %02x -- '%c'\n", ch, printable_ch( ch ) );

        // CP/M read console buffer treats these control characters as special: c, e, h, j, m, r, u, x
        // per http://www.gaby.de/cpm/manuals/archive/cpm22htm/ch5.htm
        // Only c, h, j, and m are currently handled correctly.
        // ^c means exit the currently running app in CP/M if it's the first character in the buffer

        if ( ( 3 == ch ) && ( 0 == out_len ) )
            return true;

        if ( '\n' == ch || '\r' == ch )
        {
            printf( "\r" );
            fflush( stdout ); // fflush is required on linux or it'll be buffered and not seen until the app ends.
            break;
        }

        if ( 0x7f == ch || 8 == ch ) // backspace (it's not 8 for some reason)
        {
            if ( out_len > 0 )
            {
                printf( "\x8 \x8" );
                fflush( stdout );
                out_len--;
            }
        }
        else
        {
            printf( "%c", ch );
            fflush( stdout );
            buf[ out_len++ ] = ch;
        }
    }

    return false;
} //cpm_read_console

uint8_t x80_invoke_hook()
{
    static uint64_t kbd_poll_busyloops = 0;
    uint16_t address = reg.pc - 1; // the emulator has moved past this instruction already

    if ( address >= 0xff00 )
    {
        tracer.Trace( "bios function %#x: %s, bc %02x, de %02x, hl %02x\n", address, get_bios_function( address ), reg.B(), reg.D(), reg.H() );
        //x80_trace_state();

        // BIOS call. 0xff00-0xff33 are for actual, documented BIOS calls.
        // 0xff80 and above are when apps like mbasic.com read the jump table, assume it's a series of
        // jmp <16-bit-address> sequences, and call the <16-bit-address> directly.

        if ( 0xff00 == address || 0xff03 == address || 0xff80 == address || 0xff81 == address )
        {
            // boot, which means exit the app

            return OPCODE_HLT;
        }
        else if ( 0xff06 == address || 0xff82 == address )
        {
            // const console status. A=0 if nothing available, A=0xff if a keystroke is available

            if ( ConsoleConfiguration::throttled_kbhit() )
                reg.a = 0xff;
            else
                reg.a = 0;
        }
        else if ( 0xff09 == address || 0xff83 == address )
        {
            // conin

            uint8_t input = (uint8_t) ConsoleConfiguration::portable_getch();
            tracer.Trace( "  conin got %02xh from getch()\n", input );
            reg.a = map_input( input );
            tracer.Trace( "  conin is returning %02xh\n", reg.a );
        }
        else if ( 0xff0c == address || 0xff84 == address )
        {
            // conout

            char ch = reg.c;
            {
                tracer.Trace( "  bios console out: %02x == '%c'\n", ch, printable_ch( ch ) );
                output_character( reg.c );
                fflush( stdout );
            }
        }
        else
        {
            // I only implemented bios calls actually used in apps I tested.

            tracer.Trace( "UNIMPLEMENTED BIOS CODE!!!!!!!!!!!!!!!: %#x\n", address );
            printf( "unhandled bios code!!!!!!!!!!!!!!! %#x\n", address );
        }

        kbd_poll_busyloops = 0;
        return OPCODE_RET;
    }

    if ( 5 != address )
    {
        tracer.Trace( "hook call, but not bios or bdos. likely just a mov h, h. address: %04x\n", address );
        return OPCODE_NOP;
    }

    uint8_t function = reg.c;
    tracer.Trace( "bdos function %d: %s, bc %02x, de %02x, hl %02x\n", function, get_bdos_function( function ), reg.B(), reg.D(), reg.H() );
    //x80_trace_state();

    if ( ( 6 != reg.c ) || ( 0xff != reg.e ) )
        kbd_poll_busyloops = 0;

    // Only BDOS calls called by a apps I tested are implemented. 

    switch( reg.c )
    {
        case 0:
        {
            // system reset. end execution of the app.

            return OPCODE_HLT;
        }
        case 1:
        {
            // console input

            uint8_t input = (uint8_t) ConsoleConfiguration::portable_getch();
            reg.a = map_input( input );

            break;
        }
        case 2:
        {
            // console output
            // CP/M checks for a ^c from the keyboard and ends the application if found. This code doesn't do that yet.

            uint8_t ch = reg.e;
            if ( 0x0d != ch )             // skip carriage return because line feed turns into cr+lf
            {
                tracer.Trace( "  bdos console out: %02x == '%c'\n", ch, printable_ch( ch ) );

                output_character( ch );
                fflush( stdout );
            }

            break;
        }
        case 3:
        {
            // reader input

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
            // e = ff means input -- return char in a if available or return 0 otherwise
            // e != ff means output that character

            if ( 0xff == reg.e )
            {
                if ( ConsoleConfiguration::throttled_kbhit() )
                {
                    kbd_poll_busyloops = 0;
                    uint8_t input = (uint8_t) ConsoleConfiguration::portable_getch();
                    tracer.Trace( "  read character %u == %02x == '%c'\n", input, input, printable_ch( input ) );
                    reg.a = map_input( input );
                }
                else
                {
                    if ( kbd_poll_busyloops > 20 )
                    {
                        // some apps like forth and multiplan call this in a busy loop.
                        // don't sleep every call because sometimes they alternate calling this with updating the display.

                        sleep_ms( 1 );
                        tracer.Trace( "sleeping in direct console i/o\n" );
                        kbd_poll_busyloops = 0;
                    }
                    else
                        kbd_poll_busyloops++;
                    reg.a = 0;
                }
            }
            else
            {
                uint8_t ch = reg.e;
                tracer.Trace( "  bdos direct console i/o output: %u == %02x == '%c'\n", ch, ch, printable_ch( ch ) );
                output_character( ch );
                fflush( stdout );
            }

            break;
        }
        case 9:
        {
            // print string terminated by a dollar sign $. string is pointed to by DE

            uint16_t i = reg.D();
            uint32_t count = 0;
            //tracer.TraceBinaryData( memory + i, 0x20, 0 );
    
            while ( '$' != memory[i] )
            {
                if ( count++ > 2000 ) // arbitrary limit, but probably a bug if this long
                {
                    tracer.Trace( "  ERROR: String to print is too long!\n", stderr );
                    break;
                }

                uint8_t ch = memory[ i++ ];
                if ( 0x0d != ch )              // skip carriage return because line feed turns into cr+lf
                {
                    output_character( ch );
                    fflush( stdout );
                }
            }

            break;
        }
        case 10:
        {
            // read console buffer. DE: buffer address. buffer:
            //   0      1       2       3
            //   in_len out_len char1   char2 ...

            uint16_t offset = reg.D();
            char * pbuf = (char *) memory + offset;
            pbuf[ 1 ] = 0;
            uint16_t in_len = *pbuf;

            if ( in_len > 0 )
            {
                pbuf[ 2 ] = 0;
                uint8_t out_len;
                bool reboot = cpm_read_console( pbuf + 2, in_len, out_len );
                if ( reboot )
                {
                    tracer.Trace( "  bdos read console buffer read a ^c at the first position, so it's terminating the app\n" );
                    return OPCODE_HLT;
                }

                pbuf[ 1 ] = out_len;
                tracer.Trace( "  read console len %u, string '%.*s'\n", out_len, (size_t) out_len, pbuf + 2 );
            }
            else
                tracer.Trace( "WARNING: read console buffer asked for input but provided a 0-length buffer\n" );

            break;
        }
        case 11:
        {
            // console status. return A=0 if no characters are waiting or non-zero if a character is waiting

            if ( ConsoleConfiguration::throttled_kbhit() )
                reg.a = 0xff;
            else
                reg.a = 0;

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
    
            tracer.Trace( "  open file\n" );
    
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
                    // the cp/m 2.2 spec says that filenames may contain question marks. I haven't found an app that uses that.

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
    
            tracer.Trace( "  close file\n" );
    
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

            tracer.Trace( "  search for first\n" );

            // Find First on CP/M has a side-effect of flushing data to disk. Aztec C relies on this and calls
            // Find First at exit() to ensure the disk is flushed, though it does close all files first.

            fflush( 0 );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  searchinf for first match of '%s'\n", acFilename );

#ifdef _MSC_VER
                if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                {
                    FindClose( g_hFindFirst );
                    g_hFindFirst = INVALID_HANDLE_VALUE;
                }

                BOOL found = FALSE;
                WIN32_FIND_DATAA fd = {0};
                g_hFindFirst = FindFirstFileA( acFilename, &fd );
                if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                {
                    do
                    {
                        if ( ValidCPMFilename( fd.cFileName ) )
                        {
                            ParseFoundFile( fd.cFileName );
                            reg.a = 0;
                            found = TRUE;
                            break;
                        }
                        else
                        {
                            found = FindNextFileA( g_hFindFirst, &fd );
                            if ( !found )
                                break;
                        }
                    } while ( true );
                }
                else
                    tracer.Trace( "WARNING: find first file failed, error %d\n", GetLastError() );

                if ( !found )
                {
                    if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                    {
                        FindClose( g_hFindFirst );
                        g_hFindFirst = INVALID_HANDLE_VALUE;
                    }

                    tracer.Trace( "WARNING: find first file couldn't find a single match\n" );
                }
#else           
                if ( 0 != g_FindFirst )
                {
                    closedir( g_FindFirst );
                    g_FindFirst = 0;
                }

                LINUX_FIND_DATA fd = {0};
                g_FindFirst = FindFirstFileLinux( acFilename, fd );
                if ( 0 != g_FindFirst )
                {
                    ParseFoundFile( fd.cFileName );
                    reg.a = 0;
                }
                else
                    tracer.Trace( "WARNING: find first file failed, error %d\n", errno );
#endif                    
            }
            else
                tracer.Trace( "ERROR: can't parse filename for search for first\n" );

            break;
        }
        case 18:
        {
            // search for next. Use the FCB in de and write directory entries to the DMA address, then point to
            // which of those entries is the actual one (0-3) or 0xff for not found in A.

            tracer.Trace( "  search for next\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  searchinf for next match of '%s'\n", acFilename );

#ifdef _MSC_VER
                if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                {
                    BOOL found = FALSE;

                    do
                    {
                        WIN32_FIND_DATAA fd = {0};
                        found = FindNextFileA( g_hFindFirst, &fd );
                        if ( found )
                        {
                            if ( ValidCPMFilename( fd.cFileName ) )
                            {
                                ParseFoundFile( fd.cFileName );
                                reg.a = 0;
                                break;
                            }
                            else
                                found = FALSE;
                        }
                        else
                            break;
                    } while ( true );

                    if ( !found )
                    {
                        tracer.Trace( "WARNING: find next file found no more, error %d\n", GetLastError() );
                        FindClose( g_hFindFirst );
                        g_hFindFirst = INVALID_HANDLE_VALUE;
                    }
                }
                else
                    tracer.Trace( "ERROR: search for next without a prior successful search for first\n" );
#else
                if ( 0 != g_FindFirst )
                {
                    LINUX_FIND_DATA fd = {0};
                    bool found = FindNextFileLinux( acFilename, g_FindFirst, fd );
                    if ( found )
                    {
                        ParseFoundFile( fd.cFileName );
                        reg.a = 0;
                    }
                    else
                    {
                        tracer.Trace( "WARNING: find next file found no more, error %d\n", errno );
                        FindCloseLinux( g_FindFirst );
                        g_FindFirst = 0;
                    }
                }
                else
                    tracer.Trace( "ERROR: search for next without a prior successful search for first\n" );
#endif                    
            }
            else
                tracer.Trace( "ERROR: can't parse filename for search for first\n" );

            break;
        }
        case 19:
        {
            // delete file. return 255 if file not found and 0..3 directory code otherwise
    
            tracer.Trace( "  delete file\n" );
    
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            trace_FCB( pfcb );
            reg.a = 255;
    
            char acFilename[ CPM_FILENAME_LEN ];
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                // if deleting an open file, close it first. CalcStar does this on file save.

                if ( FindFileEntry( acFilename ) )
                {
                    FILE * fp = RemoveFileEntry( acFilename );
                    if ( fp )
                        fclose( fp );
                }

                int removeok = ( 0 == remove( acFilename ) );
                tracer.Trace( "  attempt to remove file '%s' result ok: %d\n", acFilename, removeok );
                if ( removeok )
                    reg.a = 0;
                else
                    tracer.Trace( "  error %d\n", errno );
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
    
            tracer.Trace( "  read sequential file\n" );
    
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
                    tracer.Trace( "  reading at offset %u\n", curr );

                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
                    fseek( fp, curr, SEEK_SET );
                    tracer.Trace( "  file size: %u, current %u\n", file_size, curr );

                    uint32_t to_read = get_min( file_size - curr, (uint32_t) 128 );
                    memset( g_DMA, 0x1a, 128 ); // fill with ^z, the EOF marker in CP/M
        
                    size_t numread = fread( g_DMA, to_read, 1, fp );
                    if ( numread > 0 )
                    {
                        tracer.TraceBinaryData( g_DMA, 128, 0 );
        
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
    
            tracer.Trace( "  write sequential file\n" );
    
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
                    tracer.Trace( "  writing at offset %#x = %u\n", curr, curr );
        
                    tracer.TraceBinaryData( g_DMA, 128, 0 );
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
    
            tracer.Trace( "  make file\n" );
    
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

                    tracer.Trace( "  successfully created fp %p for write\n", fp );
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

            tracer.Trace( "  rename file\n" );
    
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
            tracer.Trace( "  updating DMA address; D %u = %#x\n", reg.D(), reg.D() );

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

            tracer.Trace( "  set file attributes \n" );
    
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
        case 31:
        {
            // get PDB address. Get Disk Parameter Block. Return the address in HL

            reg.h = DPB_HI_OFFSET;
            reg.l = DPB_LO_OFFSET;

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

            tracer.Trace( "  read random\n" );
    
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
                    tracer.Trace( "  read random record %#x\n", record );
                    uint32_t file_offset = record * 128;
                    memset( g_DMA, 0x1a, 128 ); // fill with ^z, the EOF marker in CP/M
    
                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
    
                    // OS workaround for app bug: Turbo Pascal expects a read just past the end of file to succeed.
    
                    if ( file_size == file_offset )
                    {
                        tracer.Trace( "  random read past eof\n" );
                        reg.a = 1;
                        break;
                    }
    
                    if ( file_size > file_offset )
                    {
                        uint32_t to_read = get_min( file_size - file_offset, (uint32_t) 128 );
                        ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                        {
                            tracer.Trace( "  reading random at offset %#x\n", file_offset );
                            size_t numread = fread( g_DMA, to_read, 1, fp );
                            if ( numread )
                            {
                                tracer.TraceBinaryData( g_DMA, to_read, 0 );
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
                    {
                        reg.a = 1;
                        tracer.Trace( "ERROR: read random read at %u beyond end of file size %u\n", file_offset, file_size );
                    }
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

            tracer.Trace( "  write random\n" );
    
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
                    uint32_t file_offset = record * 128;
    
                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );

                    tracer.Trace( "  write random file %p, record %#x, file_offset %d, file_size %d\n", fp, record, file_offset, file_size );

                    if ( file_offset > file_size )
                    {
                        ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                            file_size = ftell( fp );
                        else
                            tracer.Trace( "  can't seek to extend file with zeros, error %d = %s\n", errno, strerror( errno ) );
                    }
    
                    if ( file_size >= file_offset )
                    {
                        ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                        {
                            tracer.Trace( "  writing random at offset %#x\n", file_offset );
                            tracer.TraceBinaryData( g_DMA, 128, 0 );
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
                        tracer.Trace( "ERROR: write random at offset %d beyond end of file size %d\n", file_offset, file_size );
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

            tracer.Trace( "  compute file size\n" );
    
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
                    pfcb->SetRandomIOOffset( (uint16_t) ( file_size / 128 ) );
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

            tracer.Trace( "  set random record\n" );
    
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
                    pfcb->SetRandomIOOffset( (uint16_t) ( curr / 128 ) );
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

            tracer.Trace( "  write random with zero fill\n" );
    
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
                    tracer.Trace( "  write random record %#x\n", record );
                    uint32_t file_offset = record * 128;
    
                    fseek( fp, 0, SEEK_END );
                    uint32_t file_size = ftell( fp );
                    if ( file_size >= file_offset )
                    {
                        ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                        {
                            tracer.Trace( "  writing random at offset %#x\n", file_offset );
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
        case 105:
        {
            // non-standard BDOS call GetTime. DE points to a CPMTime structure

            tracer.Trace( "  get time (non-standard BDOS call)\n" );
            CPMTime * ptime = (CPMTime *) ( memory + reg.D() );

            system_clock::time_point now = system_clock::now();
            uint64_t ms = duration_cast<milliseconds>( now.time_since_epoch() ).count() % 1000;
            time_t time_now = system_clock::to_time_t( now );
            struct tm * plocal = localtime( & time_now );

            ptime->hour = (uint16_t) plocal->tm_hour;
            ptime->minute = (uint16_t) plocal->tm_min;
            ptime->second = (uint16_t) plocal->tm_sec;
            ptime->millisecond = (uint16_t) ( ms / 10 ); // hundredths of a second;
            reg.a = 0;

            break;
        }
        case 106:
        {
            // non-standard BDOS call sleep. DE contains a count of milliseconds 0-32767

            uint16_t ms = reg.D();
            sleep_ms( ms );
            break;
        }
#ifdef NTVCM_RSS_SUPPORT
        case 107:
        {
            // non-standard BDOS call load rss feeds. DE points to a 0-terminated list of URLs
            // hl returns the # of items loaded
            // any single item can only be of size 2048 when rendered by fetch_rss_item

            uint16_t * poffsets = (uint16_t *) ( memory + reg.D() );

            char * afeeds[ 10 ];
            size_t count;
            for ( count = 0; 0 != poffsets[ count ] && count < _countof( afeeds ); count++ )
                afeeds[ count ] = (char *) ( memory + poffsets[ count ] );
            afeeds[ count ] = 0;

            size_t result = g_rssFeed.load_rss_feeds( afeeds, 2048 );
            tracer.Trace( "  found %zd items in %zd feeds feeds\n", result, count );
            reg.SetH( (uint16_t) result );
            break;
        }
        case 108:
        {
            // non-standard BDOS call load rss item. DE is the item to load. DMA points to
            // a 2048 byte buffer of the form name0title0description0

            char * buf = (char *) g_DMA;
            uint16_t item = reg.D();

            reg.a = g_rssFeed.fetch_rss_item( item, buf, 2048 );
            break;
        }
#endif //NTVCM_RSS_SUPPORT
        case 109:
        {
            // non-standard BDOS call puts a random number in A

            reg.a = (uint8_t) rand();
            break;
        }
        case 110:
        {
            // non-standard BDOS call: enable/disable tracing if DE is non-zero/zero

            x80_trace_instructions( 0 != reg.D() );
            break;
        }
        default:
        {
            tracer.Trace( "UNIMPLEMENTED BDOS FUNCTION!!!!!!!!!!!!!!!: %u = %#x\n", reg.c, reg.c );
            printf( "UNIMPLEMENTED BDOS FUNCTION!!!!!!!!!!!!!!!: %u = %#x\n", reg.c, reg.c );

            x80_trace_state();
            reg.a = 0xff;
        }
    }

    return OPCODE_RET;
} //x80_invoke_hook

void usage( char const * perr = 0 )
{
    g_consoleConfig.RestoreConsole( false );

    if ( perr )
        printf( "error: %s\n", perr );

    printf( "NT Virtual CP/M 2.2 Machine: emulates a CP/M 2.2 i8080/Z80 runtime environment\n" );
    printf( "usage: ntvcm [-c] [-p] [-s:X] [-t] <cp/m 2.2 .com file> [filearg1] [filearg2]\n" );
    printf( "  notes: filearg1 and filearg2 optionally specify filename arguments for the CP/M command\n" );
    printf( "         -b     turn bios console key backspace/BS/0x08 to delete/DEL/0x7f. for Turbo Pascal\n" );
    printf( "         -c     never auto-detect ESC characters and change to to 80x24 mode\n" );
    printf( "         -C     always switch to 80x24 mode (Windows only)\n" );
    printf( "         -d     don't clear the display on app exit when in 80x24 mode\n" );
    printf( "         -i     trace 8080/Z80 instructions when tracing with -t\n" );
    printf( "         -k     translate Kaypro II extended characters to Windows code page 437 or Linux ascii art\n" );
    printf( "         -l     force CP/M filenames to be lowercase (can be useful on Linux)\n" );
    printf( "         -p     show performance information at app exit\n" ); 
    printf( "         -s:X   speed in Hz. Default is 0, which is as fast as possible.\n" );
    printf( "                for 4Mhz, use -s:4000000\n" );
    printf( "         -t     enable debug tracing to ntvcm.log\n" );
    printf( "         -v:X   translate escape sequences to VT-100 where X can be one of:\n" );
    printf( "                  5:  translate vt-52 escape sequences. for CalcStar and other apps\n" );
    printf( "                  k:  translate Kaypro II / Lear-Siegler ADM-3A escape sequences. for strtrk.com\n" );
    printf( "         -z:X   applies X as a hex mask to SetProcessAffinityMask, e.g.:\n" );
    printf( "                  /z:11    2 performance cores on an i7-1280P\n" );
    printf( "                  /z:3000  2 efficiency cores on an i7-1280P\n" );
    printf( "                  /z:11    2 random good cores on a 5950x\n" );
    printf( "         -8     emulate the i8080, not Z80\n" );
    printf( "  e.g. to assemble, load, and run test.asm:\n" );
    printf( "       ntvcm asm.com test\n" );
    printf( "       ntvcm load.com test\n" );
    printf( "       ntvcm test.com\n" );
    printf( "  e.g. to run Star Trek in mbasic in 80x24 mode using i8080 emulation:\n" );
    printf( "       ntvcm -8 -C mbasic startrek.bas\n" );
    printf( "  %s\n", build_string() );
    exit( -1 );
} //usage

bool write_arg( FCB_ARGUMENT * arg, char * pc )
{
    if ( ':' == pc[ 1 ] )
    {
        if ( pc[0] > 'P' || pc[0] < 'A' )
            return false; // don't write arguments that don't look like filenames

        arg->dr = 1 + pc[0] - 'A';
        pc += 2;
    }

    char * dot = strchr( pc, '.' );
    if ( dot )
    {
        memcpy( & ( arg->f ), pc, dot - pc );
        memcpy( & ( arg->t ), dot + 1, strlen( dot + 1 ) );
    }
    else
        memcpy( & ( arg->f ), pc, strlen( pc ) );

    return true;
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

int ends_with( const char * str, const char * end )
{
    size_t len = strlen( str );
    size_t lenend = strlen( end );

    if ( len < lenend )
        return false;

    return ( 0 == _stricmp( str + len - lenend, end ) );
} //ends_with

int main( int argc, char * argv[] )
{
    bump_thread_priority(); // for performance benchmarking only

    memset( memory, 0, sizeof( memory ) );
    memset( &reg, 0, sizeof( reg ) );
    reg.fZ80Mode = true;

    char * pCommandTail = (char *) memory + COMMAND_TAIL_OFFSET;
    char * pCommandTailLen = (char *) memory + COMMAND_TAIL_LEN_OFFSET;
    char * pcCOM = 0;
    char * pcArg1 = 0;
    char * pcArg2 = 0;
    bool trace = false;
    bool traceInstructions = false;
    uint64_t clockrate = 0;
    bool showPerformance = false;
    bool force80x24 = false;
    bool clearDisplayOnExit = true;
    uint64_t processAffinityMask = 0; // by default let the OS decide

    for ( int i = 1; i < argc; i++ )
    {
        char *parg = argv[i];
        char c = *parg;

        // linux shell scripts pass carriage returns '\r' at the end of strings for DOS-style cr/lf files

        char * pR = strchr( parg, '\r' );
        if ( 0 != pR )
            *pR = 0;

        // append arguments past the .com file to the command tail

        if ( 0 != pcCOM )
        {
            size_t tailLen = strlen( pCommandTail ) + strlen( parg ) + 1 + 1; // +1 null termination +1 space
            if ( tailLen > 127 )
                usage( "command length is too long for the 127 char limit in CP/M" );

            // CP/M puts a space at the start of non-zero-length command tails. Also, add a space between arguments.

            strcat( pCommandTail, " " );
            strcat( pCommandTail, parg );
        }

        if ( 0 == pcCOM && ( '-' == c || '/' == c ) )
        {
            char ca = (char) tolower( parg[1] );

            if ( 's' == ca )
            {
                if ( ':' == parg[2] )
                    clockrate = strtoull( parg + 3 , 0, 10 );
                else
                    usage( "colon required after s argument" );
            }
            else if ( 'd' == ca )
                clearDisplayOnExit = false;
            else if ( '8' == ca )
                reg.fZ80Mode = false;
            else if ( 'i' == ca )
                traceInstructions = true;
            else if ( 'l' == ca )
                g_forceLowercase = true;
            else if ( 'k' == ca )
                g_kayproToCP437 = true;
            else if ( 't' == ca )
                trace = true;
            else if ( 'p' == ca )
                showPerformance = true;
            else if ( 'b' == parg[1] )
                g_backspaceToDel = true;
            else if ( 'v' == parg[1] )
            {
                if ( ':' != parg[2] )
                    usage( "colon required after v argument" );

                if ( 'k' == tolower( parg[3] ) )
                    g_termEscape = termKayproII;
                else if ( '5' == parg[3] )
                    g_termEscape = termVT52;
                else
                    usage( "invalid terminal emulation identifier. Only 5 and k are supported" );
            }
            else if ( 'c' == parg[1] )
                g_forceConsole = true;
            else if ( 'C' == parg[1] )
                force80x24 = true;
            else if ( 'z' == ca )
            {
                if ( ':' == parg[2] )
                    processAffinityMask = strtoull( parg + 3 , 0, 16 );
                else
                    usage( "colon required after z argument" );
            }
            else
                usage( "invalid argument specified" );
        }
        else
        {
            if ( 0 == pcCOM )
                pcCOM = parg;
            else if ( 0 == pcArg1 && '-' != *parg )
                pcArg1 = parg;
            else if ( 0 == pcArg2 && '-' != *parg )
                pcArg2 = parg;
        }
    }

    tracer.Enable( trace, L"ntvcm.log", true );
    tracer.SetQuiet( true );
    tracer.SetFlushEachTrace( true );
    x80_trace_instructions( traceInstructions );

    if ( 0 != processAffinityMask )
        set_process_affinity( processAffinityMask );

    if ( 0 == pcCOM )
    {
        usage( "no CP/M command specified" );
        assume_false;
    }

    * pCommandTailLen = (char) strlen( pCommandTail );

    tracer.Trace( "command tail len %d value: '%s'\n", *pCommandTailLen, pCommandTail );

    char acCOM[ MAX_PATH ] = {0};
    strcpy( acCOM, pcCOM );

    if ( !file_exists( acCOM ) )
    {
        if ( ends_with( acCOM, ".com" ) )
            usage( "can't find command file" );
        else
        {
            strcat( acCOM, ".com" );
            if ( !file_exists( acCOM ) )
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
        _strupr( pcArg1 );
        write_arg( arg1, pcArg1 );

        if ( pcArg2 )
        {
            _strupr( pcArg2 );
            write_arg( arg2, pcArg2 );
        }
    }

    tracer.Trace( "fcb argument 1:\n" );
    trace_FCB( (FCB *) arg1 );
    tracer.Trace( "fcb argument 2:\n" );
    trace_FCB( (FCB *) arg2 );

    // make memory look like CP/M 2.2. The first 8-byte interrupt vector has this:

    memory[0] = OPCODE_HLT;  // when an app exits by jumping or returning here, halt execution
    memory[1] = 0x03;        // low byte of BIOS jump table. boot is at -3 from this address. wboot is here.
    memory[2] = 0xff;        // high byte of BIOS jump table
    memory[3] = 0;           // use TTY: for console, READER, PUNCH, and LIST
    memory[4] = 0;           // default drive 0 == A
    memory[5] = OPCODE_HOOK; // BDOS entry point
    memory[6] = 0x00;        // these two bytes point to the first byte above app-available RAM (reserved RAM)
    memory[7] = 0xfe;

    // The real bios function table is a list of 3-byte entries containing jmp and the address of
    // each of the 16 bios functions (17 including the -1 entry to exit).
    // Here, just hook the jmp instruction and put a pointer to the hook opcode in each address.
    // Apps like mbasic.com don't call bios functions; they take the address from the vector and
    // call it directly to save a tiny bit of performance.
    // The memory map is here (giving apps a little more RAM than standard CP/M):
    //   0000-003f: CP/M global storage + RST hardware interrupt service routines. 8 entries are 8 bytes each
    //   0040-00ff: CP/M global storage
    //   0100-????: App run space growing upward until it collides with the stack
    //   ????-fdfd: Stack growing downward until it collides with the app
    //   fdfe-fdff: two bytes of 0 so apps can return instead of a standard app exit. ccp has "call 0x100" it's OK to return.
    //   fe00-fe10: reserved space for bdos; filled with the Disk Parameter Block for BDOS call 31 Get DPB.
    //   fe10-ff00: reserved space for bdos; filled with 0s
    //   ff00-ff33: bios jump table of 3*17 bytes. (0xff03 is stored at addess 0x1)
    //   ff80-ff91: where bios jump table addresses point, filled with OPCODE_HOOK
    //   ff92-ffff: unused, filled with 0
    //
    // On a typical CP/M machine:
    //   0000-003f: RST hardware interrupt service routines. 8 entries are 8 bytes each
    //   0040-00ff: CP/M global storage
    //   0100-????: app space
    //   ????-e3a9: stack given to apps at start.
    //   e406-????: bdos
    //   f200-????: bios (0xf203 stored at address 0x1)

    memset( memory + 0xff00, OPCODE_HOOK, 0xff );
    for ( uint16_t v = 0; v < 17; v++ )
        setmword( 0xff01 + ( v * 3 ), 0xff80 + v );

    int file_size = 0;
    bool ok = load_file( acCOM, file_size, 0, memory + 0x100 );
    if ( !ok )
    {
        printf( "unable to load command %s\n", acCOM );
        exit( 1 );
    }

    // Use made-up numbers that look believable enough to a CP/M app checking for free disk space. 107k.

    DiskParameterBlock * pdpb = (DiskParameterBlock *) ( memory + ( DPB_HI_OFFSET << 8 ) + DPB_LO_OFFSET );
    pdpb->spt = 128;
    pdpb->bsh = 3;
    pdpb->blm = 7;
    pdpb->exm = 7;
    pdpb->dsm = 127;
    pdpb->drm = 1;
    pdpb->al0 = 0xf0;
    pdpb->al1 = 0;
    pdpb->off = 0;

    memory[ 0x100 + file_size ] = OPCODE_HLT; // in case the app doesn't shutdown properly
    reg.powerOn();       // set default values of registers
    reg.pc = 0x100;
    reg.sp = 0xfdfe;     // the stack is written to below this address. 2 bytes here are zero for ret from entry
    reg.a = reg.b = reg.c = reg.d = reg.e = reg.h = reg.l = 0;
    reg.fp = reg.ap = 0; // apparently this is expected by CPUTEST
    reg.bp = reg.cp = reg.dp = reg.ep = reg.hp = reg.lp = 0;
    reg.ix = reg.iy = 0;
    g_haltExecuted = false;

    if ( trace )
    {
        x80_trace_state();
        tracer.Trace( "starting execution of app '%s' size %d\n", acCOM, file_size );
    }

    if ( force80x24 )
        g_consoleConfig.EstablishConsoleOutput( 80, 24 );

    uint64_t total_cycles = 0;
    CPUCycleDelay delay( clockrate );
    high_resolution_clock::time_point tStart = high_resolution_clock::now();

    do
    {
        total_cycles += x80_emulate( 1000 );

        if ( g_haltExecuted )
            break;

        delay.Delay( total_cycles );
    } while ( true );

    high_resolution_clock::time_point tDone = high_resolution_clock::now();
    g_consoleConfig.RestoreConsole( clearDisplayOnExit );

    CloseFindFirst();

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

#ifdef NTVCM_RSS_SUPPORT
    g_rssFeed.clear();
#endif //NTVCM_RSS_SUPPORT

    tracer.Shutdown();
} //main

