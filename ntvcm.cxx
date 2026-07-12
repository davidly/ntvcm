// NT Virtual CP/M Machine
// This app runs CP/M 2.2 .com programs on Windows, MacOS, Linux, and DOS (real-mode 8086 using WATCOM compiler)
// Written by David Lee in late 2022
// Notes:   -- Only the subset of CP/M 2.2 required to run the apps I tested is implemented.
//          -- asm.com, load.com, and Turbo Pascal 1.00 & 3.01A are rested.
//          -- Also tested with Wordstar Release 4 and mbasic.com BASIC-80 Ref 5.21.
//          -- Also tested with Aztec C 1.06. compiler, assembler, linker, and generated apps work.
//          -- Also tested with MBasic.
//          -- Use tinst.com to configure Turbo Pascal 1.00 for vT100 and 3.01A for ANSI to get the screen to work.
//          -- pip.com runs for simple file copies. Not tested for other modes, which probably fail.
//          -- Tested with AlgolM, Janus ADA, CB80, Turbo Modula 2, Multiplan, Microsoft Fortran, MT Pascal, JRT Pascal, and more.
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
// To build on Windows targeting DOS:
//    I used Open Watcom C/C++ x86 16-bit Compile and Link Utility Version 2.0 beta Oct  9 2023 02:19:55 (64-bit)
//    https://github.com/open-watcom/open-watcom-v2/releases/tag/Current-build
//    wcl -q -zp=1 -ml -obmr -oh -ei -oi -s -0 -xs -j -oe=128 -ol+ -ot ntvcm.cxx x80.cxx -bcl=DOS -k8192 /I. /DWATCOMDOS /DNDEBUG
//    helpful: https://open-watcom.github.io/open-watcom-v2-wikidocs/clib.html
// Note: openmp, wininet, ssl, crypto, and djl_rssrdr.hxx are only required for RSS support.
//

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <cstring>
#include <assert.h>
#include <ctype.h>
#include <sys/stat.h>
#if !defined( _WIN32 ) && !defined( WATCOMDOS )
#include <sys/select.h>
#include <unistd.h>
#endif

#include <djl_os.hxx>
#include <djltrace.hxx>
#include <djl_con.hxx>
#include <djl_cycle.hxx>

#define FILENAME   "ntvcm"
#define VERSION    "0.1"
#if !defined(BUILD)
#define BUILD      ""
#endif
#if !defined(COMMIT_ID)
#define  COMMIT_ID  ""
#endif

// On non-Windows platforms djl_rssrdr.hxx has a dependency on:
//     httplib.h from https://github.com/yhirose/cpp-httplib
//     openssl headers and libraries

#ifdef NTVCM_RSS_SUPPORT
    #include <djl_rssrdr.hxx>
    CRssFeed g_rssFeed;
#endif //NTVCM_RSS_SUPPORT

#include "x80.hxx"
#include "ntvcm.h" // get bdos extensions

// CP/M constants for memory addresses where OS-global state is stored

#define FCB_ARG1_OFFSET            0x5c
#define FCB_ARG2_OFFSET            0x6c
#define COMMAND_TAIL_LEN_OFFSET    0x80
#define COMMAND_TAIL_OFFSET        0x81
#define DEFAULT_DMA_OFFSET         0x80 // read arguments before doing I/O because it's the same address

// The BDOS invocation function that ships with M80 incorrectly assumes the BIOS_JUMP_TABLE low byte (minus 3) is 0.
// The BDOS_ENTRY address must be one byte beyond free memory for the app.
// So 260 bytes must be reserved: 256 for page-aligned BIOS and 4 bytes for the BDOS stub.

const uint8_t  BDOS_ENTRY_LO =       0xfc;
const uint8_t  BDOS_ENTRY_HI =       0xfe;
const uint16_t BDOS_ENTRY =          ( ( BDOS_ENTRY_HI << 8 ) | BDOS_ENTRY_LO );

const uint8_t  BIOS_JUMP_TABLE_LO =  0x00;
const uint8_t  BIOS_JUMP_TABLE_HI =  0xff;
const uint16_t BIOS_JUMP_TABLE =     ( ( BIOS_JUMP_TABLE_HI << 8 ) | BIOS_JUMP_TABLE_LO );

const uint16_t BIOS_FUNCTIONS =      0xff40; // 17 1-byte hooks
const uint16_t BIOS_FUNCTION_COUNT = 17;

const uint8_t  DPB_OFFSET_LO =       0x60; // lo part of DPB
const uint8_t  DPB_OFFSET_HI =       0xff; // where the Disk Parameter Block resides for BDOS 31.
const uint16_t DPB_OFFSET =          ( ( DPB_OFFSET_HI << 8 ) | DPB_OFFSET_LO );

#define CPM_FILENAME_LEN ( 8 + 3 + 1 + 1 ) // name + type + dot + null

// cr = current record = ( file pointer % 16k ) / 128
// ex = current extent = ( file pointer % 512k ) / 16k
// s2 = extent high    = ( file pointer / 512k )

struct FCB
{
    uint8_t dr;            // 00 0 = default drive, 1 = A ... 16 = P
    uint8_t f[8];          // 01 file name. uppercase ascii or spaces
    uint8_t t[3];          // 09 file type. uppercase ascii or spaces
    uint8_t ex;            // 12 extent 0..31 during I/O
    uint8_t s1;            // 13 reserved for CP/M
    uint8_t s2;            // 14 reserved for CP/M. extent high byte
    uint8_t rc;            // 15 record count for extent ex. 0..127
    uint8_t dImage[16];    // 16 second half of directory entry OR rename new name
    uint8_t cr;            // 32 current record to read or write in sequential file operations
    uint8_t r0;            // 33 low byte of random I/O offset.  Also for Compute File Size.
    uint8_t r1;            // 34 high byte of random I/O offset. Also for Compute File Size.
    uint8_t r2;            // 35 overflow of r0 and r1 (unused in CP/M 2.2)

    // r0 and r1 are a 16-bit count of 128 byte records

    uint16_t GetRandomIOOffset() { return ( (uint16_t) this->r1 << 8 ) | this->r0; }

    void SetRandomIOOffset( uint16_t o )
    {
        this->r0 = ( 0xff & o );
        this->r1 = ( ( o & 0xff00 ) >> 8 );
        // unused in cp/m 2.2 this->r2 = 0;
    } //SetRandomIOOffset

    void SetRecordCount( FILE * fp )
    {
        // set rc to file size in 128 byte records if < 16k, else 128
        uint32_t file_size = portable_filelen( fp );
        if ( file_size >= ( 16 * 1024 ) ) // CP/M 2.2 does this and Whitesmith C's A80.COM and LNK.COM depend on it
            this->rc = 128;
        else
        {
            uint32_t tail_size = ( file_size % ( 16 * 1024 ) ); // won't matter because of 16k check above
            this->rc = (uint8_t) ( tail_size / 128 );
            if ( 0 != ( tail_size % 128 ) )
                this->rc++;
        }
    } //SetRecordCount

    void UpdateSequentialOffset( uint32_t offset )
    {
        cr = (uint8_t) ( ( offset % ( (uint32_t) 16 * 1024 ) ) / (uint32_t) 128 );
        ex = (uint8_t) ( ( offset % ( (uint32_t) 512 * 1024 ) ) / ( (uint32_t) 16 * 1024 ) );
        s2 = (uint8_t) ( offset / ( (uint32_t) 512 * 1024 ) );
        #ifdef WATCOMDOS
        tracer.Trace( "  new offset: %u, s2 %u, ex %u, cr %u\n", (uint16_t) offset, (uint16_t) s2, (uint16_t) ex, (uint16_t) cr );
        #else
        tracer.Trace( "  new offset: %u, s2 %u, ex %u, cr %u\n", offset, s2, ex, cr );
        #endif
    } //UpdateSequentialOffset

    uint32_t GetSequentialOffset()
    {
        uint32_t curr = (uint32_t) cr * 128;
        curr += ( (uint32_t) ex * ( (uint32_t) 16 * 1024 ) );
        curr += ( (uint32_t) s2 * ( (uint32_t) 512 * 1024 ) );
        return curr;
    } //GetSequentialOffset

    void Trace( bool justArg = false ) // justArg is the first 16 bytes at app startup
    {
        tracer.Trace( "  FCB at address %04x:\n", (uint32_t) ( (uint8_t * ) this - memory ) );
        tracer.Trace( "    drive:    %#x == %c\n", dr, ( 0 == dr ) ? 'A' : 'A' + dr - 1 );
        tracer.Trace( "    filename: '%c%c%c%c%c%c%c%c'\n", 0x7f & f[0], 0x7f & f[1], 0x7f & f[2], 0x7f & f[3],
                                                            0x7f & f[4], 0x7f & f[5], 0x7f & f[6], 0x7f & f[7] );
        tracer.Trace( "    filetype: '%c%c%c'\n", 0x7f & t[0], 0x7f & t[1], 0x7f & t[2] );
        tracer.Trace( "    R S A:    %d %d %d\n", 0 != ( 0x80 & t[0] ), 0 != ( 0x80 & t[1] ), 0 != ( 0x80 & t[2] ) );
        tracer.Trace( "    ex:       %d\n", ex );
        tracer.Trace( "    s1:       %u\n", s1 );
        tracer.Trace( "    s2:       %u\n", s2 );
        tracer.Trace( "    rc:       %u\n", rc );
        if ( !justArg )
        {
            tracer.Trace( "    cr:       %u\n", cr );
            tracer.Trace( "    r0:       %u\n", r0 );
            tracer.Trace( "    r1:       %u\n", r1 );
            tracer.Trace( "    r2:       %u\n", r2 );
        }
    } //Trace
};

// this struct is used to cache FILE * objects to avoid open/close for each file access

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

    void swap_endian()
    {
        hour = flip_endian16( hour );
        minute = flip_endian16( minute );
        second = flip_endian16( second );
        millisecond = flip_endian16( millisecond );
    }
};

#pragma pack( push, 1 )
struct CPM3DateTime
{
    uint16_t day; // day 1 is 1 January 1978
    uint8_t hour; // packed bcd (nibbles for each digit)
    uint8_t minute; // packed bcd
    uint8_t second; // packed bcd. for BDOS 155, not BDOS 105

    void swap_endian()
    {
        day = flip_endian16( day );
    }
};

uint8_t packBCD( uint8_t x )
{
    return (uint8_t) ( ( ( x / 10 ) << 4 ) | ( x % 10 ) );
} //packBCD

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

    void swap_endian()
    {
        spt = flip_endian16( spt );
        dsm = flip_endian16( dsm );
        drm = flip_endian16( drm );
        cks = flip_endian16( cks );
        off = flip_endian16( off );
    }
};
#pragma pack(pop)

CDJLTrace tracer;
ConsoleConfiguration g_consoleConfig;

static bool g_haltExecuted = false;
static bool g_emulationEnded = false;
static bool g_exitCodeSet = false;
static bool g_profileWriteFailed = false;
static uint16_t g_exitCode = 0;
static uint8_t * g_DMA = memory + DEFAULT_DMA_OFFSET;
static vector<FileEntry> g_fileEntries;
static bool g_forceConsole = false;
static bool g_forceLowercase = false;
static bool g_backspaceToDel = false;
static bool g_kayproToCP437 = false;
static size_t g_fileInputOffset = 0;
static vector<char> g_fileInputText;
static bool g_sleepOnKbdLoop = true;
static bool g_clearHOnBDOSReturn = true;
static bool g_miMode = false;
static char g_miProgram[ MAX_PATH ] = {0};
static char g_miArguments[ 128 ] = {0};

struct MIBreakpoint
{
    int number;
    uint16_t address;
    bool active;
    bool temporary;
    bool conditionErrorReported;
    unsigned long hitCount;
    unsigned long ignoreCount;
    char condition[ 256 ];
    char file[ MAX_PATH ];
    int line;
};

static MIBreakpoint * g_miBreakpoints = 0;
static int g_miNextBreakpoint = 1;
static MIBreakpoint * g_miStoppedBreakpoint = 0;

struct MIDebugLine
{
    uint16_t address;
    int line;
    char file[ MAX_PATH ];
};

static MIDebugLine * g_miDebugLines = 0;
static int g_miDebugLineCount = 0;

struct MIDebugFunction
{
    uint16_t start;
    uint16_t end;
    char name[ 64 ];
    char sourceName[ 64 ];
};

struct MIDebugVariable
{
    uint16_t declaration;
    uint16_t end;
    uint16_t address;
    bool global;
    char function[ 64 ];
    char name[ 64 ];
    int type;
    int storage;
    int offset;
    int size;
    bool isArray;
    bool isVla;
    bool isFunctionPointer;
    int elementSize;
    int dimensions[ 12 ];
    int dimensionCount;
};

struct MIDebugStruct
{
    int id;
    int size;
    bool isUnion;
    char name[ 64 ];
};

struct MIDebugField
{
    int structId;
    char name[ 64 ];
    int type;
    int offset;
    int size;
    bool isArray;
    int elementSize;
    int bitWidth;
    int bitShift;
    int dimensions[ 12 ];
    int dimensionCount;
};

static MIDebugFunction * g_miDebugFunctions = 0;
static int g_miDebugFunctionCount = 0;
static MIDebugVariable * g_miDebugVariables = 0;
static int g_miDebugVariableCount = 0;
static MIDebugStruct * g_miDebugStructs = 0;
static int g_miDebugStructCount = 0;
static MIDebugField * g_miDebugFields = 0;
static int g_miDebugFieldCount = 0;

struct MIStackFrame
{
    uint16_t pc;
    uint16_t ix;
};

static MIStackFrame * g_miStackFrames = 0;
static int g_miStackFrameCount = 0;
static int g_miSelectedFrame = 0;

struct MIVariableObject
{
    bool active;
    int frame;
    char name[ 32 ];
    char expression[ 256 ];
};

static MIVariableObject * g_miVariableObjects = 0;
static int g_miVariableObjectCapacity = 0;
static int g_miNextVariableObject = 1;

static bool mi_allocate_state()
{
    g_miBreakpoints = (MIBreakpoint *) calloc( 256, sizeof( MIBreakpoint ) );
    g_miDebugLines = (MIDebugLine *) calloc( 16384, sizeof( MIDebugLine ) );
    g_miDebugFunctions = (MIDebugFunction *) calloc( 1024, sizeof( MIDebugFunction ) );
    g_miDebugVariables = (MIDebugVariable *) calloc( 4096, sizeof( MIDebugVariable ) );
    g_miDebugStructs = (MIDebugStruct *) calloc( 256, sizeof( MIDebugStruct ) );
    g_miDebugFields = (MIDebugField *) calloc( 4096, sizeof( MIDebugField ) );
    g_miStackFrames = (MIStackFrame *) calloc( 32, sizeof( MIStackFrame ) );
    g_miVariableObjectCapacity = 256;
    g_miVariableObjects = (MIVariableObject *) calloc( (size_t) g_miVariableObjectCapacity,
                                                       sizeof( MIVariableObject ) );
    return g_miBreakpoints && g_miDebugLines && g_miDebugFunctions &&
           g_miDebugVariables && g_miDebugStructs && g_miDebugFields &&
           g_miStackFrames && g_miVariableObjects;
}

static void mi_parse_dimensions( const char * text, int * dimensions, int * count )
{
    *count = 0;
    while ( *text && *count < 12 )
    {
        char * end;
        long value = strtol( text, &end, 10 );
        if ( end == text ) break;
        dimensions[ ( *count )++ ] = (int) value;
        text = end;
        if ( *text == ',' ) text++;
        else break;
    }
}

enum MISourceStepMode
{
    mi_source_step_none,
    mi_source_step_into,
    mi_source_step_over,
    mi_source_step_out
};

static MISourceStepMode g_miSourceStepMode = mi_source_step_none;
static char g_miSourceStepFile[ MAX_PATH ] = {0};
static int g_miSourceStepLine = 0;
static uint16_t g_miSourceStepSp = 0;
static uint16_t g_miSourceStepReturnPc = 0;
static uint16_t g_miSourceStepReturnIx = 0;

static bool mi_path_suffix_equal( const char * requested, const char * recorded )
{
    const char * longer = requested;
    const char * shorter = recorded;
    size_t longerLength = strlen( longer );
    size_t shorterLength = strlen( shorter );
    size_t offset;
    size_t index;
    if ( shorterLength > longerLength )
    {
        const char * path = longer;
        size_t length = longerLength;
        longer = shorter;
        longerLength = shorterLength;
        shorter = path;
        shorterLength = length;
    }
    offset = longerLength - shorterLength;
    if ( offset && longer[ offset - 1 ] != '/' && longer[ offset - 1 ] != '\\' )
        return false;
    for ( index = 0; index < shorterLength; index++ )
    {
        char a = longer[ offset + index ];
        char b = shorter[ index ];
        if ( a == '\\' ) a = '/';
        if ( b == '\\' ) b = '/';
        if ( a != b )
            return false;
    }
    return true;
}

static void mi_load_debug_metadata()
{
    char path[ MAX_PATH ];
    char line[ 4096 ];
    FILE * file;
    char * extension;
    char * slash;
    char * backslash;
    bool lineCapacityExceeded = false;
    bool functionCapacityExceeded = false;
    bool variableCapacityExceeded = false;
    bool structCapacityExceeded = false;
    bool fieldCapacityExceeded = false;

    g_miDebugLineCount = 0;
    g_miDebugFunctionCount = 0;
    g_miDebugVariableCount = 0;
    g_miDebugStructCount = 0;
    g_miDebugFieldCount = 0;
    strncpy( path, g_miProgram, sizeof( path ) - 1 );
    path[ sizeof( path ) - 1 ] = 0;
    extension = strrchr( path, '.' );
    slash = strrchr( path, '/' );
    backslash = strrchr( path, '\\' );
    if ( !extension || ( slash && extension < slash ) || ( backslash && extension < backslash ) )
        extension = path + strlen( path );
    if ( (size_t) ( extension - path ) + 5 > sizeof( path ) )
        return;
    strcpy( extension, ".DBG" );
    file = fopen( path, "r" );
    if ( !file )
        return;
    if ( !fgets( line, sizeof( line ), file ) || strncmp( line, "DCCDBG 2", 8 ) )
    {
        fclose( file );
        return;
    }
    while ( fgets( line, sizeof( line ), file ) )
    {
        unsigned long address;
        int sourceLine;
        char * firstQuote;
        char * lastQuote;
        size_t length;
        MIDebugLine * debugLine;
        char functionName[ 64 ];
        char sourceFunctionName[ 64 ];
        char variableName[ 64 ];
        int type;
        int storage;
        int offset;
        int size;
        int isArray;
        int isVla;
        int isFunctionPointer;
        int elementSize;
        int structId;
        int isUnion;
        int bitWidth;
        int bitShift;
        int variableFields;
        char dimensions[ 128 ];
        if ( g_miDebugFunctionCount >= 1024 && !strncmp( line, "function-begin ", 15 ) )
        {
            functionCapacityExceeded = true;
            continue;
        }
        if ( g_miDebugVariableCount >= 4096 &&
             ( !strncmp( line, "variable ", 9 ) || !strncmp( line, "global ", 7 ) ) )
        {
            variableCapacityExceeded = true;
            continue;
        }
        if ( g_miDebugStructCount >= 256 && !strncmp( line, "struct ", 7 ) )
        {
            structCapacityExceeded = true;
            continue;
        }
        if ( g_miDebugFieldCount >= 4096 && !strncmp( line, "field ", 6 ) )
        {
            fieldCapacityExceeded = true;
            continue;
        }
        if ( g_miDebugLineCount >= 16384 && !strncmp( line, "line ", 5 ) )
        {
            lineCapacityExceeded = true;
            continue;
        }
        int functionFields = sscanf( line, "function-begin %lx \"%63[^\"]\" \"%63[^\"]\"",
                         &address, functionName, sourceFunctionName );
        if ( functionFields >= 2 &&
             address <= 0xffff && g_miDebugFunctionCount < 1024 )
        {
            MIDebugFunction * function = &g_miDebugFunctions[ g_miDebugFunctionCount++ ];
            function->start = (uint16_t) address;
            function->end = 0xffff;
            strcpy( function->name, functionName );
            strcpy( function->sourceName, functionFields >= 3 ? sourceFunctionName : functionName );
            continue;
        }
        if ( sscanf( line, "function-end %lx \"%63[^\"]\"", &address, functionName ) == 2 &&
             address <= 0xffff )
        {
            int index;
            for ( index = g_miDebugFunctionCount - 1; index >= 0; index-- )
                if ( !strcmp( g_miDebugFunctions[ index ].name, functionName ) )
                {
                    g_miDebugFunctions[ index ].end = (uint16_t) address;
                    break;
                }
            for ( index = 0; index < g_miDebugVariableCount; index++ )
                if ( !g_miDebugVariables[ index ].global && g_miDebugVariables[ index ].end == 0xffff &&
                     !strcmp( g_miDebugVariables[ index ].function, functionName ) )
                    g_miDebugVariables[ index ].end = (uint16_t) address;
            continue;
        }
        isFunctionPointer = 0;
        dimensions[ 0 ] = 0;
        variableFields = sscanf( line, "variable %lx \"%63[^\"]\" \"%63[^\"]\" %d %d %d %d %d %d %d %d \"%127[^\"]\"",
                     &address, functionName, variableName, &type, &storage,
                     &offset, &size, &isArray, &isVla, &elementSize,
                     &isFunctionPointer, dimensions );
        if ( variableFields < 11 )
        {
            isFunctionPointer = 0;
            dimensions[ 0 ] = 0;
            variableFields = sscanf( line, "variable %lx \"%63[^\"]\" \"%63[^\"]\" %d %d %d %d %d %d %d \"%127[^\"]\"",
                         &address, functionName, variableName, &type, &storage,
                         &offset, &size, &isArray, &isVla, &elementSize, dimensions );
        }
        if ( variableFields >= 10 && address <= 0xffff &&
             g_miDebugVariableCount < 4096 )
        {
            MIDebugVariable * variable = &g_miDebugVariables[ g_miDebugVariableCount++ ];
            variable->declaration = (uint16_t) address;
            variable->end = 0xffff;
            variable->address = 0;
            variable->global = false;
            strcpy( variable->function, functionName );
            strcpy( variable->name, variableName );
            variable->type = type;
            variable->storage = storage;
            variable->offset = offset;
            variable->size = size;
            variable->isArray = isArray != 0;
            variable->isVla = isVla != 0;
            variable->isFunctionPointer = isFunctionPointer != 0;
            variable->elementSize = elementSize;
            mi_parse_dimensions( dimensions, variable->dimensions, &variable->dimensionCount );
            continue;
        }
        if ( sscanf( line, "variable-end %lx \"%63[^\"]\" \"%63[^\"]\" %d",
                     &address, functionName, variableName, &offset ) == 4 && address <= 0xffff )
        {
            int index;
            for ( index = g_miDebugVariableCount - 1; index >= 0; index-- )
            {
                MIDebugVariable * variable = &g_miDebugVariables[ index ];
                if ( variable->end == 0xffff && variable->offset == offset &&
                     !strcmp( variable->function, functionName ) && !strcmp( variable->name, variableName ) )
                {
                    variable->end = (uint16_t) address;
                    break;
                }
            }
            continue;
        }
        isFunctionPointer = 0;
        dimensions[ 0 ] = 0;
        variableFields = sscanf( line, "global %lx \"%63[^\"]\" \"%63[^\"]\" %d %d %d %d %d %d \"%127[^\"]\"",
                     &address, functionName, variableName, &type, &size, &isArray,
                     &isVla, &elementSize, &isFunctionPointer, dimensions );
        if ( variableFields < 9 )
        {
            isFunctionPointer = 0;
            dimensions[ 0 ] = 0;
            variableFields = sscanf( line, "global %lx \"%63[^\"]\" \"%63[^\"]\" %d %d %d %d %d \"%127[^\"]\"",
                         &address, functionName, variableName, &type, &size, &isArray,
                         &isVla, &elementSize, dimensions );
        }
        if ( variableFields >= 8 && address <= 0xffff &&
             g_miDebugVariableCount < 4096 )
        {
            MIDebugVariable * variable = &g_miDebugVariables[ g_miDebugVariableCount++ ];
            memset( variable, 0, sizeof( *variable ) );
            variable->address = (uint16_t) address;
            variable->global = true;
            strcpy( variable->function, "<global>" );
            strcpy( variable->name, variableName );
            variable->type = type;
            variable->storage = 1;
            variable->size = size;
            variable->isArray = isArray != 0;
            variable->isVla = isVla != 0;
            variable->isFunctionPointer = isFunctionPointer != 0;
            variable->elementSize = elementSize;
            mi_parse_dimensions( dimensions, variable->dimensions, &variable->dimensionCount );
            continue;
        }
        if ( sscanf( line, "struct %d %d %d \"%63[^\"]\"", &structId, &size,
                     &isUnion, variableName ) == 4 && g_miDebugStructCount < 256 )
        {
            MIDebugStruct * debugStruct = &g_miDebugStructs[ g_miDebugStructCount++ ];
            debugStruct->id = structId;
            debugStruct->size = size;
            debugStruct->isUnion = isUnion != 0;
            strcpy( debugStruct->name, variableName );
            continue;
        }
        if ( sscanf( line, "field %d \"%63[^\"]\" %d %d %d %d %d %d %d \"%127[^\"]\"",
                     &structId, variableName, &type, &offset, &size, &isArray,
                     &elementSize, &bitWidth, &bitShift, dimensions ) >= 9 &&
             g_miDebugFieldCount < 4096 )
        {
            MIDebugField * field = &g_miDebugFields[ g_miDebugFieldCount++ ];
            field->structId = structId;
            strcpy( field->name, variableName );
            field->type = type;
            field->offset = offset;
            field->size = size;
            field->isArray = isArray != 0;
            field->elementSize = elementSize;
            field->bitWidth = bitWidth;
            field->bitShift = bitShift;
            mi_parse_dimensions( dimensions, field->dimensions, &field->dimensionCount );
            continue;
        }
        if ( sscanf( line, "line %lx %d", &address, &sourceLine ) != 2 || address > 0xffff )
            continue;
        firstQuote = strchr( line, '"' );
        lastQuote = strrchr( line, '"' );
        if ( !firstQuote || lastQuote == firstQuote )
            continue;
        if ( g_miDebugLineCount >= 16384 )
            continue;
        debugLine = &g_miDebugLines[ g_miDebugLineCount++ ];
        debugLine->address = (uint16_t) address;
        debugLine->line = sourceLine;
        length = (size_t) ( lastQuote - firstQuote - 1 );
        if ( length >= sizeof( debugLine->file ) )
            length = sizeof( debugLine->file ) - 1;
        memcpy( debugLine->file, firstQuote + 1, length );
        debugLine->file[ length ] = 0;
    }
    fclose( file );
    if ( lineCapacityExceeded )
        printf( "~\"warning: DCC debug line metadata capacity exceeded; later records were ignored\\n\"\n" );
    if ( functionCapacityExceeded )
        printf( "~\"warning: DCC debug function metadata capacity exceeded; later records were ignored\\n\"\n" );
    if ( variableCapacityExceeded )
        printf( "~\"warning: DCC debug variable metadata capacity exceeded; later records were ignored\\n\"\n" );
    if ( structCapacityExceeded )
        printf( "~\"warning: DCC debug struct metadata capacity exceeded; later records were ignored\\n\"\n" );
    if ( fieldCapacityExceeded )
        printf( "~\"warning: DCC debug field metadata capacity exceeded; later records were ignored\\n\"\n" );
    if ( lineCapacityExceeded || functionCapacityExceeded || variableCapacityExceeded ||
         structCapacityExceeded || fieldCapacityExceeded )
        fflush( stdout );
}

static MIDebugFunction * mi_find_debug_function( uint16_t address )
{
    MIDebugFunction * best = NULL;
    int index;
    for ( index = 0; index < g_miDebugFunctionCount; index++ )
    {
        MIDebugFunction * function = &g_miDebugFunctions[ index ];
        if ( address < function->start || address >= function->end )
            continue;
        if ( !best || function->start > best->start )
            best = function;
    }
    return best;
}

static MIDebugFunction * mi_find_debug_function_name( const char * name )
{
    int index;
    for ( index = 0; index < g_miDebugFunctionCount; index++ )
        if ( !strcmp( g_miDebugFunctions[ index ].name, name ) ||
             !strcmp( g_miDebugFunctions[ index ].sourceName, name ) )
            return &g_miDebugFunctions[ index ];
    return NULL;
}

static uint16_t mi_context_pc()
{
    return g_miSelectedFrame >= 0 && g_miSelectedFrame < g_miStackFrameCount ?
           g_miStackFrames[ g_miSelectedFrame ].pc : reg.pc;
}

static uint16_t mi_context_ix()
{
    return g_miSelectedFrame >= 0 && g_miSelectedFrame < g_miStackFrameCount ?
           g_miStackFrames[ g_miSelectedFrame ].ix : reg.ix;
}

static MIDebugVariable * mi_find_debug_variable( const char * name )
{
    uint16_t pc = mi_context_pc();
    MIDebugFunction * function = mi_find_debug_function( pc );
    MIDebugVariable * best = NULL;
    int index;
    if ( function ) for ( index = 0; index < g_miDebugVariableCount; index++ )
    {
        MIDebugVariable * variable = &g_miDebugVariables[ index ];
        if ( strcmp( variable->function, function->name ) || strcmp( variable->name, name ) ||
               variable->declaration > pc || pc >= variable->end )
            continue;
        if ( !best || variable->declaration >= best->declaration )
            best = variable;
    }
    if ( best ) return best;
    for ( index = 0; index < g_miDebugVariableCount; index++ )
        if ( g_miDebugVariables[ index ].global && !strcmp( g_miDebugVariables[ index ].name, name ) )
            return &g_miDebugVariables[ index ];
    return best;
}

struct MIDebugValue
{
    uint16_t address;
    int type;
    int size;
    bool immediate;
    unsigned long immediateValue;
    bool isArray;
    bool isFunctionPointer;
    int elementSize;
    int dimensions[ 12 ];
    int dimensionCount;
    int bitWidth;
    int bitShift;
};

static uint16_t mi_read_word( uint16_t address )
{
    return (uint16_t) ( memory[ address ] | ( (uint16_t) memory[ (uint16_t) ( address + 1 ) ] << 8 ) );
}

static void mi_rebuild_stack_frames()
{
    uint16_t frameIx = reg.ix;
    uint16_t framePc = reg.pc;
    g_miStackFrameCount = 0;
    g_miSelectedFrame = 0;
    while ( g_miStackFrameCount < 32 )
    {
        uint16_t callerIx;
        uint16_t returnPc;
        g_miStackFrames[ g_miStackFrameCount ].ix = frameIx;
        g_miStackFrames[ g_miStackFrameCount ].pc = framePc;
        g_miStackFrameCount++;
        if ( frameIx > 0xfffb ) break;
        callerIx = mi_read_word( frameIx );
        returnPc = mi_read_word( (uint16_t) ( frameIx + 2 ) );
        if ( callerIx <= frameIx || returnPc == 0 ) break;
        framePc = (uint16_t) ( returnPc - 1 );
        if ( !mi_find_debug_function( framePc ) ) break;
        frameIx = callerIx;
    }
}

static MIDebugStruct * mi_find_struct( int id )
{
    int index;
    for ( index = 0; index < g_miDebugStructCount; index++ )
        if ( g_miDebugStructs[ index ].id == id ) return &g_miDebugStructs[ index ];
    return NULL;
}

static MIDebugField * mi_find_field( int structId, const char * name )
{
    int index;
    for ( index = 0; index < g_miDebugFieldCount; index++ )
        if ( g_miDebugFields[ index ].structId == structId &&
             !strcmp( g_miDebugFields[ index ].name, name ) ) return &g_miDebugFields[ index ];
    return NULL;
}

static int mi_type_size( int type )
{
    MIDebugStruct * debugStruct;
    int base = type & 15;
    if ( type & ( 16 | 64 ) ) return 2;
    if ( type & 128 )
    {
        debugStruct = mi_find_struct( type >> 8 );
        return debugStruct ? debugStruct->size : 0;
    }
    if ( base == 1 || base == 6 ) return 1;
    if ( base == 4 || base == 5 ) return 4;
    return 2;
}

static int mi_decay_pointer_type( int type )
{
    if ( type & 64 ) return type & ~64;
    if ( type & 16 ) return type & ~16;
    return type;
}

static const char * mi_type_name( int type, bool isArray, const int * dimensions, int dimensionCount,
                                  bool isFunctionPointer = false )
{
    static char typeName[ 128 ];
    MIDebugStruct * debugStruct;
    int pointerDepth = ( type & 16 ? 1 : 0 ) + ( type & 64 ? 1 : 0 );
    int valueType = type & ~( 16 | 64 );
    int base = valueType & 15;
    bool isUnsigned = ( type & 32 ) != 0;
    int index;
    if ( valueType & 128 )
    {
        debugStruct = mi_find_struct( valueType >> 8 );
        snprintf( typeName, sizeof( typeName ), "%s %s",
                  debugStruct && debugStruct->isUnion ? "union" : "struct",
                  debugStruct && debugStruct->name[ 0 ] ? debugStruct->name : "<anonymous>" );
    }
    else if ( base == 1 ) strcpy( typeName, isUnsigned ? "unsigned char" : "char" );
    else if ( base == 2 ) strcpy( typeName, isUnsigned ? "unsigned int" : "int" );
    else if ( base == 4 ) strcpy( typeName, isUnsigned ? "unsigned long" : "long" );
    else if ( base == 5 ) strcpy( typeName, "float" );
    else if ( base == 6 ) strcpy( typeName, "_Bool" );
    else strcpy( typeName, "value" );
    if ( isFunctionPointer )
        strcat( typeName, " (*)()" );
    else
        for ( index = 0; index < pointerDepth && strlen( typeName ) + 3 < sizeof( typeName ); index++ )
            strcat( typeName, " *" );
    if ( isArray )
        for ( index = 0; index < dimensionCount && strlen( typeName ) + 16 < sizeof( typeName ); index++ )
            snprintf( typeName + strlen( typeName ), sizeof( typeName ) - strlen( typeName ),
                      "[%d]", dimensions[ index ] );
    return typeName;
}

static const char * mi_variable_type_name( const MIDebugVariable * variable )
{
    return mi_type_name( variable->type, variable->isArray,
                         variable->dimensions, variable->dimensionCount,
                         variable->isFunctionPointer );
}

static void mi_value_from_variable( const MIDebugVariable * variable, MIDebugValue * value )
{
    int index;
    memset( value, 0, sizeof( *value ) );
    value->address = variable->global ? variable->address :
                     (uint16_t) ( mi_context_ix() + variable->offset );
    if ( variable->isVla ) value->address = mi_read_word( value->address );
    value->type = variable->type;
    value->size = variable->size;
    value->isArray = variable->isArray;
    value->isFunctionPointer = variable->isFunctionPointer;
    value->elementSize = variable->elementSize;
    value->dimensionCount = variable->dimensionCount;
    for ( index = 0; index < variable->dimensionCount; index++ )
        value->dimensions[ index ] = variable->dimensions[ index ];
}

    static bool mi_evaluate_integer_expression( const char * expression, unsigned long * result,
                                                int * valueSize = NULL, bool * valueUnsigned = NULL );

static bool mi_resolve_debug_value( const char * expression, MIDebugValue * value )
{
    char name[ 64 ];
    int length = 0;
    MIDebugVariable * variable;
    while ( isspace( (unsigned char) *expression ) ) expression++;
    if ( *expression == '*' )
    {
        expression++;
        if ( !mi_resolve_debug_value( expression, value ) || !( value->type & ( 16 | 64 ) ) ) return false;
        value->address = mi_read_word( value->address );
        value->type = mi_decay_pointer_type( value->type );
        value->size = mi_type_size( value->type );
        value->isArray = false;
        value->dimensionCount = 0;
        return true;
    }
    while ( ( isalnum( (unsigned char) *expression ) || *expression == '_' ) && length + 1 < (int) sizeof( name ) )
        name[ length++ ] = *expression++;
    name[ length ] = 0;
    variable = mi_find_debug_variable( name );
    if ( !variable ) return false;
    mi_value_from_variable( variable, value );
    for (;;)
    {
        while ( isspace( (unsigned char) *expression ) ) expression++;
        if ( *expression == '[' )
        {
            const char * indexStart;
            const char * cursor;
            char indexExpression[ 256 ];
            unsigned long element;
            int bracketDepth = 1;
            int stride;
            int index;
            expression++;
            indexStart = expression;
            cursor = expression;
            while ( *cursor && bracketDepth )
            {
                if ( *cursor == '[' ) bracketDepth++;
                else if ( *cursor == ']' ) bracketDepth--;
                if ( bracketDepth ) cursor++;
            }
            if ( bracketDepth || cursor == indexStart ||
                 (size_t) ( cursor - indexStart ) >= sizeof( indexExpression ) ) return false;
            memcpy( indexExpression, indexStart, (size_t) ( cursor - indexStart ) );
            indexExpression[ cursor - indexStart ] = 0;
            if ( !mi_evaluate_integer_expression( indexExpression, &element ) ) return false;
            expression = cursor + 1;
            if ( value->isArray && value->dimensionCount > 0 )
            {
                if ( element >= (unsigned long) value->dimensions[ 0 ] ) return false;
                stride = value->dimensions[ 0 ] ? value->size / value->dimensions[ 0 ] : value->elementSize;
                value->address = (uint16_t) ( value->address + element * stride );
                value->size = stride;
                for ( index = 1; index < value->dimensionCount; index++ )
                    value->dimensions[ index - 1 ] = value->dimensions[ index ];
                value->dimensionCount--;
                value->isArray = value->dimensionCount > 0;
                if ( !value->isArray ) value->size = value->elementSize;
            }
            else if ( value->type & ( 16 | 64 ) )
            {
                value->type = mi_decay_pointer_type( value->type );
                stride = mi_type_size( value->type );
                value->address = (uint16_t) ( mi_read_word( value->address ) + element * stride );
                value->size = stride;
            }
            else return false;
        }
        else if ( *expression == '.' || ( expression[ 0 ] == '-' && expression[ 1 ] == '>' ) )
        {
            MIDebugField * field;
            bool indirect = *expression == '-';
            int structId;
            int index;
            expression += indirect ? 2 : 1;
            length = 0;
            while ( ( isalnum( (unsigned char) *expression ) || *expression == '_' ) && length + 1 < (int) sizeof( name ) )
                name[ length++ ] = *expression++;
            name[ length ] = 0;
            if ( indirect )
            {
                if ( !( value->type & ( 16 | 64 ) ) ) return false;
                value->address = mi_read_word( value->address );
                value->type = mi_decay_pointer_type( value->type );
            }
            if ( !( value->type & 128 ) ) return false;
            structId = value->type >> 8;
            field = mi_find_field( structId, name );
            if ( !field ) return false;
            value->address = (uint16_t) ( value->address + field->offset );
            value->type = field->type;
            value->size = field->size;
            value->isArray = field->isArray;
            value->elementSize = field->elementSize;
            value->bitWidth = field->bitWidth;
            value->bitShift = field->bitShift;
            value->dimensionCount = field->dimensionCount;
            for ( index = 0; index < field->dimensionCount; index++ ) value->dimensions[ index ] = field->dimensions[ index ];
        }
        else break;
    }
    while ( isspace( (unsigned char) *expression ) ) expression++;
    return *expression == 0;
}

struct MIExpressionParser
{
    const char * text;
    bool ok;
    int valueSize;
    bool valueUnsigned;
};

static unsigned long mi_expression_mask( int size )
{
    return size == 1 ? 0xffUL : ( size == 2 ? 0xffffUL : 0xffffffffUL );
}

static unsigned long mi_expression_normalize( unsigned long value, int size, bool isUnsigned )
{
    value &= mi_expression_mask( size );
    if ( isUnsigned ) return value;
    if ( size == 1 ) return (unsigned long) (long) (int8_t) value;
    if ( size == 2 ) return (unsigned long) (long) (int16_t) value;
    return (unsigned long) (int64_t) (int32_t) value;
}

static int64_t mi_expression_signed( unsigned long value, int size )
{
    if ( size == 1 ) return (int8_t) value;
    if ( size == 2 ) return (int16_t) value;
    return (int32_t) value;
}

static void mi_expression_promote( int * size, bool * isUnsigned )
{
    if ( *size < 2 )
    {
        *size = 2;
        *isUnsigned = false;
    }
}

static void mi_expression_common_type( int leftSize, bool leftUnsigned,
                                       int rightSize, bool rightUnsigned,
                                       int * size, bool * isUnsigned )
{
    mi_expression_promote( &leftSize, &leftUnsigned );
    mi_expression_promote( &rightSize, &rightUnsigned );
    if ( leftSize == rightSize )
    {
        *size = leftSize;
        *isUnsigned = leftUnsigned || rightUnsigned;
    }
    else if ( leftSize > rightSize )
    {
        *size = leftSize;
        *isUnsigned = leftUnsigned;
    }
    else
    {
        *size = rightSize;
        *isUnsigned = rightUnsigned;
    }
}

static void mi_expression_set_int( MIExpressionParser * parser )
{
    parser->valueSize = 2;
    parser->valueUnsigned = false;
}

static void mi_expression_space( MIExpressionParser * parser )
{
    while ( isspace( (unsigned char) *parser->text ) ) parser->text++;
}

static unsigned long mi_debug_value_integer( const MIDebugValue * value )
{
    unsigned long result;
    unsigned long fieldMask;
    if ( value->immediate ) return value->immediateValue;
    result = memory[ value->address ];
    if ( value->size > 1 ) result |= (unsigned long) memory[ (uint16_t) ( value->address + 1 ) ] << 8;
    if ( value->size > 2 )
    {
        result |= (unsigned long) memory[ (uint16_t) ( value->address + 2 ) ] << 16;
        result |= (unsigned long) memory[ (uint16_t) ( value->address + 3 ) ] << 24;
    }
    if ( value->bitWidth > 0 )
    {
        fieldMask = ( 1UL << value->bitWidth ) - 1;
        result = ( result >> value->bitShift ) & fieldMask;
        if ( !( value->type & 32 ) && ( result & ( 1UL << ( value->bitWidth - 1 ) ) ) )
            result |= ~fieldMask;
    }
    else if ( !( value->type & 32 ) && value->size == 1 ) result = (unsigned long) (long) (signed char) result;
    else if ( !( value->type & 32 ) && value->size == 2 ) result = (unsigned long) (long) (short) result;
    return result;
}

static unsigned long mi_parse_comma( MIExpressionParser * parser );

static unsigned long mi_parse_primary( MIExpressionParser * parser )
{
    unsigned long result = 0;
    mi_expression_space( parser );
    if ( *parser->text == '(' )
    {
        parser->text++;
        result = mi_parse_comma( parser );
        mi_expression_space( parser );
        if ( *parser->text != ')' ) parser->ok = false;
        else parser->text++;
        return result;
    }
    if ( isdigit( (unsigned char) *parser->text ) )
    {
        const char * literal = parser->text;
        char * end;
        bool isUnsigned = false;
        bool isLong = false;
        bool decimal;
        result = strtoul( parser->text, &end, 0 );
        if ( end == parser->text ) parser->ok = false;
        parser->text = end;
        while ( *parser->text == 'u' || *parser->text == 'U' || *parser->text == 'l' || *parser->text == 'L' )
        {
            if ( *parser->text == 'u' || *parser->text == 'U' ) isUnsigned = true;
            else isLong = true;
            parser->text++;
        }
        decimal = literal[ 0 ] != '0';
        if ( isUnsigned )
        {
            parser->valueSize = isLong || result > 0xffffUL ? 4 : 2;
        }
        else if ( isLong )
        {
            parser->valueSize = 4;
            isUnsigned = result > 0x7fffffffUL;
        }
        else if ( decimal )
        {
            parser->valueSize = result <= 0x7fffUL ? 2 : 4;
            isUnsigned = result > 0x7fffffffUL;
        }
        else if ( result <= 0x7fffUL )
            parser->valueSize = 2;
        else if ( result <= 0xffffUL )
        {
            parser->valueSize = 2;
            isUnsigned = true;
        }
        else
        {
            parser->valueSize = 4;
            isUnsigned = result > 0x7fffffffUL;
        }
        parser->valueUnsigned = isUnsigned;
        return mi_expression_normalize( result, parser->valueSize, parser->valueUnsigned );
    }
    if ( *parser->text == '\'' )
    {
        parser->text++;
        if ( *parser->text == '\\' )
        {
            parser->text++;
            if ( *parser->text == 'n' ) result = '\n';
            else if ( *parser->text == 'r' ) result = '\r';
            else if ( *parser->text == 't' ) result = '\t';
            else if ( *parser->text == '0' ) result = 0;
            else result = (unsigned char) *parser->text;
        }
        else result = (unsigned char) *parser->text;
        if ( !*parser->text ) parser->ok = false;
        else parser->text++;
        if ( *parser->text != '\'' ) parser->ok = false;
        else parser->text++;
        mi_expression_set_int( parser );
        return result;
    }
    if ( isalpha( (unsigned char) *parser->text ) || *parser->text == '_' || *parser->text == '*' )
    {
        const char * start = parser->text;
        const char * cursor = parser->text;
        char expression[ 256 ];
        int bracketDepth = 0;
        MIDebugValue value;
        while ( *cursor == '*' ) cursor++;
        while ( *cursor )
        {
            if ( *cursor == '[' ) bracketDepth++;
            else if ( *cursor == ']' ) bracketDepth--;
            else if ( bracketDepth == 0 &&
                      ( isspace( (unsigned char) *cursor ) || strchr( "+*/%<>=!&|^?:,()", *cursor ) ||
                        ( *cursor == '-' && cursor[ 1 ] != '>' ) ) ) break;
            cursor++;
        }
        if ( cursor == start || (size_t) ( cursor - start ) >= sizeof( expression ) )
        {
            parser->ok = false;
            return 0;
        }
        memcpy( expression, start, (size_t) ( cursor - start ) );
        expression[ cursor - start ] = 0;
        if ( !mi_resolve_debug_value( expression, &value ) || value.isArray || ( value.type & 128 ) )
        {
            parser->ok = false;
            return 0;
        }
        if ( ( value.type & 15 ) == 5 )
        {
            parser->ok = false;
            return 0;
        }
        parser->text = cursor;
        parser->valueSize = value.type & ( 16 | 64 ) ? 2 : value.size;
        parser->valueUnsigned = ( value.type & ( 16 | 64 | 32 ) ) != 0;
        return mi_debug_value_integer( &value );
    }
    parser->ok = false;
    return 0;
}

static unsigned long mi_parse_unary( MIExpressionParser * parser )
{
    static const struct
    {
        const char * text;
        int size;
        bool isUnsigned;
        bool isBoolean;
    } casts[] = {
        { "(unsigned char)", 1, true, false }, { "(char)", 1, false, false },
        { "(unsigned int)", 2, true, false }, { "(unsigned)", 2, true, false },
        { "(int)", 2, false, false }, { "(short)", 2, false, false },
        { "(unsigned long)", 4, true, false }, { "(long)", 4, false, false },
        { "(_Bool)", 1, true, true }
    };
    int castIndex;
    mi_expression_space( parser );
    if ( !strncmp( parser->text, "sizeof", 6 ) &&
         !isalnum( (unsigned char) parser->text[ 6 ] ) && parser->text[ 6 ] != '_' )
    {
        char operand[ 256 ];
        const char * start;
        const char * cursor;
        int depth = 0;
        MIDebugValue value;
        parser->text += 6;
        mi_expression_space( parser );
        if ( *parser->text != '(' ) { parser->ok = false; return 0; }
        start = ++parser->text;
        cursor = start;
        do
        {
            if ( *cursor == '(' ) depth++;
            else if ( *cursor == ')' )
            {
                if ( depth == 0 ) break;
                depth--;
            }
            if ( *cursor ) cursor++;
        } while ( *cursor );
        if ( *cursor != ')' || cursor == start ||
             (size_t) ( cursor - start ) >= sizeof( operand ) )
        {
            parser->ok = false;
            return 0;
        }
        memcpy( operand, start, (size_t) ( cursor - start ) );
        operand[ cursor - start ] = 0;
        parser->text = cursor + 1;
        parser->valueSize = 2;
        parser->valueUnsigned = true;
        if ( mi_resolve_debug_value( operand, &value ) ) return (unsigned long) value.size;
        if ( strchr( operand, '*' ) ) return 2;
        if ( strstr( operand, "char" ) || strstr( operand, "_Bool" ) ) return 1;
        if ( strstr( operand, "long" ) || strstr( operand, "float" ) ) return 4;
        if ( strstr( operand, "int" ) || strstr( operand, "short" ) ||
             !strcmp( operand, "unsigned" ) || !strcmp( operand, "signed" ) ) return 2;
        parser->ok = false;
        return 0;
    }
    for ( castIndex = 0; castIndex < (int) ( sizeof( casts ) / sizeof( casts[ 0 ] ) ); castIndex++ )
    {
        size_t castLength = strlen( casts[ castIndex ].text );
        unsigned long result;
        if ( strncmp( parser->text, casts[ castIndex ].text, castLength ) ) continue;
        parser->text += castLength;
        result = mi_parse_unary( parser );
        parser->valueSize = casts[ castIndex ].size;
        parser->valueUnsigned = casts[ castIndex ].isUnsigned;
        if ( casts[ castIndex ].isBoolean ) return result != 0;
        return mi_expression_normalize( result, parser->valueSize, parser->valueUnsigned );
    }
    if ( *parser->text == '+' )
    {
        unsigned long result;
        parser->text++;
        result = mi_parse_unary( parser );
        mi_expression_promote( &parser->valueSize, &parser->valueUnsigned );
        return mi_expression_normalize( result, parser->valueSize, parser->valueUnsigned );
    }
    if ( *parser->text == '-' && parser->text[ 1 ] != '>' )
    {
        unsigned long result;
        parser->text++;
        result = mi_parse_unary( parser );
        mi_expression_promote( &parser->valueSize, &parser->valueUnsigned );
        return mi_expression_normalize( 0UL - result, parser->valueSize, parser->valueUnsigned );
    }
    if ( *parser->text == '!' )
    {
        unsigned long result;
        parser->text++;
        result = !mi_parse_unary( parser );
        mi_expression_set_int( parser );
        return result;
    }
    if ( *parser->text == '~' )
    {
        unsigned long result;
        parser->text++;
        result = mi_parse_unary( parser );
        mi_expression_promote( &parser->valueSize, &parser->valueUnsigned );
        return mi_expression_normalize( ~result, parser->valueSize, parser->valueUnsigned );
    }
    return mi_parse_primary( parser );
}

static unsigned long mi_parse_multiplicative( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_unary( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        char operation;
        unsigned long right;
        int rightSize;
        bool rightUnsigned;
        int commonSize;
        bool commonUnsigned;
        mi_expression_space( parser );
        operation = *parser->text;
        if ( operation != '*' && operation != '/' && operation != '%' ) break;
        parser->text++;
        right = mi_parse_unary( parser );
        rightSize = parser->valueSize;
        rightUnsigned = parser->valueUnsigned;
        mi_expression_common_type( leftSize, leftUnsigned, rightSize, rightUnsigned,
                                   &commonSize, &commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        right = mi_expression_normalize( right, commonSize, commonUnsigned );
        if ( ( operation == '/' || operation == '%' ) && right == 0 ) { parser->ok = false; return 0; }
        if ( operation == '*' ) left *= right;
        else if ( commonUnsigned ) left = operation == '/' ? left / right : left % right;
        else if ( operation == '/' )
            left = (unsigned long) ( mi_expression_signed( left, commonSize ) /
                                     mi_expression_signed( right, commonSize ) );
        else
            left = (unsigned long) ( mi_expression_signed( left, commonSize ) %
                                     mi_expression_signed( right, commonSize ) );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        leftSize = commonSize;
        leftUnsigned = commonUnsigned;
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_additive( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_multiplicative( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        char operation;
        unsigned long right;
        int rightSize;
        bool rightUnsigned;
        int commonSize;
        bool commonUnsigned;
        mi_expression_space( parser );
        operation = *parser->text;
        if ( operation != '+' && ( operation != '-' || parser->text[1] == '>' ) ) break;
        parser->text++;
        right = mi_parse_multiplicative( parser );
        rightSize = parser->valueSize;
        rightUnsigned = parser->valueUnsigned;
        mi_expression_common_type( leftSize, leftUnsigned, rightSize, rightUnsigned,
                                   &commonSize, &commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        right = mi_expression_normalize( right, commonSize, commonUnsigned );
        left = operation == '+' ? left + right : left - right;
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        leftSize = commonSize;
        leftUnsigned = commonUnsigned;
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_shift( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_additive( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        unsigned long right;
        mi_expression_space( parser );
        if ( strncmp( parser->text, "<<", 2 ) && strncmp( parser->text, ">>", 2 ) ) break;
        bool leftShift = parser->text[0] == '<';
        mi_expression_promote( &leftSize, &leftUnsigned );
        left = mi_expression_normalize( left, leftSize, leftUnsigned );
        parser->text += 2;
        right = mi_parse_additive( parser );
        if ( right >= (unsigned long) ( leftSize * 8 ) ) { parser->ok = false; return 0; }
        if ( leftShift ) left <<= right;
        else if ( leftUnsigned ) left >>= right;
        else left = (unsigned long) ( mi_expression_signed( left, leftSize ) >> right );
        left = mi_expression_normalize( left, leftSize, leftUnsigned );
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_relational( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_shift( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        unsigned long right;
        int rightSize;
        bool rightUnsigned;
        int commonSize;
        bool commonUnsigned;
        int operation = 0;
        mi_expression_space( parser );
        if ( !strncmp( parser->text, "<=", 2 ) ) operation = 1;
        else if ( !strncmp( parser->text, ">=", 2 ) ) operation = 2;
        else if ( *parser->text == '<' && parser->text[1] != '<' ) operation = 3;
        else if ( *parser->text == '>' && parser->text[1] != '>' ) operation = 4;
        else break;
        parser->text += operation <= 2 ? 2 : 1;
        right = mi_parse_shift( parser );
        rightSize = parser->valueSize;
        rightUnsigned = parser->valueUnsigned;
        mi_expression_common_type( leftSize, leftUnsigned, rightSize, rightUnsigned,
                                   &commonSize, &commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        right = mi_expression_normalize( right, commonSize, commonUnsigned );
        if ( commonUnsigned )
        {
            if ( operation == 1 ) left = left <= right;
            else if ( operation == 2 ) left = left >= right;
            else if ( operation == 3 ) left = left < right;
            else left = left > right;
        }
        else
        {
            int64_t signedLeft = mi_expression_signed( left, commonSize );
            int64_t signedRight = mi_expression_signed( right, commonSize );
            if ( operation == 1 ) left = signedLeft <= signedRight;
            else if ( operation == 2 ) left = signedLeft >= signedRight;
            else if ( operation == 3 ) left = signedLeft < signedRight;
            else left = signedLeft > signedRight;
        }
        leftSize = 2;
        leftUnsigned = false;
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_equality( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_relational( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        bool equal;
        unsigned long right;
        int rightSize;
        bool rightUnsigned;
        int commonSize;
        bool commonUnsigned;
        mi_expression_space( parser );
        if ( strncmp( parser->text, "==", 2 ) && strncmp( parser->text, "!=", 2 ) ) break;
        equal = parser->text[0] == '=';
        parser->text += 2;
        right = mi_parse_relational( parser );
        rightSize = parser->valueSize;
        rightUnsigned = parser->valueUnsigned;
        mi_expression_common_type( leftSize, leftUnsigned, rightSize, rightUnsigned,
                                   &commonSize, &commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        right = mi_expression_normalize( right, commonSize, commonUnsigned );
        left = equal ? left == right : left != right;
        leftSize = 2;
        leftUnsigned = false;
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_bit_and( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_equality( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        unsigned long right;
        int rightSize;
        bool rightUnsigned;
        int commonSize;
        bool commonUnsigned;
        mi_expression_space( parser );
        if ( *parser->text != '&' || parser->text[1] == '&' ) break;
        parser->text++;
        right = mi_parse_equality( parser );
        rightSize = parser->valueSize;
        rightUnsigned = parser->valueUnsigned;
        mi_expression_common_type( leftSize, leftUnsigned, rightSize, rightUnsigned,
                                   &commonSize, &commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned ) &
               mi_expression_normalize( right, commonSize, commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        leftSize = commonSize;
        leftUnsigned = commonUnsigned;
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_bit_xor( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_bit_and( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        unsigned long right;
        int rightSize;
        bool rightUnsigned;
        int commonSize;
        bool commonUnsigned;
        mi_expression_space( parser );
        if ( *parser->text != '^' ) break;
        parser->text++;
        right = mi_parse_bit_and( parser );
        rightSize = parser->valueSize;
        rightUnsigned = parser->valueUnsigned;
        mi_expression_common_type( leftSize, leftUnsigned, rightSize, rightUnsigned,
                                   &commonSize, &commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned ) ^
               mi_expression_normalize( right, commonSize, commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        leftSize = commonSize;
        leftUnsigned = commonUnsigned;
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_bit_or( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_bit_xor( parser );
    int leftSize = parser->valueSize;
    bool leftUnsigned = parser->valueUnsigned;
    for (;;)
    {
        unsigned long right;
        int rightSize;
        bool rightUnsigned;
        int commonSize;
        bool commonUnsigned;
        mi_expression_space( parser );
        if ( *parser->text != '|' || parser->text[1] == '|' ) break;
        parser->text++;
        right = mi_parse_bit_xor( parser );
        rightSize = parser->valueSize;
        rightUnsigned = parser->valueUnsigned;
        mi_expression_common_type( leftSize, leftUnsigned, rightSize, rightUnsigned,
                                   &commonSize, &commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned ) |
               mi_expression_normalize( right, commonSize, commonUnsigned );
        left = mi_expression_normalize( left, commonSize, commonUnsigned );
        leftSize = commonSize;
        leftUnsigned = commonUnsigned;
    }
    parser->valueSize = leftSize;
    parser->valueUnsigned = leftUnsigned;
    return left;
}

static unsigned long mi_parse_logical_and( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_bit_or( parser );
    while ( ( mi_expression_space( parser ), !strncmp( parser->text, "&&", 2 ) ) )
    {
        unsigned long right;
        parser->text += 2;
        right = mi_parse_bit_or( parser );
        left = left != 0 && right != 0;
        mi_expression_set_int( parser );
    }
    return left;
}

static unsigned long mi_parse_logical_or( MIExpressionParser * parser )
{
    unsigned long left = mi_parse_logical_and( parser );
    while ( ( mi_expression_space( parser ), !strncmp( parser->text, "||", 2 ) ) )
    {
        unsigned long right;
        parser->text += 2;
        right = mi_parse_logical_and( parser );
        left = left != 0 || right != 0;
        mi_expression_set_int( parser );
    }
    return left;
}

static unsigned long mi_parse_conditional( MIExpressionParser * parser )
{
    unsigned long condition = mi_parse_logical_or( parser );
    unsigned long whenTrue;
    unsigned long whenFalse;
    int trueSize;
    bool trueUnsigned;
    int falseSize;
    bool falseUnsigned;
    int commonSize;
    bool commonUnsigned;
    mi_expression_space( parser );
    if ( *parser->text != '?' ) return condition;
    parser->text++;
    whenTrue = mi_parse_comma( parser );
    trueSize = parser->valueSize;
    trueUnsigned = parser->valueUnsigned;
    mi_expression_space( parser );
    if ( *parser->text != ':' )
    {
        parser->ok = false;
        return 0;
    }
    parser->text++;
    whenFalse = mi_parse_conditional( parser );
    falseSize = parser->valueSize;
    falseUnsigned = parser->valueUnsigned;
    mi_expression_common_type( trueSize, trueUnsigned, falseSize, falseUnsigned,
                               &commonSize, &commonUnsigned );
    parser->valueSize = commonSize;
    parser->valueUnsigned = commonUnsigned;
    return mi_expression_normalize( condition ? whenTrue : whenFalse,
                                    commonSize, commonUnsigned );
}

static unsigned long mi_parse_comma( MIExpressionParser * parser )
{
    unsigned long result = mi_parse_conditional( parser );
    while ( ( mi_expression_space( parser ), *parser->text == ',' ) )
    {
        parser->text++;
        result = mi_parse_conditional( parser );
    }
    return result;
}

static bool mi_evaluate_integer_expression( const char * expression, unsigned long * result,
                                            int * valueSize, bool * valueUnsigned )
{
    MIExpressionParser parser;
    parser.text = expression;
    parser.ok = true;
    parser.valueSize = 2;
    parser.valueUnsigned = false;
    *result = mi_parse_comma( &parser );
    mi_expression_space( &parser );
    if ( valueSize ) *valueSize = parser.valueSize;
    if ( valueUnsigned ) *valueUnsigned = parser.valueUnsigned;
    return parser.ok && *parser.text == 0;
}

static bool mi_resolve_or_evaluate( const char * expression, MIDebugValue * value )
{
    size_t length;
    bool valueUnsigned;
    if ( mi_resolve_debug_value( expression, value ) ) return true;
    length = strlen( expression );
    if ( length >= 2 && expression[ 0 ] == '&' )
    {
        char operand[ 256 ];
        const char * operandStart = expression + 1;
        size_t operandLength = length - 1;
        while ( operandLength && isspace( (unsigned char) *operandStart ) )
        {
            operandStart++;
            operandLength--;
        }
        if ( operandLength >= 2 && operandStart[ 0 ] == '(' && operandStart[ operandLength - 1 ] == ')' )
        {
            operandStart++;
            operandLength -= 2;
        }
        if ( operandLength >= sizeof( operand ) ) return false;
        memcpy( operand, operandStart, operandLength );
        operand[ operandLength ] = 0;
        if ( !mi_resolve_debug_value( operand, value ) ) return false;
        value->immediate = true;
        value->immediateValue = value->address;
        value->type = value->type & 16 ? value->type | 64 : value->type | 16;
        value->size = 2;
        value->isArray = false;
        value->dimensionCount = 0;
        return true;
    }
    memset( value, 0, sizeof( *value ) );
    if ( !mi_evaluate_integer_expression( expression, &value->immediateValue,
                                          &value->size, &valueUnsigned ) ) return false;
    value->immediate = true;
    value->type = ( value->size == 1 ? 1 : ( value->size == 4 ? 4 : 2 ) ) |
                  ( valueUnsigned ? 32 : 0 );
    return true;
}

static void mi_format_string_preview( uint16_t address, int limit, char * output, size_t outputSize )
{
    size_t used = 0;
    int index;
    if ( outputSize < 3 ) return;
    output[ used++ ] = '\'';
    for ( index = 0; index < limit && used + 5 < outputSize; index++ )
    {
        unsigned char character = memory[ (uint16_t) ( address + index ) ];
        if ( !character ) break;
        if ( character == '"' || character == '\\' )
        {
            output[ used++ ] = '\\';
            output[ used++ ] = (char) character;
        }
        else if ( character == '\n' || character == '\r' || character == '\t' )
        {
            output[ used++ ] = '\\';
            output[ used++ ] = character == '\n' ? 'n' : ( character == '\r' ? 'r' : 't' );
        }
        else if ( isprint( character ) ) output[ used++ ] = (char) character;
        else
        {
            snprintf( output + used, outputSize - used, "\\x%02x", character );
            used += 4;
        }
    }
    if ( index == limit && used + 4 < outputSize )
    {
        output[ used++ ] = '.';
        output[ used++ ] = '.';
        output[ used++ ] = '.';
    }
    output[ used++ ] = '\'';
    output[ used ] = 0;
}

static void mi_format_debug_value( const MIDebugValue * debugValue, char * output, size_t outputSize )
{
    uint16_t address = debugValue->address;
    unsigned long value = debugValue->immediate ? debugValue->immediateValue : memory[ address ];
    int base = debugValue->type & 15;
    bool isUnsigned = ( debugValue->type & 32 ) != 0;
    if ( debugValue->isArray )
    {
        if ( base == 1 && debugValue->elementSize == 1 )
            mi_format_string_preview( address, debugValue->size < 48 ? debugValue->size : 48,
                                      output, outputSize );
        else
            snprintf( output, outputSize, "[%d]", debugValue->dimensionCount ? debugValue->dimensions[ 0 ] : 0 );
        return;
    }
    if ( debugValue->type & 128 ) { snprintf( output, outputSize, "{%s}", mi_type_name( debugValue->type, false, NULL, 0 ) ); return; }
    if ( !debugValue->immediate && debugValue->size > 1 )
        value |= (unsigned long) memory[ (uint16_t) ( address + 1 ) ] << 8;
    if ( !debugValue->immediate && debugValue->size > 2 )
    {
        value |= (unsigned long) memory[ (uint16_t) ( address + 2 ) ] << 16;
        value |= (unsigned long) memory[ (uint16_t) ( address + 3 ) ] << 24;
    }
    if ( debugValue->bitWidth > 0 )
    {
        unsigned long fieldValue = ( value >> debugValue->bitShift ) &
                                   ( ( 1UL << debugValue->bitWidth ) - 1 );
        if ( !isUnsigned && ( fieldValue & ( 1UL << ( debugValue->bitWidth - 1 ) ) ) )
            snprintf( output, outputSize, "%ld",
                      (long) fieldValue - (long) ( 1UL << debugValue->bitWidth ) );
        else
            snprintf( output, outputSize, "%lu", fieldValue );
    }
    else if ( debugValue->type & ( 16 | 64 ) )
    {
        if ( base == 1 && ( value & 0xffff ) != 0 )
        {
            char preview[ 64 ];
            mi_format_string_preview( (uint16_t) value, 32, preview, sizeof( preview ) );
            snprintf( output, outputSize, "0x%04lx %s", value & 0xffff, preview );
        }
        else
            snprintf( output, outputSize, "0x%04lx", value & 0xffff );
    }
    else if ( base == 5 && debugValue->size == 4 )
    {
        uint32_t bits = (uint32_t) value;
        float floatValue;
        memcpy( &floatValue, &bits, sizeof( floatValue ) );
        snprintf( output, outputSize, "%g", (double) floatValue );
    }
    else if ( isUnsigned )
        snprintf( output, outputSize, "%lu", value );
    else if ( debugValue->size == 1 )
        snprintf( output, outputSize, "%d", (int) (int8_t) value );
    else if ( debugValue->size == 2 )
        snprintf( output, outputSize, "%d", (int) (int16_t) value );
    else
        snprintf( output, outputSize, "%ld", (long) (int32_t) value );
}

static void mi_format_variable_value( const MIDebugVariable * variable, char * output, size_t outputSize )
{
    MIDebugValue value;
    mi_value_from_variable( variable, &value );
    mi_format_debug_value( &value, output, outputSize );
}

static bool mi_debug_value_writable( const MIDebugValue * value )
{
    return !value->immediate && !value->isArray && !( value->type & 128 ) &&
           ( value->type & 15 ) != 5 &&
           ( value->size == 1 || value->size == 2 || value->size == 4 );
}

static bool mi_write_debug_value( const MIDebugValue * value, unsigned long newValue )
{
    unsigned long storedValue = newValue;
    int index;
    if ( !mi_debug_value_writable( value ) ) return false;
    if ( ( value->type & 15 ) == 6 ) storedValue = newValue != 0;
    if ( value->bitWidth > 0 )
    {
        unsigned long fieldMask = ( ( 1UL << value->bitWidth ) - 1 ) << value->bitShift;
        unsigned long oldValue = memory[ value->address ];
        if ( value->size > 1 ) oldValue = mi_read_word( value->address );
        if ( value->size > 2 )
            oldValue |= (unsigned long) mi_read_word( (uint16_t) ( value->address + 2 ) ) << 16;
        storedValue = ( oldValue & ~fieldMask ) |
                      ( ( newValue << value->bitShift ) & fieldMask );
    }
    for ( index = 0; index < value->size; index++ )
        memory[ (uint16_t) ( value->address + index ) ] = (uint8_t) ( storedValue >> ( index * 8 ) );
    return true;
}

static int mi_debug_value_child_count( const MIDebugValue * value )
{
    int index;
    int count = 0;
    if ( value->isArray && value->dimensionCount > 0 ) return value->dimensions[ 0 ];
    if ( value->isFunctionPointer ) return 0;
    if ( value->type & ( 16 | 64 ) ) return 1;
    if ( value->type & 128 )
        for ( index = 0; index < g_miDebugFieldCount; index++ )
            if ( g_miDebugFields[ index ].structId == ( value->type >> 8 ) ) count++;
    return count;
}

static bool mi_debug_value_child_expression( const MIDebugValue * parent, const char * parentExpression,
                                             int childIndex, char * expression, size_t expressionSize,
                                             char * displayName, size_t displayNameSize )
{
    if ( parent->isArray && parent->dimensionCount > 0 )
    {
        if ( childIndex < 0 || childIndex >= parent->dimensions[ 0 ] ) return false;
        snprintf( expression, expressionSize, "%s[%d]", parentExpression, childIndex );
        snprintf( displayName, displayNameSize, "[%d]", childIndex );
        return true;
    }
    if ( !parent->isFunctionPointer && ( parent->type & ( 16 | 64 ) ) )
    {
        if ( childIndex != 0 ) return false;
        snprintf( expression, expressionSize, "*%s", parentExpression );
        strcpy( displayName, "*" );
        return true;
    }
    if ( parent->type & 128 )
    {
        int index;
        int found = 0;
        for ( index = 0; index < g_miDebugFieldCount; index++ )
            if ( g_miDebugFields[ index ].structId == ( parent->type >> 8 ) )
            {
                if ( found++ != childIndex ) continue;
                snprintf( expression, expressionSize, "%s.%s", parentExpression, g_miDebugFields[ index ].name );
                strncpy( displayName, g_miDebugFields[ index ].name, displayNameSize - 1 );
                displayName[ displayNameSize - 1 ] = 0;
                return true;
            }
    }
    return false;
}

static MIVariableObject * mi_create_variable_object( const char * expression )
{
    int slot;
    MIVariableObject * object;
    for ( slot = 0; slot < g_miVariableObjectCapacity && g_miVariableObjects[ slot ].active; slot++ ) {}
    if ( slot == g_miVariableObjectCapacity )
    {
        int oldCapacity = g_miVariableObjectCapacity;
        int newCapacity = oldCapacity * 2;
        MIVariableObject * grown = (MIVariableObject *) realloc(
            g_miVariableObjects, (size_t) newCapacity * sizeof( MIVariableObject ) );
        if ( !grown ) return NULL;
        g_miVariableObjects = grown;
        memset( g_miVariableObjects + oldCapacity, 0,
                (size_t) ( newCapacity - oldCapacity ) * sizeof( MIVariableObject ) );
        g_miVariableObjectCapacity = newCapacity;
    }
    object = &g_miVariableObjects[ slot ];
    object->active = true;
    object->frame = g_miSelectedFrame;
    snprintf( object->name, sizeof( object->name ), "var%d", g_miNextVariableObject++ );
    strncpy( object->expression, expression, sizeof( object->expression ) - 1 );
    object->expression[ sizeof( object->expression ) - 1 ] = 0;
    return object;
}

static MIVariableObject * mi_find_variable_object_in_command( const char * command )
{
    int slot;
    for ( slot = 0; slot < g_miVariableObjectCapacity; slot++ )
    {
        MIVariableObject * object = &g_miVariableObjects[ slot ];
        const char * found;
        size_t length;
        if ( !object->active ) continue;
        length = strlen( object->name );
        found = command;
        while ( ( found = strstr( found, object->name ) ) != NULL )
        {
            char before = found == command ? ' ' : found[ -1 ];
            char after = found[ length ];
            if ( ( isspace( (unsigned char) before ) || before == '"' ) &&
                 ( !after || isspace( (unsigned char) after ) || after == '"' ) ) return object;
            found += length;
        }
    }
    return NULL;
}

static MIDebugLine * mi_find_source_line( const char * file, int line )
{
    MIDebugLine * best = NULL;
    int index;
    for ( index = 0; index < g_miDebugLineCount; index++ )
    {
        MIDebugLine * candidate = &g_miDebugLines[ index ];
        if ( !mi_path_suffix_equal( file, candidate->file ) || candidate->line < line )
            continue;
        if ( !best || candidate->line < best->line ||
             ( candidate->line == best->line && candidate->address < best->address ) )
            best = candidate;
    }
    return best;
}

static MIDebugLine * mi_find_address_line( uint16_t address )
{
    MIDebugLine * best = NULL;
    MIDebugFunction * function = mi_find_debug_function( address );
    int index;
    if ( g_miDebugFunctionCount && !function )
        return NULL;
    for ( index = 0; index < g_miDebugLineCount; index++ )
    {
        MIDebugLine * candidate = &g_miDebugLines[ index ];
        if ( candidate->address > address ||
             ( function && candidate->address < function->start ) )
            continue;
        if ( !best || candidate->address >= best->address )
            best = candidate;
    }
    return best;
}

static void mi_start_source_step( MISourceStepMode mode )
{
    MIDebugLine * debugLine = mi_find_address_line( reg.pc );
    g_miSourceStepMode = mode;
    g_miSourceStepFile[ 0 ] = 0;
    g_miSourceStepLine = 0;
    g_miSourceStepSp = reg.sp;
    if ( debugLine )
    {
        strncpy( g_miSourceStepFile, debugLine->file, sizeof( g_miSourceStepFile ) - 1 );
        g_miSourceStepFile[ sizeof( g_miSourceStepFile ) - 1 ] = 0;
        g_miSourceStepLine = debugLine->line;
    }
    x80_debug_step();
}

static bool mi_start_source_step_out()
{
    uint16_t frameIx = mi_context_ix();
    if ( frameIx > 0xfffb )
        return false;
    g_miSourceStepReturnIx = mi_read_word( frameIx );
    g_miSourceStepReturnPc = mi_read_word( (uint16_t) ( frameIx + 2 ) );
    if ( g_miSourceStepReturnIx <= frameIx || g_miSourceStepReturnPc == 0 )
        return false;
    g_miSourceStepMode = mi_source_step_out;
    x80_debug_step();
    return true;
}

static bool mi_source_step_complete()
{
    MIDebugLine * debugLine;
    if ( g_miSourceStepMode == mi_source_step_none ||
         x80_debug_reason() != x80_debug_stop_step )
        return true;
    if ( g_miSourceStepMode == mi_source_step_out )
        return reg.pc == g_miSourceStepReturnPc && reg.ix == g_miSourceStepReturnIx;
    debugLine = mi_find_address_line( reg.pc );
    if ( !debugLine )
        return false;
    if ( g_miSourceStepLine == debugLine->line &&
         !strcmp( g_miSourceStepFile, debugLine->file ) )
        return false;
    if ( g_miSourceStepMode == mi_source_step_over && reg.sp < g_miSourceStepSp )
        return false;
    return true;
}

static MIBreakpoint * mi_find_address_breakpoint( uint16_t address )
{
    int slot;
    for ( slot = 0; slot < 256; slot++ )
        if ( g_miBreakpoints[ slot ].active && g_miBreakpoints[ slot ].address == address )
            return &g_miBreakpoints[ slot ];
    return NULL;
}

static void mi_copy_quoted_argument( const char * argument, char * output, size_t output_size );

static void mi_update_cpu_breakpoint( uint16_t address )
{
    int slot;
    for ( slot = 0; slot < 256; slot++ )
        if ( g_miBreakpoints[ slot ].active && g_miBreakpoints[ slot ].address == address )
        {
            x80_debug_set_breakpoint( address, true );
            return;
        }
    x80_debug_set_breakpoint( address, false );
}

static void mi_configure_breakpoint( MIBreakpoint * breakpoint, const char * command )
{
    const char * conditionOption = strstr( command, " -c " );
    const char * ignoreOption = strstr( command, " -i " );
    breakpoint->temporary = strstr( command, " -t " ) != NULL;
    breakpoint->conditionErrorReported = false;
    breakpoint->hitCount = 0;
    breakpoint->ignoreCount = ignoreOption ? strtoul( ignoreOption + 4, NULL, 0 ) : 0;
    breakpoint->condition[ 0 ] = 0;
    if ( conditionOption )
        mi_copy_quoted_argument( conditionOption + 4, breakpoint->condition,
                                 sizeof( breakpoint->condition ) );
}

static bool mi_prepare_breakpoint_stop()
{
    int slot;
    bool matched = false;
    bool breakpointRemoved = false;
    g_miStoppedBreakpoint = NULL;
    mi_rebuild_stack_frames();
    for ( slot = 0; slot < 256; slot++ )
    {
        MIBreakpoint * breakpoint = &g_miBreakpoints[ slot ];
        unsigned long conditionValue = 1;
        if ( !breakpoint->active || breakpoint->address != reg.pc ) continue;
        matched = true;
        breakpoint->hitCount++;
        if ( breakpoint->hitCount <= breakpoint->ignoreCount ) continue;
        if ( breakpoint->condition[ 0 ] &&
             !mi_evaluate_integer_expression( breakpoint->condition, &conditionValue ) )
        {
            if ( !breakpoint->conditionErrorReported )
            {
                printf( "&\"Breakpoint %d condition could not be evaluated; stopping\\n\"\n",
                        breakpoint->number );
                fflush( stdout );
                breakpoint->conditionErrorReported = true;
            }
            conditionValue = 1;
        }
        if ( !conditionValue ) continue;
        if ( !g_miStoppedBreakpoint )
            g_miStoppedBreakpoint = breakpoint;
        if ( breakpoint->temporary )
        {
            breakpoint->active = false;
            breakpointRemoved = true;
        }
    }
    if ( breakpointRemoved )
        mi_update_cpu_breakpoint( reg.pc );
    return g_miStoppedBreakpoint != NULL || !matched;
}

static const char * mi_skip_token( const char * line, char * token, size_t token_size )
{
    size_t length = 0;
    while ( isdigit( (unsigned char) *line ) )
    {
        if ( length + 1 < token_size )
            token[ length++ ] = *line;
        line++;
    }
    token[ length ] = 0;
    return line;
}

static void mi_result( const char * token, const char * result )
{
    printf( "%s%s\n", token, result );
    fflush( stdout );
}

static void mi_escape_text( const char * text, char * escaped, size_t escaped_size )
{
    size_t out = 0;
    while ( *text && out + 2 < escaped_size )
    {
        unsigned char c = (unsigned char) *text++;
        if ( c == '\\' || c == '"' )
        {
            escaped[ out++ ] = '\\';
            escaped[ out++ ] = (char) c;
        }
        else if ( c == '\n' || c == '\r' || c == '\t' )
        {
            escaped[ out++ ] = '\\';
            escaped[ out++ ] = c == '\n' ? 'n' : ( c == '\r' ? 'r' : 't' );
        }
        else if ( c >= 0x20 && c < 0x7f )
            escaped[ out++ ] = (char) c;
    }
    escaped[ out ] = 0;
}

static void mi_emit_source_location( const char * file, int line )
{
    char escapedFile[ MAX_PATH * 2 ];
    mi_escape_text( file, escapedFile, sizeof( escapedFile ) );
    printf( ",file=\"%s\",fullname=\"%s\",line=\"%d\"", escapedFile, escapedFile, line );
}

static void mi_target_character( uint8_t c )
{
    char text[ 2 ] = { (char) c, 0 };
    char escaped[ 8 ];
    mi_escape_text( text, escaped, sizeof( escaped ) );
    printf( "@\"%s\"\n", escaped );
    fflush( stdout );
}

static bool mi_input_ready()
{
#if defined( _WIN32 )
    HANDLE input = GetStdHandle( STD_INPUT_HANDLE );
    DWORD available = 0;
    return input != INVALID_HANDLE_VALUE && PeekNamedPipe( input, NULL, 0, NULL, &available, NULL ) && available != 0;
#elif defined( WATCOMDOS )
    return false;
#else
    fd_set input;
    struct timeval timeout = { 0, 0 };
    FD_ZERO( &input );
    FD_SET( STDIN_FILENO, &input );
    return select( STDIN_FILENO + 1, &input, NULL, NULL, &timeout ) > 0;
#endif
}

static const char * mi_find_argument( const char * command )
{
    const char * argument = strchr( command, ' ' );
    if ( !argument )
        return "";
    while ( *argument == ' ' )
        argument++;
    return argument;
}

static void mi_copy_quoted_argument( const char * argument, char * output, size_t output_size )
{
    size_t length = 0;
    while ( *argument == ' ' )
        argument++;
    if ( *argument == '"' )
        argument++;
    while ( *argument && *argument != '"' && length + 1 < output_size )
    {
        if ( *argument == '\\' && argument[ 1 ] )
            argument++;
        output[ length++ ] = *argument++;
    }
    output[ length ] = 0;
}

static void mi_copy_last_argument( const char * command, char * output, size_t output_size )
{
    const char * end = command + strlen( command );
    const char * start;
    while ( end > command && end[ -1 ] == ' ' )
        end--;
    start = end;
    if ( start > command && start[ -1 ] == '"' )
    {
        start--;
        while ( start > command )
        {
            start--;
            if ( *start == '"' && ( start == command || start[ -1 ] != '\\' ) )
                break;
        }
    }
    else
        while ( start > command && start[ -1 ] != ' ' )
            start--;
    mi_copy_quoted_argument( start, output, output_size );
}

static int mi_inline_frame( const char * command )
{
    const char * option = strstr( command, "--frame" );
    if ( !option ) return -1;
    option += 7;
    if ( *option == '=' ) option++;
    while ( isspace( (unsigned char) *option ) ) option++;
    return isdigit( (unsigned char) *option ) ? atoi( option ) : -1;
}

#if 0 // unused
static MIDebugVariable * mi_resolve_variable_expression( const char * argument, char * expression,
                                                        size_t expressionSize )
{
    char * start;
    char * end;
    mi_copy_quoted_argument( argument, expression, expressionSize );
    start = expression;
    while ( isspace( (unsigned char) *start ) ) start++;
    end = start + strlen( start );
    while ( end > start && isspace( (unsigned char) end[ -1 ] ) ) *--end = 0;
    while ( end - start >= 2 && *start == '(' && end[ -1 ] == ')' )
    {
        start++;
        end--;
        *end = 0;
        while ( isspace( (unsigned char) *start ) ) start++;
        while ( end > start && isspace( (unsigned char) end[ -1 ] ) ) *--end = 0;
    }
    if ( start != expression )
        memmove( expression, start, strlen( start ) + 1 );
    return mi_find_debug_variable( expression );
}
#endif

static uint16_t mi_register_psw()
{
    return reg.fZ80Mode ? reg.PSW<true>() : reg.PSW<false>();
}

static unsigned long mi_register_value( int index )
{
    switch ( index )
    {
        case 0: return mi_register_psw();
        case 1: return reg.B();
        case 2: return reg.D();
        case 3: return reg.H();
        case 4: return reg.ix;
        case 5: return reg.iy;
        case 6: return reg.sp;
        case 7: return reg.pc;
        case 8: return reg.ap | ( (unsigned long) reg.fp << 8 );
        case 9: return reg.bp | ( (unsigned long) reg.cp << 8 );
        case 10: return reg.dp | ( (unsigned long) reg.ep << 8 );
        case 11: return reg.hp | ( (unsigned long) reg.lp << 8 );
        case 12: return reg.i;
        case 13: return reg.r;
        default: return 0;
    }
}

static void mi_emit_stop()
{
    const char * reason = "signal-received";
    int breakpointNumber = 0;
    MIBreakpoint * breakpoint = NULL;
    MIDebugLine * debugLine = mi_find_address_line( reg.pc );
    mi_rebuild_stack_frames();
    if ( x80_debug_reason() == x80_debug_stop_breakpoint )
    {
        reason = "breakpoint-hit";
        breakpoint = g_miStoppedBreakpoint;
        if ( breakpoint ) breakpointNumber = breakpoint->number;
    }
    else if ( x80_debug_reason() == x80_debug_stop_step )
        reason = "end-stepping-range";
    else if ( x80_debug_reason() == x80_debug_stop_entry )
        reason = "entry-point-hit";
    else if ( x80_debug_reason() == x80_debug_stop_pause )
        reason = "signal-received";
    printf( "*stopped,reason=\"%s\"", reason );
    if ( breakpointNumber )
        printf( ",bkptno=\"%d\"", breakpointNumber );
    printf( ",frame={addr=\"0x%04x\",func=\"??\"", reg.pc );
    if ( breakpoint && breakpoint->file[ 0 ] )
        mi_emit_source_location( breakpoint->file, breakpoint->line );
    else if ( debugLine )
        mi_emit_source_location( debugLine->file, debugLine->line );
    printf( "},thread-id=\"1\",stopped-threads=\"all\"\n" );
    fflush( stdout );
}

enum MIAction
{
    mi_action_none,
    mi_action_run,
    mi_action_exit
};

static MIAction mi_handle_command_impl( const char * input )
{
    char line[ 4096 ];
    char token[ 32 ];
    const char * command;
    size_t length;

    strncpy( line, input, sizeof( line ) - 1 );
    line[ sizeof( line ) - 1 ] = 0;
    length = strlen( line );
    while ( length && ( line[ length - 1 ] == '\n' || line[ length - 1 ] == '\r' ) )
        line[ --length ] = 0;
    command = mi_skip_token( line, token, sizeof( token ) );
    {
        int frame = mi_inline_frame( command );
        if ( frame >= 0 && frame < g_miStackFrameCount ) g_miSelectedFrame = frame;
    }

    if ( !strncmp( command, "-file-exec-and-symbols", 22 ) )
    {
        mi_copy_quoted_argument( mi_find_argument( command ), g_miProgram, sizeof( g_miProgram ) );
        mi_load_debug_metadata();
        mi_result( token, "^done" );
    }
    else if ( !strncmp( command, "-symbol-list-lines", 18 ) )
    {
        char sourceFile[ MAX_PATH ];
        bool first = true;
        int index;
        mi_copy_quoted_argument( mi_find_argument( command ), sourceFile, sizeof( sourceFile ) );
        printf( "%s^done,lines=[", token );
        for ( index = 0; index < g_miDebugLineCount; index++ )
        {
            MIDebugLine * debugLine = &g_miDebugLines[ index ];
            if ( !mi_path_suffix_equal( sourceFile, debugLine->file ) )
                continue;
            printf( "%s{pc=\"0x%04x\",line=\"%d\"}", first ? "" : ",",
                    debugLine->address, debugLine->line );
            first = false;
        }
        printf( "]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-list-features", 14 ) )
        mi_result( token, "^done,features=[]" );
    else if ( !strncmp( command, "-exec-arguments", 15 ) )
    {
        strncpy( g_miArguments, mi_find_argument( command ), sizeof( g_miArguments ) - 1 );
        g_miArguments[ sizeof( g_miArguments ) - 1 ] = 0;
        mi_result( token, "^done" );
    }
    else if ( !strncmp( command, "-environment-cd", 15 ) )
    {
        char path[ MAX_PATH ];
        mi_copy_quoted_argument( mi_find_argument( command ), path, sizeof( path ) );
        if ( chdir( path ) == 0 )
            mi_result( token, "^done" );
        else
            mi_result( token, "^error,msg=\"unable to change directory\"" );
    }
    else if ( !strncmp( command, "-exec-run", 9 ) || !strncmp( command, "-exec-continue", 14 ) )
    {
        g_miSourceStepMode = mi_source_step_none;
        x80_debug_continue();
        mi_result( token, "^running" );
        printf( "*running,thread-id=\"all\"\n" );
        fflush( stdout );
        return mi_action_run;
    }
    else if ( !strncmp( command, "-exec-step-instruction", 22 ) ||
              !strncmp( command, "-exec-next-instruction", 22 ) )
    {
        g_miSourceStepMode = mi_source_step_none;
        x80_debug_step();
        mi_result( token, "^running" );
        printf( "*running,thread-id=\"all\"\n" );
        fflush( stdout );
        return mi_action_run;
    }
    else if ( !strncmp( command, "-exec-step", 10 ) || !strncmp( command, "-exec-next", 10 ) )
    {
        mi_start_source_step( !strncmp( command, "-exec-next", 10 ) ?
                              mi_source_step_over : mi_source_step_into );
        mi_result( token, "^running" );
        printf( "*running,thread-id=\"all\"\n" );
        fflush( stdout );
        return mi_action_run;
    }
    else if ( !strncmp( command, "-exec-finish", 12 ) )
    {
        if ( !mi_start_source_step_out() )
        {
            int frame = g_miSelectedFrame >= 0 ? g_miSelectedFrame : 0;
            if ( !g_miStackFrameCount || frame != g_miStackFrameCount - 1 )
            {
                mi_result( token, "^error,msg=\"unable to determine caller return address\"" );
                return mi_action_none;
            }
            g_miSourceStepMode = mi_source_step_none;
            x80_debug_continue();
        }
        mi_result( token, "^running" );
        printf( "*running,thread-id=\"all\"\n" );
        fflush( stdout );
        return mi_action_run;
    }
    else if ( !strncmp( command, "-exec-interrupt", 15 ) )
    {
        x80_debug_pause();
        mi_result( token, "^done" );
    }
    else if ( !strncmp( command, "-break-insert", 13 ) )
    {
        const char * star = strrchr( command, '*' );
        if ( star )
        {
            unsigned long address = strtoul( star + 1, NULL, 0 );
            int slot;
            for ( slot = 0; slot < 256 && g_miBreakpoints[ slot ].active; slot++ ) {}
            if ( slot < 256 && address <= 0xffff )
            {
                memset( &g_miBreakpoints[ slot ], 0, sizeof( g_miBreakpoints[ slot ] ) );
                g_miBreakpoints[ slot ].number = g_miNextBreakpoint++;
                g_miBreakpoints[ slot ].address = (uint16_t) address;
                g_miBreakpoints[ slot ].active = true;
                mi_configure_breakpoint( &g_miBreakpoints[ slot ], command );
                g_miBreakpoints[ slot ].file[ 0 ] = 0;
                g_miBreakpoints[ slot ].line = 0;
                mi_update_cpu_breakpoint( (uint16_t) address );
                printf( "%s^done,bkpt={number=\"%d\",type=\"breakpoint\",disp=\"%s\",enabled=\"y\",addr=\"0x%04lx\",thread-groups=[\"i1\"],times=\"0\",original-location=\"*0x%04lx\"}\n",
                    token, g_miBreakpoints[ slot ].number,
                    g_miBreakpoints[ slot ].temporary ? "del" : "keep", address, address );
                fflush( stdout );
            }
            else
                mi_result( token, "^error,msg=\"invalid breakpoint address\"" );
        }
        else
        {
            char sourceFile[ MAX_PATH ] = {0};
            int sourceLine = 0;
            const char * fileOption = strstr( command, " -f " );
            const char * lineOption = strstr( command, " -l " );
            MIDebugLine * debugLine = NULL;
            MIDebugFunction * debugFunction = NULL;
            uint16_t breakpointAddress = 0;
            int slot;
            if ( fileOption && lineOption )
            {
                mi_copy_quoted_argument( fileOption + 4, sourceFile, sizeof( sourceFile ) );
                sourceLine = atoi( lineOption + 4 );
            }
            else
            {
                char location[ MAX_PATH + 32 ];
                char * colon;
                mi_copy_last_argument( command, location, sizeof( location ) );
                colon = strrchr( location, ':' );
                if ( colon )
                {
                    *colon++ = 0;
                    strncpy( sourceFile, location, sizeof( sourceFile ) - 1 );
                    sourceLine = atoi( colon );
                }
                else
                {
                    strncpy( sourceFile, location, sizeof( sourceFile ) - 1 );
                    debugFunction = mi_find_debug_function_name( location );
                }
            }
            if ( sourceLine > 0 )
            {
                debugLine = mi_find_source_line( sourceFile, sourceLine );
                if ( debugLine ) breakpointAddress = debugLine->address;
            }
            else if ( debugFunction )
            {
                breakpointAddress = debugFunction->start;
                debugLine = mi_find_address_line( breakpointAddress );
                if ( debugLine && debugLine->address != breakpointAddress ) debugLine = NULL;
            }
            for ( slot = 0; slot < 256 && g_miBreakpoints[ slot ].active; slot++ ) {}
            if ( breakpointAddress && slot < 256 )
            {
                char escapedOriginal[ MAX_PATH * 2 ];
                mi_escape_text( sourceFile, escapedOriginal, sizeof( escapedOriginal ) );
                memset( &g_miBreakpoints[ slot ], 0, sizeof( g_miBreakpoints[ slot ] ) );
                g_miBreakpoints[ slot ].number = g_miNextBreakpoint++;
                g_miBreakpoints[ slot ].address = breakpointAddress;
                g_miBreakpoints[ slot ].active = true;
                mi_configure_breakpoint( &g_miBreakpoints[ slot ], command );
                if ( debugLine )
                {
                    strncpy( g_miBreakpoints[ slot ].file, debugLine->file, sizeof( g_miBreakpoints[ slot ].file ) - 1 );
                    g_miBreakpoints[ slot ].file[ sizeof( g_miBreakpoints[ slot ].file ) - 1 ] = 0;
                    g_miBreakpoints[ slot ].line = debugLine->line;
                }
                mi_update_cpu_breakpoint( breakpointAddress );
                printf( "%s^done,bkpt={number=\"%d\",type=\"breakpoint\",disp=\"%s\",enabled=\"y\",addr=\"0x%04x\"",
                    token, g_miBreakpoints[ slot ].number,
                        g_miBreakpoints[ slot ].temporary ? "del" : "keep", breakpointAddress );
                if ( debugLine )
                    mi_emit_source_location( debugLine->file, debugLine->line );
                printf( ",thread-groups=[\"i1\"],times=\"0\",original-location=\"%s",
                    escapedOriginal );
                if ( sourceLine ) printf( ":%d", sourceLine );
                printf( "\"}\n" );
                fflush( stdout );
            }
            else
                mi_result( token, "^error,msg=\"no executable source location found\"" );
        }
    }
    else if ( !strncmp( command, "-break-list", 11 ) )
    {
        bool first = true;
        int slot;
        printf( "%s^done,BreakpointTable={nr_rows=\"", token );
        {
            int count = 0;
            for ( slot = 0; slot < 256; slot++ ) if ( g_miBreakpoints[ slot ].active ) count++;
            printf( "%d\",nr_cols=\"6\",body=[", count );
        }
        for ( slot = 0; slot < 256; slot++ )
        {
            MIBreakpoint * breakpoint = &g_miBreakpoints[ slot ];
            if ( !breakpoint->active ) continue;
            printf( "%sbkpt={number=\"%d\",type=\"breakpoint\",disp=\"%s\",enabled=\"y\",addr=\"0x%04x\",times=\"%lu\"}",
                    first ? "" : ",", breakpoint->number,
                    breakpoint->temporary ? "del" : "keep", breakpoint->address,
                    breakpoint->hitCount );
            first = false;
        }
        printf( "]}\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-break-delete", 13 ) )
    {
        int number = atoi( mi_find_argument( command ) );
        int slot;
        for ( slot = 0; slot < 256; slot++ )
            if ( g_miBreakpoints[ slot ].active && g_miBreakpoints[ slot ].number == number )
            {
                uint16_t address = g_miBreakpoints[ slot ].address;
                g_miBreakpoints[ slot ].active = false;
                mi_update_cpu_breakpoint( address );
            }
        mi_result( token, "^done" );
    }
    else if ( !strncmp( command, "-data-list-register-names", 25 ) )
        mi_result( token, "^done,register-names=[\"af\",\"bc\",\"de\",\"hl\",\"ix\",\"iy\",\"sp\",\"pc\",\"af'\",\"bc'\",\"de'\",\"hl'\",\"i\",\"r\"]" );
    else if ( !strncmp( command, "-data-list-register-values", 26 ) )
    {
        int index;
        printf( "%s^done,register-values=[", token );
        for ( index = 0; index < 14; index++ )
            printf( "%s{number=\"%d\",value=\"0x%04lx\"}", index ? "," : "", index, mi_register_value( index ) );
        printf( "]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-data-read-memory-bytes", 23 ) )
    {
        const char * argument = mi_find_argument( command );
        long offset = 0;
        unsigned long baseAddress;
        long effectiveAddress;
        unsigned long address;
        if ( !strncmp( argument, "-o", 2 ) && isspace( (unsigned char) argument[ 2 ] ) )
        {
            argument += 2;
            offset = strtol( argument, (char **) &argument, 0 );
        }
        baseAddress = strtoul( argument, (char **) &argument, 0 );
        unsigned long count = strtoul( argument, NULL, 0 );
        unsigned long index;
        effectiveAddress = (long) baseAddress + offset;
        if ( effectiveAddress < 0 ) effectiveAddress = 0;
        if ( effectiveAddress > 0xffff ) effectiveAddress = 0xffff;
        address = (unsigned long) effectiveAddress;
        if ( count > 0x10000 - address ) count = 0x10000 - address;
        printf( "%s^done,memory=[{begin=\"0x%04lx\",offset=\"0x00000000\",end=\"0x%04lx\",contents=\"", token, address, address + count );
        for ( index = 0; index < count; index++ ) printf( "%02x", memory[ address + index ] );
        printf( "\"}]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-data-disassemble", 17 ) )
    {
        const char * startOption = strstr( command, "-s " );
        const char * endOption = strstr( command, "-e " );
        const char * modeOption = strstr( command, "--" );
        unsigned long start;
        unsigned long end;
        unsigned long address;
        int mode;
        bool first = true;
        if ( !startOption || !endOption || !modeOption )
        {
            mi_result( token, "^error,msg=\"-data-disassemble requires -s START -e END -- MODE\"" );
            return mi_action_none;
        }
        start = strtoul( startOption + 3, NULL, 0 );
        end = strtoul( endOption + 3, NULL, 0 );
        mode = atoi( modeOption + 2 );
        if ( start > 0xffff || end > 0x10000 || end < start )
        {
            mi_result( token, "^error,msg=\"invalid disassembly address range\"" );
            return mi_action_none;
        }
        if ( mode != 0 && mode != 2 )
        {
            mi_result( token, "^error,msg=\"mixed source and assembly disassembly is not supported\"" );
            return mi_action_none;
        }
        printf( "%s^done,asm_insns=[", token );
        address = start;
        while ( address < end )
        {
            uint8_t instructionLength = x80_instruction_length( (uint16_t) address );
            MIDebugFunction * function = mi_find_debug_function( (uint16_t) address );
            char escapedInstruction[ 128 ];
            char rawInstruction[ 64 ];
            unsigned int index;
            if ( !instructionLength || instructionLength > 0x10000 - address )
            {
                instructionLength = 1;
                snprintf( rawInstruction, sizeof( rawInstruction ), "db %02xh", memory[ address ] );
            }
            else
            {
                strncpy( rawInstruction, x80_render_operation( (uint16_t) address ),
                         sizeof( rawInstruction ) - 1 );
                rawInstruction[ sizeof( rawInstruction ) - 1 ] = 0;
            }
            mi_escape_text( rawInstruction,
                            escapedInstruction, sizeof( escapedInstruction ) );
            printf( "%s{address=\"0x%04lx\",func-name=\"%s\",offset=\"%lu\",opcodes=\"",
                    first ? "" : ",", address,
                    function ? function->sourceName : "??",
                    function ? address - function->start : 0 );
            for ( index = 0; index < instructionLength; index++ )
                printf( "%s%02x", index ? " " : "", memory[ address + index ] );
            printf( "\",inst=\"%s\"}", escapedInstruction );
            first = false;
            address += instructionLength;
        }
        printf( "]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-data-evaluate-expression", 25 ) )
    {
        char expression[ 256 ];
        unsigned long value = 0;
        MIDebugValue debugValue;
        mi_copy_last_argument( command, expression, sizeof( expression ) );
        if ( mi_resolve_or_evaluate( expression, &debugValue ) )
        {
            char formatted[ 64 ];
            mi_format_debug_value( &debugValue, formatted, sizeof( formatted ) );
            printf( "%s^done,value=\"%s\"\n", token, formatted );
            fflush( stdout );
            return mi_action_none;
        }
        if ( strchr( expression, '$' ) )
        {
            const char * name = strchr( expression, '$' ) + 1;
            if ( !strncmp( name, "pc", 2 ) ) value = reg.pc;
            else if ( !strncmp( name, "sp", 2 ) ) value = reg.sp;
            else if ( !strncmp( name, "af", 2 ) ) value = mi_register_psw();
            else if ( !strncmp( name, "bc", 2 ) ) value = reg.B();
            else if ( !strncmp( name, "de", 2 ) ) value = reg.D();
            else if ( !strncmp( name, "hl", 2 ) ) value = reg.H();
            else if ( !strncmp( name, "ix", 2 ) ) value = reg.ix;
            else if ( !strncmp( name, "iy", 2 ) ) value = reg.iy;
        }
        else
        {
            char * end;
            value = strtoul( expression, &end, 0 );
            while ( isspace( (unsigned char) *end ) ) end++;
            if ( end == expression || *end )
            {
                printf( "%s^error,msg=\"unsupported or side-effecting expression\"\n", token );
                fflush( stdout );
                return mi_action_none;
            }
        }
        printf( "%s^done,value=\"0x%04lx\"\n", token, value );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-var-create", 11 ) )
    {
        char expression[ 256 ];
        MIDebugValue debugValue;
        MIVariableObject * object;
        mi_copy_last_argument( command, expression, sizeof( expression ) );
        object = mi_resolve_or_evaluate( expression, &debugValue ) ? mi_create_variable_object( expression ) : NULL;
        if ( !object )
            mi_result( token, "^error,msg=\"variable is not available in the current scope\"" );
        else
        {
            char formatted[ 64 ];
            int childCount = mi_debug_value_child_count( &debugValue );
            mi_format_debug_value( &debugValue, formatted, sizeof( formatted ) );
            printf( "%s^done,name=\"%s\",numchild=\"%d\",value=\"%s\",type=\"%s\",thread-id=\"1\",has_more=\"0\"\n",
                    token, object->name, childCount, formatted,
                    mi_type_name( debugValue.type, debugValue.isArray,
                          debugValue.dimensions, debugValue.dimensionCount,
                          debugValue.isFunctionPointer ) );
            fflush( stdout );
        }
    }
    else if ( !strncmp( command, "-var-evaluate-expression", 24 ) ||
              !strncmp( command, "-var-info-type", 14 ) ||
              !strncmp( command, "-var-show-attributes", 20 ) ||
              !strncmp( command, "-var-list-children", 18 ) )
    {
        MIVariableObject * object;
        MIDebugValue debugValue;
        int selectedFrame = g_miSelectedFrame;
        object = mi_find_variable_object_in_command( command );
        if ( object ) g_miSelectedFrame = object->frame;
        if ( !object || !mi_resolve_or_evaluate( object->expression, &debugValue ) )
            mi_result( token, "^error,msg=\"variable object is not available\"" );
        else if ( !strncmp( command, "-var-info-type", 14 ) )
        {
            char result[ 128 ];
            snprintf( result, sizeof( result ), "^done,type=\"%s\"",
                      mi_type_name( debugValue.type, debugValue.isArray,
                                    debugValue.dimensions, debugValue.dimensionCount,
                                    debugValue.isFunctionPointer ) );
            mi_result( token, result );
        }
        else if ( !strncmp( command, "-var-show-attributes", 20 ) )
            mi_result( token, mi_debug_value_writable( &debugValue ) ?
                       "^done,attr=\"editable\"" : "^done,attr=\"noneditable\"" );
        else if ( !strncmp( command, "-var-list-children", 18 ) )
        {
            int childCount = mi_debug_value_child_count( &debugValue );
            int childIndex;
            printf( "%s^done,numchild=\"%d\",children=[", token, childCount );
            for ( childIndex = 0; childIndex < childCount; childIndex++ )
            {
                char childExpression[ 256 ];
                char displayName[ 64 ];
                char formatted[ 64 ];
                MIDebugValue childValue;
                MIVariableObject * childObject;
                if ( !mi_debug_value_child_expression( &debugValue, object->expression, childIndex,
                                                       childExpression, sizeof( childExpression ),
                                                       displayName, sizeof( displayName ) ) ||
                     !mi_resolve_debug_value( childExpression, &childValue ) ) continue;
                childObject = mi_create_variable_object( childExpression );
                if ( !childObject ) break;
                mi_format_debug_value( &childValue, formatted, sizeof( formatted ) );
                printf( "%schild={name=\"%s\",exp=\"%s\",numchild=\"%d\",value=\"%s\",type=\"%s\",thread-id=\"1\"}",
                        childIndex ? "," : "", childObject->name, displayName,
                        mi_debug_value_child_count( &childValue ), formatted,
                        mi_type_name( childValue.type, childValue.isArray,
                                      childValue.dimensions, childValue.dimensionCount,
                                      childValue.isFunctionPointer ) );
            }
            printf( "],has_more=\"0\"\n" );
            fflush( stdout );
        }
        else
        {
            char formatted[ 64 ];
            mi_format_debug_value( &debugValue, formatted, sizeof( formatted ) );
            printf( "%s^done,value=\"%s\"\n", token, formatted );
            fflush( stdout );
        }
        g_miSelectedFrame = selectedFrame;
    }
    else if ( !strncmp( command, "-var-assign", 11 ) )
    {
        MIVariableObject * object = mi_find_variable_object_in_command( command );
        MIDebugValue debugValue;
        unsigned long newValue;
        char assignment[ 256 ];
        const char * argument = mi_find_argument( command );
        int selectedFrame = g_miSelectedFrame;
        bool assigned = false;
        if ( object )
        {
            const char * objectName = strstr( argument, object->name );
            argument = objectName ? objectName + strlen( object->name ) : argument;
            while ( isspace( (unsigned char) *argument ) ) argument++;
            mi_copy_quoted_argument( argument, assignment, sizeof( assignment ) );
            g_miSelectedFrame = object->frame;
            assigned = mi_resolve_debug_value( object->expression, &debugValue ) &&
                       mi_evaluate_integer_expression( assignment, &newValue ) &&
                       mi_write_debug_value( &debugValue, newValue );
        }
        if ( !assigned )
            mi_result( token, "^error,msg=\"variable is not editable or value is unsupported\"" );
        else
        {
            char formatted[ 64 ];
            mi_format_debug_value( &debugValue, formatted, sizeof( formatted ) );
            printf( "%s^done,value=\"%s\"\n", token, formatted );
            fflush( stdout );
        }
        g_miSelectedFrame = selectedFrame;
    }
    else if ( !strncmp( command, "-var-update", 11 ) )
    {
        bool first = true;
        int selectedFrame = g_miSelectedFrame;
        int slot;
        printf( "%s^done,changelist=[", token );
        for ( slot = 0; slot < g_miVariableObjectCapacity; slot++ )
        {
            MIVariableObject * object = &g_miVariableObjects[ slot ];
            MIDebugValue debugValue;
            char formatted[ 64 ];
            if ( !object->active ) continue;
            g_miSelectedFrame = object->frame;
            if ( !mi_resolve_or_evaluate( object->expression, &debugValue ) )
                printf( "%s{name=\"%s\",in_scope=\"false\"}", first ? "" : ",", object->name );
            else
            {
                mi_format_debug_value( &debugValue, formatted, sizeof( formatted ) );
                printf( "%s{name=\"%s\",value=\"%s\",in_scope=\"true\",type_changed=\"false\",has_more=\"0\"}",
                        first ? "" : ",", object->name, formatted );
            }
            first = false;
        }
        g_miSelectedFrame = selectedFrame;
        printf( "]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-var-delete", 11 ) )
    {
        char objectName[ 32 ];
        int slot;
        mi_copy_last_argument( command, objectName, sizeof( objectName ) );
        for ( slot = 0; slot < g_miVariableObjectCapacity; slot++ )
            if ( g_miVariableObjects[ slot ].active && !strcmp( g_miVariableObjects[ slot ].name, objectName ) )
                g_miVariableObjects[ slot ].active = false;
        mi_result( token, "^done,ndeleted=\"1\"" );
    }
    else if ( !strncmp( command, "-stack-select-frame", 19 ) )
    {
        int frame = atoi( mi_find_argument( command ) );
        if ( frame >= 0 && frame < g_miStackFrameCount )
        {
            g_miSelectedFrame = frame;
            mi_result( token, "^done" );
        }
        else
            mi_result( token, "^error,msg=\"invalid stack frame\"" );
    }
    else if ( !strncmp( command, "-stack-list-frames", 18 ) )
    {
        int frame;
        printf( "%s^done,stack=[", token );
        for ( frame = 0; frame < g_miStackFrameCount; frame++ )
        {
            MIStackFrame * stackFrame = &g_miStackFrames[ frame ];
            MIDebugFunction * function = mi_find_debug_function( stackFrame->pc );
            MIDebugLine * debugLine = mi_find_address_line( stackFrame->pc );
            printf( "%sframe={level=\"%d\",addr=\"0x%04x\",func=\"%s\"",
                    frame ? "," : "", frame, stackFrame->pc, function ? function->sourceName : "??" );
            if ( debugLine )
                mi_emit_source_location( debugLine->file, debugLine->line );
            printf( "}" );
        }
        printf( "]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-stack-info-depth", 17 ) )
    {
        char result[ 64 ];
        snprintf( result, sizeof( result ), "^done,depth=\"%d\"", g_miStackFrameCount );
        mi_result( token, result );
    }
    else if ( !strncmp( command, "-stack-list-arguments", 21 ) )
    {
        int savedFrame = g_miSelectedFrame;
        int frame;
        printf( "%s^done,stack-args=[", token );
        for ( frame = 0; frame < g_miStackFrameCount; frame++ )
        {
            MIDebugFunction * function;
            bool first = true;
            int index;
            g_miSelectedFrame = frame;
            function = mi_find_debug_function( mi_context_pc() );
            printf( "%sframe={level=\"%d\",args=[", frame ? "," : "", frame );
            if ( function ) for ( index = 0; index < g_miDebugVariableCount; index++ )
            {
                MIDebugVariable * variable = &g_miDebugVariables[ index ];
                char formatted[ 64 ];
                 if ( variable->storage != 3 || strcmp( variable->function, function->name ) ||
                     variable->declaration > mi_context_pc() || mi_context_pc() >= variable->end ) continue;
                mi_format_variable_value( variable, formatted, sizeof( formatted ) );
                printf( "%s{name=\"%s\",value=\"%s\"}", first ? "" : ",", variable->name, formatted );
                first = false;
            }
            printf( "]}" );
        }
        g_miSelectedFrame = savedFrame;
        printf( "]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-stack-list-locals", 18 ) || !strncmp( command, "-stack-list-variables", 21 ) )
    {
        MIDebugFunction * function = mi_find_debug_function( mi_context_pc() );
        bool first = true;
        int index;
        printf( "%s^done,variables=[", token );
        if ( function )
            for ( index = 0; index < g_miDebugVariableCount; index++ )
            {
                MIDebugVariable * variable = &g_miDebugVariables[ index ];
                char formatted[ 64 ];
                 if ( strcmp( variable->function, function->name ) || variable->declaration > mi_context_pc() ||
                     mi_context_pc() >= variable->end || mi_find_debug_variable( variable->name ) != variable )
                    continue;
                mi_format_variable_value( variable, formatted, sizeof( formatted ) );
                printf( "%s{name=\"%s\",value=\"%s\",type=\"%s\"%s}",
                        first ? "" : ",", variable->name, formatted,
                        mi_variable_type_name( variable ),
                        variable->storage == 3 ? ",arg=\"1\"" : "" );
                first = false;
            }
        printf( "]\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-thread-info", 12 ) )
    {
        MIBreakpoint * breakpoint = mi_find_address_breakpoint( reg.pc );
        MIDebugLine * debugLine = mi_find_address_line( reg.pc );
        printf( "%s^done,threads=[{id=\"1\",target-id=\"CP/M\",frame={level=\"0\",addr=\"0x%04x\",func=\"??\"", token, reg.pc );
        if ( breakpoint && breakpoint->file[ 0 ] )
            mi_emit_source_location( breakpoint->file, breakpoint->line );
        else if ( debugLine )
            mi_emit_source_location( debugLine->file, debugLine->line );
        printf( "},state=\"stopped\"}],current-thread-id=\"1\"\n" );
        fflush( stdout );
    }
    else if ( !strncmp( command, "-thread-list-ids", 16 ) )
        mi_result( token, "^done,thread-ids={thread-id=\"1\",number-of-threads=\"1\"},current-thread-id=\"1\"" );
    else if ( !strncmp( command, "-list-thread-groups", 19 ) )
        mi_result( token, "^done,groups=[{id=\"i1\",type=\"process\",pid=\"1\",executable=\"ntvcm\"}]" );
    else if ( !strncmp( command, "-gdb-exit", 9 ) )
    {
        mi_result( token, "^exit" );
        return mi_action_exit;
    }
    else
        mi_result( token, "^done" );

    return mi_action_none;
}

static MIAction mi_handle_command( const char * input )
{
    int selectedFrame = g_miSelectedFrame;
    int inlineFrame = mi_inline_frame( input );
    MIAction action = mi_handle_command_impl( input );
    if ( inlineFrame >= 0 && inlineFrame < g_miStackFrameCount )
        g_miSelectedFrame = selectedFrame;
    return action;
}

static bool mi_read_command( bool wait, MIAction * action )
{
    char line[ 4096 ];
    if ( !wait && !mi_input_ready() )
        return false;
    if ( !fgets( line, sizeof( line ), stdin ) )
    {
        *action = mi_action_exit;
        return false;
    }
    *action = mi_handle_command( line );
    return true;
}

static void mi_wait_for_program()
{
    MIAction action = mi_action_none;
    printf( "=thread-group-added,id=\"i1\"\n(gdb) \n" );
    fflush( stdout );
    while ( !g_miProgram[ 0 ] && action != mi_action_exit )
        mi_read_command( true, &action );
}

// ^z. CP/M writes 128 bytes at a time. When a file is read by the emulator and isn't 128-byte aligned in size
// we can only guess what the app that created such a file on CP/M would have had in the bytes above the
// remainder of the file above the last byte of valid data. Most apps work with various fill values, but:
//  -  Hisoft C v3.09, Eco C, and other apps fail if zero is assumed as the fill value and works with ^z
//  -  Cowgol fails if ^z is assumed and works with 0.
// Note that the correct fix is for apps to always write 128-byte aligned files so there is no guesswork.

static uint8_t g_readFileFillValue = 0x1a; // assume ^z, the EOF marker for CP/M. Works with all tested apps except Cowgol

enum terminal_escape { termVT100, termVT52, termKayproII };
static terminal_escape g_termEscape = termVT100;

void dump_memory( const char * pname = "ntvcm.dmp" )
{
    FILE * fp = fopen( pname, "wb" );
    if ( !fp )
    {
        printf( "can't create dump file, error %d = %s\n", errno, strerror( errno ) );
        return;
    }

    for ( uint32_t i = 0; i < 65536; i += 128 ) // write in small blocks so it works on DOS too
        fwrite( & memory[ i ], 1, 128, fp );

    fclose( fp );
} //dump_memory

#ifdef WATCOMDOS
    #include <dos.h>
    uint32_t DosTimeInMS()
    {
        struct dostime_t tNow;
        _dos_gettime( &tNow );
        uint32_t t = (uint32_t) tNow.hour * 60 * 60 * 100;
        t += (uint32_t) tNow.minute * 60 * 100;
        t += (uint32_t) tNow.second * 100;
        t += (uint32_t) tNow.hsecond;
        return t * 10;
    } //DosTimeInMS
#endif //WATCOMDOS

#ifdef WATCOMLINUX
    const uint64_t ms_per_ns = 1000000;
    uint64_t LinuxTimeInMS()
    {
        struct timespec spec;
        clock_gettime( CLOCK_REALTIME, &spec );
        return spec.tv_sec * 1000LL + ( ( spec.tv_nsec + ( ms_per_ns / 2 ) ) / ms_per_ns );
    } //LinuxTimeInMS
#endif //WATCOMLINUX

int ends_with( const char * str, const char * end )
{
    size_t len = strlen( str );
    size_t lenend = strlen( end );

    if ( len < lenend )
        return false;

    return ( 0 == _stricmp( str + len - lenend, end ) );
} //ends_with

bool IsAFolder( const char * pc )
{
    struct stat file_stat;
    int result = stat( pc, &file_stat );
    return ( ( 0 == result ) && ( 0 != ( S_IFDIR & file_stat.st_mode ) ) );
} //IsAFolder

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

    // CP/M extensions are at most 3 characters. Reject host names whose part
    // after the dot is longer (e.g. ".gitignore"), or that contain a second
    // dot, since CP/M can't represent them. Without this, such a file reaches
    // ExtractFilename and trips its (extlen <= 3) assert during enumeration.
    if ( pcdot )
    {
        if ( strchr( pcdot + 1, '.' ) )
            return false;
        if ( strlen( pcdot + 1 ) > 3 )
            return false;
    }

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
#elif defined( WATCOMDOS )
    static bool g_FindActive = false;
    static struct find_t g_FindFirst;

    void CloseFindFirst()
    {
        if ( g_FindActive )
        {
            g_FindActive = false;
            _dos_findclose( & g_FindFirst );
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

    void ExtractFilename( const char * filename, char * name, char * ext )
    {
        bool pastdot = false;
        const char * p = filename;
        int extlen = 0;
        int namelen = 0;
        while ( *p )
        {
            if ( '.' == *p )
                pastdot = true;
            else
            {
                if ( pastdot )
                    ext[ extlen++ ] = *p;
                else
                    name[ namelen++ ] = *p;
            }

            p++;
        }

        assert( namelen <= 8 );
        assert( extlen <= 3 );
        name[ namelen ] = 0;
        ext[ extlen ] = 0;
    } //ExtractFilename

    bool IsCPMPatternMatch( const char * pattern, const char * name )
    {
        bool match = true;
        int o_pattern = 0, o_name = 0;
        while ( pattern[ o_pattern ] || name[ o_name ] )
        {
            char cp = pattern[ o_pattern ];
            char cn = name[ o_name ];

            if ( ( '?' != cp ) && ( cp != cn ) )
            {
                tracer.Trace( "  not a pattern match at offsets %d / %d\n", o_pattern, o_name );
                match = false;
                break;
            }

            if ( cp )
                o_pattern++;
            if ( cn )
                o_name++;
        }

        return match;
    } //IsCPMPatternMatch

    bool FindNextFileLinux( const char * pattern, DIR * pdir, LINUX_FIND_DATA & fd )
    {
        do
        {
            struct dirent * pent = readdir( pdir );
            if ( 0 == pent )
                return false;

            // ignore files CP/M just wouldn't understand

            if ( !ValidCPMFilename( pent->d_name ) || IsAFolder( pent->d_name ) )
                continue;

            tracer.Trace( "  FindNextFileLinux is matching '%s' with '%s'\n", pattern, pent->d_name );

            bool match = true;
            if ( strcmp( pattern, "????????.???" ) )
            {
                // cp/m patterns contain '?' (match anything including nothing), ' ' (match nothing), or literal characters

                char acName[ 9 ], acExt[ 4 ];
                char acPatternName[ 9 ], acPatternExt[ 4 ];
                ExtractFilename( pattern, acPatternName, acPatternExt );
                ExtractFilename( pent->d_name, acName, acExt );

                tracer.Trace( "  extracted pattern name '%s' ext '%s', file name '%s', ext '%s'\n", acPatternName, acPatternExt, acName, acExt );

                match = IsCPMPatternMatch( acPatternName, acName );
                if ( match )
                    match = IsCPMPatternMatch( acPatternExt, acExt );
            }

            if ( !match )
                continue;

            strcpy( fd.cFileName, pent->d_name );
            return true;
        } while ( true );

        return false;
    } //FindNextFileLinux

    DIR * FindFirstFileLinux( const char * pattern, LINUX_FIND_DATA & fd )
    {
        DIR * pdir = opendir( "." );
        tracer.Trace( "  opendir returned %p\n", pdir );
        if ( 0 == pdir )
        {
            tracer.Trace( "  opendir errno %d = %s\n", errno, strerror( errno ) );
            return 0;
        }

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

bool parse_FCB_Filename( FCB * pfcb, char * pcFilename )
{
    char * orig = pcFilename;

    // note: the high bits are used for file attributes. Mask them away

    for ( int i = 0; i < 8; i++ )
    {
        char c = ( 0x7f & pfcb->f[ i ] );
        if ( ' ' == c )
            break;
        if ( '/' == c ) // slash is legal in CP/M and MT Pascal v3.0b uses it for P2/FLT.OVL. hack it to use #
            c = '#';
        *pcFilename++ = c;
    }

    if ( ' ' != pfcb->t[0] )
    {
        *pcFilename++ = '.';

        for ( int i = 0; i < 3; i++ )
        {
            char c = ( 0x7f & pfcb->t[ i ] );
            if ( ' ' == c )
                break;
            *pcFilename++ = c;
        }
    }

    *pcFilename = 0;

    // CP/M assumes all filenames are uppercase. Linux users generally use all lowercase filenames

    if ( g_forceLowercase )
        _strlwr( orig );

    return ( pcFilename != orig );
} //parse_FCB_Filename

const char * low_address_names[] =
{
    "warm boot",
    "warm boot (bios) low",
    "warm boot (bios) high",
    "tty",
    "default drive",
    "bdos call",
    "bdos low",
    "bdos high",
};

void x80_hard_exit( const char * pcerror, uint8_t arg1, uint8_t arg2 )
{
    //dump_memory( "ntvcm_hard_exit.dmp" );
    g_consoleConfig.RestoreConsole( false );

    tracer.Trace( pcerror, arg1, arg2 );
    printf( pcerror, arg1, arg2 );
    tracer.Trace( "  %s\n", build_string() );
    printf( "  %s\n", build_string() );

    exit( 1 );
} //x80_hard_exit

#pragma warning(disable: 4100)
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
    g_emulationEnded = true;
} //x80_invoke_halt

FILE * RemoveFileEntry( char * name )
{
    for ( size_t i = 0; i < g_fileEntries.size(); i++ )
    {
        if ( !strcmp( name, g_fileEntries[ i ].acName ) )
        {
            FILE * fp = g_fileEntries[ i ].fp;
            tracer.Trace( "  removing file entry '%s'\n", name );
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
            //tracer.Trace( "  found file entry '%s': %p\n", name, g_fileEntries[ i ].fp );
            tracer.Trace( "  found file entry '%s'\n", name );
            return g_fileEntries[ i ].fp;
        }
    }

    tracer.Trace( "  could not find an open file entry for '%s'; that might be OK\n", name );
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
    if ( BDOS_GET_TIME == id )
        return "get time";
    if ( BDOS_SLEEP == id )
        return "sleep";
    if ( BDOS_INITIALIZE_RSS_FEED == id )
        return "initialize rss feed";
    if ( BDOS_FETCH_RSS_ITEM == id )
        return "fetch rss item";
    if ( BDOS_RAND == id )
        return "rand";
    if ( BDOS_ENABLE_INSTRUCTION_TRACING == id )
        return "enable/disable instruction tracing";
    if ( BDOS_GET_PUT_PROGRAM_RETURN_CODE == id )
        return "get/put program return code";
    if ( 45 == id )
        return "non - cp/m 2.2: set action on hardware error";
    if ( 48 == id )
        return "non - cp/m 2.2: empty disk buffers";
    if ( 105 == id )
        return "non - cp/m 2.2: get date and time";
    if ( 155 == id )
        return "non - cp/m 2.2: get date and time with seconds";

    return "unknown";
} //get_bdos_function

const char * get_bios_function( uint16_t id )
{
    // the ids are shifted by 3 since I put code start at 0, not -3

    id *= 3;

    if ( 0 == id )
        return "cold start";
    if ( 3 == id )
        return "warm boot (reload command processor)";
    if ( 6 == id )
        return "console status";
    if ( 9 == id )
        return "console input";
    if ( 12 == id )
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
        return "setdma: set dma address";
    if ( 39 == id )
        return "read: read a sector";
    if ( 42 == id )
        return "write: write a sector";
    if ( 45 == id )
        return "listst: status of list device";
    if ( 48 == id )
        return "sectran: sector translation for skewing";

    return "unknown";
} //get_bios_function

char kaypro_to_cp437( uint8_t c )
{
#if defined( _MSC_VER ) || defined( WATCOMDOS )

    #pragma warning(disable: 4310)

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

    if ( 0xb0 == c || 0x8c == c )
        return '-';
    if ( 0xd0 == c || 0xdf == c || 0x85 == c || 0x8a == c )
        return '+';
    if ( 0xd5 == c )
        return '|';
    if ( 0xbc == c || 0xbf == c )
        return '*';

#endif

    if ( c >= 0x80 )
        tracer.Trace( "untranslated kaypro high character %u == %02x\n", c, c );

    return c;
 } //kaypro_to_cp437

#ifdef WATCOMDOS

void append( char * pc, size_t len, char c )
{
    pc[ len ] = c;
    pc[ len + 1 ] = 0;
} //append

void match_vt100( char * pc, size_t len )
{
    static bool text_positive = true;
    assert( 0x1b == pc[ 0 ] );
    if ( len > 1 )
    {
        char last = pc[ len - 1 ];
        if ( '[' == pc[ 1 ] )
        {
            if ( ( 'H' == last ) && ( strchr( pc, ';' ) ) ) // set cursor position
            {
                // rows and columns: vt-100 is 1-based, WATCOMDOS library functions are 1-based. DOS is 0-based.

                uint8_t row = atoi( pc + 2 );
                char * pcol = strchr( pc, ';' ) + 1;
                uint8_t col = atoi( pcol );
                if ( 0 == row )
                    row = 1;
                if ( 0 == col )
                    col = 1;
                //tracer.Trace( "  vt100: setting text position to %u, %u\n", row, col );
                _settextposition( row, col );
            }
            else if ( 'm' == last ) // other display attributes
            {
                char * pnext = pc + 2;
                while ( pnext && ( 'm' != *pnext ) )
                {
                    uint8_t val = atoi( pnext );

                    if ( 0 == val )  // reset all attributes
                    {
                        //tracer.Trace( "vt100 reset text attributes\n" );
                        _settextcolor( 7 );
                        _setbkcolor( 0 );
                        text_positive = true;
                    }
                    else if ( 1 == val ) // bright / bold
                    {
                        //tracer.Trace( "vt100 text bright\n" );
                        grcolor tc = _gettextcolor();
                        if ( tc < 8 )
                        {
                            tc += 8;
                            _settextcolor( tc );
                        }
                    }
                    else if ( 2 == val ) // dim
                    {
                        //tracer.Trace( "vt100 text dim\n" );
                        grcolor tc = _gettextcolor();
                        if ( tc >= 8 )
                        {
                            tc -= 8;
                            _settextcolor( tc );
                        }
                    }
                    else if ( 3 == val || 23 == val ) // italic on/off
                    {
                        // no such feature
                    }
                    else if ( 4 == val || 24 == val ) // underline on/off
                    {
                        // no such feature
                    }
                    else if ( 5 == val || 25 == val ) // blink / unblink cursor
                    {
                        // yuck
                    }
                    else if ( 7 == val ) // reverse
                    {
                        //tracer.Trace( "vt100 text reverse\n" );
                        if ( text_positive )
                        {
                            long bk = _getbkcolor();
                            grcolor tc = _gettextcolor();
                            _settextcolor( bk );
                            _setbkcolor( tc );
                            text_positive = false;
                            //tracer.Trace( "  reverse colors bk %u text %u\n", tc, bk );
                        }
                    }
                    else if ( 22 == val ) // turn off bold and dim/faint
                    {
                        //tracer.Trace( "vt100 normal text\n" );
                        grcolor tc = _gettextcolor();
                        _setbkcolor( 0 );

                        if ( tc >= 8 )
                        {
                            tc -= 8;
                            _setbkcolor( tc );
                        }
                    }
                    else if ( 27 == val ) // positive
                    {
                        //tracer.Trace( "vt100 text positive\n" );
                        if ( !text_positive )
                        {
                            long bk = _getbkcolor();
                            grcolor tc = _gettextcolor();
                            _settextcolor( bk );
                            _setbkcolor( tc );
                            text_positive = true;
                            //tracer.Trace( "  positive colors bk %u text %u\n", tc, bk );
                        }
                    }
                    else if ( 31 == val ) // red
                        _settextcolor( 4 );
                    else if ( 32 == val ) // green
                        _settextcolor( 2 );
                    else if ( 33 == val ) // yellow
                        _settextcolor( 6 );
                    else if ( 34 == val ) // blue
                        _settextcolor( 1 );
                    else if ( 35 == val ) // magenta
                        _settextcolor( 5 );
                    else if ( 36 == val ) // cyan
                        _settextcolor( 3 );
                    else if ( 37 == val ) // white
                        _settextcolor( 7 );
                    else
                        tracer.Trace( "  vt100 ignoring display attribute ^[%um\n", val );

                    pnext = strchr( pnext, ';' );
                    if ( pnext )
                        pnext++;
                }
            }
            else if ( 'M' == last ) // delete n lines from the buffer at the current line (scroll up what's above)
            {
                short n = atoi( pc + 2 );
                if ( n > 0 )
                {
                    short x, y, dx, dy;
                    _gettextwindow( &x, &y, &dx, &dy );
                    //tracer.Trace( "original text window: %u %u %u %d\n", x, y, dx, dy );
                    struct rccoord pos = _gettextposition();
                    //tracer.Trace( "current row, col %u, %u\n", pos.row, pos.col );
                    //tracer.Trace( "deleting line by scrolling up %d rows at row %d\n", n, pos.row );
                    _settextwindow( pos.row, 1, 24, 80 ); // temporary view for the scroll
                    _scrolltextwindow( n );
                    _settextwindow( x, y, dx, dy ); // restore to the whole window
                    _settextposition( pos.row, pos.col ); // restore as the scroll moves this
                }
            }
            else if ( !strcmp( pc + 1, "[H" ) ) // home cursor
            {
                //tracer.Trace( "  vt100: home cursor\n" );
                _settextposition( 1, 1 );
            }
            else if ( !strcmp( pc + 1, "[0K" ) || !strcmp( pc + 1, "[K" ) ) // clear line from cursor to right
            {
                struct rccoord pos = _gettextposition();
                int to_clear = 81 - pos.col;
                //tracer.Trace( "  vt100: clear line from cursor to right, position %u, %u, to_clear %u\n", pos.row, pos.col, to_clear );
                char ac[ 81 ];
                memset( ac, ' ', to_clear );
                ac[ to_clear ] = 0;
                _outtext( ac );
                _settextposition( pos.row, pos.col ); // restore the cursor position
            }
            else if ( !strcmp( pc + 1, "[2J" ) ) // erase screen and home cursor
            {
                //tracer.Trace( "  vt100: erase and home cursor\n" );
                _clearscreen( _GCLEARSCREEN );
                _settextposition( 1, 1 );
            }
            else if ( !strcmp( pc + 1, "[J" ) ) // erase from current line down to bottom of screen
            {
                struct rccoord pos = _gettextposition();
                short x, y, dx, dy;
                _gettextwindow( &x, &y, &dx, &dy );
                _settextwindow( pos.row, 1, 24, 80 ); // temporary view for the clear
                _clearscreen( _GWINDOW );
                _settextwindow( x, y, dx, dy ); // restore to the whole window
                _settextposition( pos.row, pos.col ); // restore cursor position
            }
            else if ( 'L' == last ) // insert n lines at the current line (scroll down what's below)
            {
                short n = atoi( pc + 2 );
                if ( n > 0 )
                {
                    short x, y, dx, dy;
                    _gettextwindow( &x, &y, &dx, &dy );
                    struct rccoord pos = _gettextposition();
                    //tracer.Trace( "current row, col %u, %u\n", pos.row, pos.col );
                    //tracer.Trace( "inserting line by scrolling down %d rows at row %d\n", n, pos.row );
                    _settextwindow( pos.row, 1, 24, 80 ); // temporary view for the scroll
                    _scrolltextwindow( -n );
                    _settextwindow( x, y, dx, dy ); // restore to the whole window
                    _settextposition( pos.row, pos.col ); // restore as the scroll moves this
                }
            }
            else if ( 'k' == last )
            {
                tracer.Trace( "  vt100: unhandled K, full string '%s'\n", pc + 1 );
            }
            else if ( '?' == pc[ 2 ] && 'h' == last ) // cursor commands
            {
                uint8_t cmd = atoi( pc + 3 );
                if ( 7 == cmd ) // enable text wrap mode
                    _wrapon( _GWRAPON );
                else if ( 25 == cmd ) // show cursor
                    _settextcursor( 0x607 );
                else
                    tracer.Trace( "  vt100: unhandled h cursor command %u\n", cmd );
            }
            else if ( '?' == pc[ 2 ] && 'l' == last ) // cursor commands
            {
                uint8_t cmd = atoi( pc + 3 );
                if ( 7 == cmd ) // reset text wrap mode
                    _wrapon( _GWRAPOFF );
                else if ( 25 == cmd ) // hide cursor
                    _settextcursor( 0x2000 );
                else
                    tracer.Trace( "  vt100: unhandled l cursor command %u\n", cmd );
            }
            else if ( tolower( last ) >= 'a' && tolower( last ) <= 'z' )
            {
                tracer.Trace( "  vt100: unhandled termination char %c, full string '%s'\n", last, pc + 1 );
                _outtext( pc );
            }
            else
            {
                //tracer.Trace( "  no vt100 match as of yet; waiting for more characters\n" );
                return;
            }
        }
        else if ( '<' == pc[ 1 ] ) // enter ANSI mode
        {
            // (already there)
        }
        else if ( 'A' == pc[ 1 ] ) // cursor up
        {
            rccoord rc = _gettextposition();
            if ( rc.row > 1 )
                _settextposition( rc.row - 1, rc.col );
        }
        else if ( 'B' == pc[ 1 ] ) // cursor down
        {
            rccoord rc = _gettextposition();
            if (  rc.row < 24 )
                _settextposition( rc.row + 1, rc.col );
        }
        else if ( 'C' == pc[ 1 ] ) // cursor right
        {
            rccoord rc = _gettextposition();
            if ( rc.col < 80 )
                _settextposition( rc.row, rc.col + 1 );
        }
        else if ( 'D' == pc[ 1 ] ) // cursor left
        {
            rccoord rc = _gettextposition();
            if ( rc.col > 1 )
                _settextposition( rc.row, rc.col - 1 );
        }
        else if ( 'H' == pc[ 1 ] ) // cursor home
            _settextposition( 1, 1 );
        else if ( 'c' == pc[ 1 ] ) // reset terminal
        {
            _clearscreen( 0 );
            _settextposition( 1, 1 );
            _settextcolor( 7 );
            _setbkcolor( 0 );
            _settextcursor( 0x607 );
            text_positive = true;
        }
        else
        {
            tracer.Trace( "  vt100: output unhandled escape sequence doesn't start with a [: '%s'\n", pc + 1 );
            _outtext( pc );
        }

        pc[ 0 ] = 0;
    }
} //match_vt100

#endif // WATCOMDOS

void send_character( uint8_t c )
{
    #if defined( _WIN32 ) || defined( WATCOMDOS )
        if ( 10 == c )
        {
            fflush( stdout );
            _setmode( _fileno( stdout ), _O_BINARY ); // don't convert LF (10) to CR LF (13 10)
        }
    #endif

    printf( "%c", c );

    #if defined( _WIN32 ) || defined( WATCOMDOS )
        if ( 10 == c )
        {
            fflush( stdout );
            _setmode( _fileno( stdout ), _O_TEXT ); // back in text mode
        }
    #endif
} //send_character

// https://en.wikipedia.org/wiki/ANSI_escape_code              vt-100 in 1978, it existed at the same time as CP/M
// https://en.wikipedia.org/wiki/VT52                          VT52
// https://mdfs.net/Archive/info-cpm/1985/01/19/053100.htm     Kaypro II / Lear-Siegler ADM-3A

void output_character( uint8_t c )
{
    if ( g_miMode )
    {
        mi_target_character( c );
        return;
    }

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

#ifdef WATCOMDOS

    if ( !g_consoleConfig.IsOutputEstablished() )
        send_character( c );
    else if ( termVT100 == g_termEscape )
    {
        const size_t max_esc_seq = 10;
        static char esc_seq[ max_esc_seq + 1 ] = {0}; // current escape sequence
        size_t esc_len = strlen( esc_seq );

        if ( esc_len >= max_esc_seq )
        {
            tracer.Trace( "unhandled vt100 escape sequence ^%s length %zd; throwing it away\n", esc_seq + 1, esc_len );
            esc_len = 0;
            esc_seq[ 0 ] = 0;
            return;
        }

        if ( 0 != esc_len )
        {
            append( esc_seq, esc_len, c );
            match_vt100( esc_seq, esc_len + 1 );
        }
        else if ( ( 0x1b == c ) && ( 0 == esc_len ) )
            append( esc_seq, 0, c );
        else
        {
            struct rccoord pos = _gettextposition();

            if ( 8 == c ) // backspace
            {
                if ( pos.col > 1 )
                    _settextposition( pos.row, pos.col - 1 );
            }
            else if ( 0xa == c ) // LF
            {
                short row = pos.row;
                if ( 24 == row ) // last row, so scroll up and keep current row here
                    _scrolltextwindow( 1 );
                else
                    row++;

                _settextposition( row, 1 );
            }
            else if ( 0xd == c ) // CR
                _settextposition( pos.row, 1 );
            else
            {
                char ac[ 2 ] = {0};
                ac[ 0 ] = c;
                _outtext( ac );
            }
        }
    }
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
                _settextposition( s_row, col );
                tracer.Trace( "  moved cursor to %d %d\n", s_row, col );
                s_escapedY = false;
                s_row = 0xff;
            }
            return;
        }

        if ( s_escaped )
        {
            tracer.Trace( "  escape command: %c\n", c );
            struct rccoord pos = _gettextposition();
            if ( 'Y' == c )
                s_escapedY = true;
            else if ( 'A' == c )      // cursor up
            {
                if ( 1 != pos.row )
                    _settextposition( pos.row - 1, pos.col );
            }
            else if ( 'B' == c )      // cursor down
            {
                if ( 24 != pos.row )
                    _settextposition( pos.row + 1, pos.col );
            }
            else if ( 'C' == c )      // cursor right
            {
                if ( 80 != pos.col )
                    _settextposition( pos.row, pos.col + 1 );
            }
            else if ( 'D' == c )      // cursor left
            {
                if ( 1 != pos.col )
                    _settextposition( pos.row, pos.col - 1 );
            }
            else if ( 'H' == c )      // cursor home
                _settextposition( 1, 1 );
            else
                tracer.Trace( "  untranslated VT-52 command '%c' = %02x\n", printable( c ), c );

            s_escaped = false;
        }
        else if ( 0x1b == c )
        {
            s_escaped = true;
            s_escapedY = false; // just in case
            s_row = 0xff; // just in case
        }
#if 0
        else if ( 0xa == c ) // LF
        {
            struct rccoord pos = _gettextposition();
            short row = get_min( pos.row + 1, 25 );
            _settextposition( row, pos.col );
        }
#endif
        else if ( 9 == c ) // horizontal tab
        {
            struct rccoord pos = _gettextposition();
            short col = get_min( pos.col + 8, 81 );
            _settextposition( pos.row, col );
        }
        else if ( 8 == c ) // backspace
        {
            struct rccoord pos = _gettextposition();
            if ( pos.col > 1 )
                _settextposition( pos.row, pos.col - 1 );
        }
        else
        {
            char ac[ 2 ] = {0};
            ac[ 0 ] = c;
            _outtext( ac );
        }
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
                    _settextposition( s_row, col );
                    tracer.Trace( "  moved cursor to %d %d\n", s_row, col );
                    s_escapedChar = 0;
                    s_row = 0xff;
                }
            }
            else if ( 'B' == s_escapedChar )
            {
                if ( '0' == c )                    // reverse video start
                    _settextcolor( 4 );            // red
                else if ( '1' == c )               // half intensity start
                    _settextcolor( 8 );            // grey
                else if ( '4' == c )               // cursor on
                    _displaycursor( _GCURSORON );
                else
                    tracer.Trace( "unprocessed kaypro B escaped char %c\n", c );
                s_escapedChar = 0;
            }
            else if ( 'C' == s_escapedChar )
            {
                if ( '0' == c )                    // reverse video stop
                    _settextcolor( 7 );            // white
                else if ( '1' == c )               // half intensity stop
                    _settextcolor( 7 );            // white
                else if ( '4' == c )               // cursor off
                    _displaycursor( _GCURSOROFF );
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
            struct rccoord pos = _gettextposition();
            if ( 8 == c ) // cursor left
            {
                if ( 1 != pos.col )
                    _settextposition( pos.row, pos.col - 1 );
            }
            else if ( 8 == c ) // cursor right
            {
                if ( 80 != pos.col )
                    _settextposition( pos.row, pos.col + 1 );
            }
            else if ( 10 == c ) // cursor down
            {
                if ( 24 != pos.row )
                    _settextposition( pos.row + 1, pos.col );
            }
            else if ( 11 == c ) // cursor up
            {
                if ( 1 != pos.row )
                    _settextposition( pos.row - 1, pos.col );
            }
            else if ( 12 == c ) // cursor right
            {
                if ( 80 != pos.col )
                    _settextposition( pos.row, pos.col + 1 );
            }
            else if ( 23 == c ) // erase to end of screen
            {
                tracer.Trace( "  erase to end of screen, current: %u, %u\n", pos.row, pos.col );
                short x, y, dx, dy;
                _gettextwindow( &x, &y, &dx, &dy );
                _settextwindow( pos.row, 1, 24, 80 ); // temporary view for the clear
                _clearscreen( _GWINDOW );
                _settextwindow( x, y, dx, dy ); // restore to the whole window
                _settextposition( pos.row, pos.col ); // restore cursor position
            }
            else if ( 24 == c ) // erase to end of line
            {
                tracer.Trace( "  erase to end of line, current: %u, %u\n", pos.row, pos.col );
                int to_clear = 81 - pos.col;
                char ac[ 81 ];
                memset( ac, ' ', to_clear );
                ac[ to_clear ] = 0;
                _outtext( ac );
                _settextposition( pos.row, pos.col ); // restore the cursor position
            }
            else if ( 26 == c ) // clear screen, home cursor
            {
                tracer.Trace( "clear screen, home cursor\n" );
                _clearscreen( _GCLEARSCREEN );
                _settextposition( 1, 1 );
            }
            else if ( 30 == c ) // home cursor
                _settextposition( 1, 1 );
            else if ( 127 == c ) // del: back + space + back
            {
                if ( pos.col > 1 )
                    _settextposition( pos.row, pos.col - 1 );
            }
            else
                tracer.Trace( "ignored character in kaypro escape range: %02x\n", c );
        }
        else
        {
            char ac[ 2 ] = {0};
            ac[ 0 ] = c;
            _outtext( ac );
        }
    }
#else // Windows and Linux
    if ( termVT100 == g_termEscape )
        send_character( c );
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
                tracer.Trace( "  untranslated VT-52 command '%c' = %02x\n", printable( c ), c );
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
            send_character( c );
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
            send_character( c );
    }
#endif
} //output_character

uint8_t map_input( uint8_t input )
{
    uint8_t output = input;

#if defined(_MSC_VER) || defined(WATCOMDOS)
    // On Windows, input is  0xe0 for standard arrow keys and 0 for keypad equivalents
    // On DOS/WATCOM, input is 0 for both cases

    if ( 0 == input || 0xe0 == input )
    {
        uint8_t next = (uint8_t) ConsoleConfiguration::portable_getch();

        // map various keys to ^ XSEDCRG used in many apps.

        if ( 'K' == next )                   // left arrow
            output = 1 + 'S' - 'A';
        else if ( 'P' == next )              // down arrow
            output = 1 + 'X' - 'A';
        else if ( 'M' == next )              // right arrow
            output = 1 + 'D' - 'A';
        else if ( 'H' == next )              // up arrow
            output = 1 + 'E' - 'A';
        else if ( 'Q' == next )              // page down
            output = 1 + 'C' - 'A';
        else if ( 'I' == next )              // page up
            output = 1 + 'R' - 'A';
        else if ( 'S' == next )              // del maps to ^h
            output = 1 + 'H' - 'A';
        else
            tracer.Trace( "  no map_input mapping for %02x, second character %02x\n", input, next );

        tracer.Trace( "    next character after %02x: %02x == '%c' mapped to %02x\n", input, printable( next ), output );
    }
#else // Linux / MacOS
    if ( 0x1b == input )
    {
        if ( g_consoleConfig.portable_kbhit() )
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
                else if ( '5' == nextb )         // page up
                {
                    uint8_t nextc = g_consoleConfig.portable_getch();
                    tracer.Trace( "  5 nextc: %02x\n", nextc );
                    if ( '~' == nextc )
                        output = 1 + 'R' - 'A';
                }
                else if ( '6' == nextb )         // page down
                {
                    uint8_t nextc = g_consoleConfig.portable_getch();
                    tracer.Trace( "  6 nextc: %02x\n", nextc );
                    if ( '~' == nextc )
                        output = 1 + 'C' - 'A';
                }
                else if ( '3' == nextb )         // DEL on linux
                {
                    uint8_t nextc = g_consoleConfig.portable_getch();
                    tracer.Trace( "  3 nextc: %02x\n", nextc );
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
    else if ( 0x7f == input && !g_backspaceToDel ) // linux gives 0x7f DEL when a user presses backspace BS 0x08 on the keyboard. Most CP/M apps like Wordstar don't want this
        output = 0x08; // BS
#endif
    else if ( g_backspaceToDel && 0x08 == input )
        output = 0x7f;

    return output;
} //map_input

char get_next_kbd_char()
{
    if ( g_fileInputOffset < g_fileInputText.size() )
        return g_fileInputText[ g_fileInputOffset++ ];

    return (char) ConsoleConfiguration::portable_getch();
} //get_next_kbd_char

bool is_kbd_char_available()
{
    if ( g_fileInputOffset < g_fileInputText.size() )
        return true;

    if ( g_sleepOnKbdLoop )
        return g_consoleConfig.throttled_kbhit();

    return g_consoleConfig.portable_kbhit();
} //is_kbd_char_available

bool cpm_read_console( char * buf, size_t bufsize, uint8_t & out_len )
{
    char ch = 0;
    out_len = 0;

    while ( out_len < (uint8_t) bufsize )
    {
        ch = map_input( get_next_kbd_char() );
        tracer.Trace( "  get_next_kbd_char read character %02x -- '%c'\n", ch, printable( ch ) );

        // behavior per https://techtinkering.com/articles/cpm-standard-console-control-characters/

        if ( ( 3 == ch ) && ( 0 == out_len ) ) // ^c exit the currently running app in CP/M if it's the first character in the buffer
            return true;

        if ( '\n' == ch || '\r' == ch ) // all done; send response
            break;

        if ( 5 == ch ) // ^e. move cursor to beginning of next line without sending line to be processed or adding a newline to the buffer
            printf( "\n" );
        else if ( 0x7f == ch || 8 == ch ) // ^h backspace / rubout / delete
        {
            if ( out_len > 0 )
            {
                printf( "\x8 \x8" );
                out_len--;
            }
        }
        else if ( 0x10 == ch ) // ^p. start echoing to the printer. ignore
            continue;
        else if ( 0x12 == ch ) // ^r. emits a '#' then retypes the current line after a new line
        {
            printf( "#\n" );
            for ( char i = 0; i < out_len; i++ )
                send_character( buf[ i ] );
        }
        else if ( 0x15 == ch ) // ^u. write '#', discard current line, and move to next line for input
        {
            printf( "#\n" );
            out_len = 0;
        }
        else if ( 0x18 == ch ) // ^x. removes all characters typed so far and starts again
        {
            for ( char i = 0; i < out_len; i++ )
                printf( "\x8 \x8" );
            out_len = 0;
        }
        else
        {
            send_character( ch );
            buf[ out_len++ ] = ch;
        }

        fflush( stdout );
    }

    return false;
} //cpm_read_console

void set_bdos_status()
{
    // Calls to bdos in CP/M 2.2 can trash any register, so this is generally safe.
    // CP/M 2.2 generally mandates L = A on return.
    // HiSoft C v3.09 and the apps it generates only look at HL for bdos results, not A or L.
    // So for HiSoft, H must be cleared so 16-bit HL checks just get the result in L.
    // But Andre Adrian's Sargon chess fails if h is modified. (the tested fix in that app is to push and pop H in the character input macro)

    reg.l = reg.a;
    reg.b = 0;
    if ( g_clearHOnBDOSReturn )
        reg.h = 0;
} //set_bdos_status

uint16_t days_since_jan1_1978()
{
    time_t current_time;
    struct tm *time_info;

    current_time = time( NULL );
    time_info = localtime( & current_time );

    struct tm target_date = {0};
    target_date.tm_year = 1978 - 1900; // Years since 1900
    target_date.tm_mon = 0;            // January (0-indexed)
    target_date.tm_mday = 1;
    target_date.tm_hour = 0;
    target_date.tm_min = 0;
    target_date.tm_sec = 0;

    time_t target_time = mktime(&target_date);
    time_t difference_seconds = current_time - target_time;
    uint16_t days_since_1978 = (uint16_t) ( difference_seconds / ( 24 * 60 * 60 ) );
    return days_since_1978;
} //days_since_jan1_1978

void WriteRandom()
{
    FCB * pfcb = (FCB *) ( memory + reg.D() );
    pfcb->Trace();
    reg.a = 6; // seek past end of disk

    char acFilename[ CPM_FILENAME_LEN ];
    bool ok = parse_FCB_Filename( pfcb, acFilename );
    if ( ok )
    {
        FILE * fp = FindFileEntry( acFilename );
        if ( fp )
        {
            uint16_t record = pfcb->GetRandomIOOffset();
            uint32_t file_offset = (uint32_t) record * (uint32_t) 128;

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
                    tracer.TraceBinaryData( g_DMA, 128, 2 );
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

    set_bdos_status();
} //WriteRandom

#ifdef WATCOMDOS

uint16_t daysSince1978( struct dosdate_t & date )
{
    uint16_t days = 0;
    int i;

    for ( i = 1978; i < date.year; i++ )
        days += (i % 4 == 0 && i % 100 != 0 || i % 400 == 0) ? 366 : 365; // Account for leap years

    uint16_t daysInMonths[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}; // Non-leap year days
    if ( ( date.year % 4 == 0 && date.year % 100 != 0 || date.year % 400 == 0 ) && date.month > 2 )
        daysInMonths[2] = 29; // Leap year
    for ( i = 1; i < date.month; i++ )
        days += daysInMonths[i];

    days += date.day;
    return days;
} //daysSince1978

#endif

// must return one of OPCODE_NOP or OPCODE_RET

uint8_t x80_invoke_hook()
{
    static uint64_t kbd_poll_busyloops = 0;
    uint16_t address = reg.pc - 1; // the emulator has moved past this instruction already

    if ( address >= BIOS_FUNCTIONS && address < ( BIOS_FUNCTIONS + BIOS_FUNCTION_COUNT ) )
    {
        uint16_t bios_function = address - BIOS_FUNCTIONS;
        tracer.Trace( "bios function %#x: %u, %s, bc %04x, de %04x, hl %04x\n",
                      address, bios_function, get_bios_function( bios_function ), reg.B(), reg.D(), reg.H() );

        switch( bios_function )
        {
            case 0: // cold start; exit the app
            case 1: // warm boot; exit the app
            {
                x80_end_emulation();
                g_emulationEnded = true;
                return OPCODE_NOP;
            }
            case 2: // const console status. A=0 if nothing available, A=0xff if a keystroke is available
            {
                if ( is_kbd_char_available() )
                    reg.a = 0xff;
                else
                    reg.a = 0;
                break;
            }
            case 3: // conin. wait until the keyboard has a character and return it in a.
            {
                uint8_t input = (uint8_t) get_next_kbd_char();
                tracer.Trace( "  conin got %02xh from getch()\n", input );
                reg.a = map_input( input );
                tracer.Trace( "  conin is returning %02xh = '%c'\n", reg.a, printable( reg.a ) );
                break;
            }
            case 4: // conout. write the chracter in c to the screen
            {
                char ch = reg.c;
                tracer.Trace( "  bios console out: %02x == '%c'\n", ch, printable( ch ) );
                output_character( reg.c );
                fflush( stdout );
                break;
            }
            case 5: // list. Write character in c to the printer. If the printer isn't ready, wait until it is.
            case 6: // punch / auxout. Write the character in c to the paper tape punch.
                break;
            case 7: // reader. read a character from the paper tape or other auxilliary device. Return in a.
            {
                // Wait until a chracter is ready. Return ^z if not implemented.
                reg.a = 26;
                break;
            }
            default:
            {
                tracer.Trace( "unhandled BIOS CODE!!!!!!!!!!!!!!!: %#x = %d\n", address, bios_function );
                printf( "unhandled bios code!!!!!!!!!!!!!!! %#x = %u\n", address, bios_function );
                //x80_hard_exit( "invalid bios call address %#x = %u", address, bios_function );
                break;
            }
        }

        kbd_poll_busyloops = 0;
        return OPCODE_RET;
    }

    if ( ( BDOS_ENTRY + 3 ) != address )
    {
        tracer.Trace( "hook call, but not bios or bdos. likely just a mov h, h. address: %04x\n", address );
        return OPCODE_NOP;
    }

    uint8_t function = reg.c;
    tracer.Trace( "bdos function %d: %s, bc %04x, de %04x, hl %04x\n", function, get_bdos_function( function ), reg.B(), reg.D(), reg.H() );
    char acFilename[ CPM_FILENAME_LEN ];

    if ( ( 12 != reg.c ) && ( ( 6 != reg.c ) || ( 0xff != reg.e ) ) ) // hisoft c apps busy loop on get version (12) and console i/o (6)
        kbd_poll_busyloops = 0;

    // Generally, only BDOS calls called by apps I tested are implemented.

    switch( reg.c )
    {
        case 0:
        {
            // system reset. end execution of the app.
            x80_end_emulation();
            g_emulationEnded = true;
            return OPCODE_NOP;
        }
        case 1:
        {
            // console input. echo input to console
            uint8_t ch = (uint8_t) get_next_kbd_char();
            tracer.Trace( "  bdos console in got next kbd char: %02x == '%c'\n", ch, printable( ch ) );
            reg.a = map_input( ch );
            set_bdos_status();
            tracer.Trace( "  bdos console in: %02x == '%c'\n", ch, printable( ch ) );
            output_character( ch );
            fflush( stdout );
            break;
        }
        case 2:
        {
            // console output
            // CP/M 2.2 checks for ^s and ^q to pause and resume output. If output is paused due to ^s,
            // a subsequent ^c terminates the application. ^q resumes output then ^c has no effect.
            uint8_t ch = reg.e;
            tracer.Trace( "  bdos console output: %02x == '%c'\n", ch, printable( ch ) );
            output_character( ch );
            fflush( stdout );
            break;
        }
        case 3:
        {
            // reader input. aka raw console input. I haven't found an app that uses this yet.
            uint8_t ch = (uint8_t) get_next_kbd_char();
            reg.a = map_input( ch );
            set_bdos_status();
            tracer.Trace( "  bdos reader input / raw console in: %02x == '%c'\n", ch, printable( ch ) );
            break;
        }
        case 4:
        {
            // punch output. paper tape punch. aux or non-standard peripheral. redirect to the console
            uint8_t ch = reg.e;
            tracer.Trace( "  bdos punch output: %02x == '%c'\n", ch, printable( ch ) );
            output_character( ch );
            fflush( stdout );
            break;
        }
        case 5:
        {
            // list output. this generally means printer output. redirect to the console. the plmx compiler sends error info here by default
            uint8_t ch = reg.e;
            tracer.Trace( "  bdos list output: %02x == '%c'\n", ch, printable( ch ) );
            output_character( ch );
            fflush( stdout );
            break;
        }
        case 6:
        {
            // direct console I/O
            // e = ff means input -- return char in a if available or return 0 otherwise
            // e != ff means output that character

            if ( 0xff == reg.e )
            {
                if ( is_kbd_char_available() )
                {
                    kbd_poll_busyloops = 0;
                    uint8_t input = (uint8_t) get_next_kbd_char();
                    tracer.Trace( "  read character %u == %02x == '%c'\n", input, input, printable( input ) );
                    reg.a = map_input( input );
                }
                else
                {
                    if ( kbd_poll_busyloops > 20 )
                    {
                        // some apps like forth and multiplan call this in a busy loop.
                        // don't sleep every call because sometimes they alternate calling this with updating the display.
                        // other apps like nevada basic poll for input as they interpret apps which makes them run slowly.

                        if ( g_sleepOnKbdLoop )
                        {
                            sleep_ms( 1 );
                            tracer.Trace( "  sleeping in direct console i/o\n" );
                        }

                        kbd_poll_busyloops = 0;
                    }
                    else
                        kbd_poll_busyloops++;
                    reg.a = 0;
                }

                set_bdos_status();
            }
            else
            {
                uint8_t ch = reg.e;
                tracer.Trace( "  bdos direct console i/o output: %u == %02x == '%c'\n", ch, ch, printable( ch ) );
                output_character( ch );
                fflush( stdout );
            }

            break;
        }
        case 7:
        {
            // get I/O byte
            //      Bits     Bits 6,7    Bits 4,5    Bits 2,3    Bits 0,1
            //      Device   LIST        PUNCH       READER      CONSOLE
            //    Value
            //       00      TTY:        TTY:        TTY:        TTY:
            //       01      CRT:        PTP:        PTR:        CRT:
            //       10      LPT:        UP1:        UR1:        BAT:
            //       11      UL1:        UP2:        UR2:        UC1:

            reg.a = memory[ 3 ]; // list = tty, punch = tty, reader = tty, console = tty
            set_bdos_status();
            break;
        }
        case 9:
        {
            // print string terminated by a dollar sign $. string is pointed to by DE

            uint16_t i = reg.D();
            uint32_t count = 0;

            while ( '$' != memory[i] )
            {
                if ( count++ > 2000 ) // arbitrary limit, but probably a bug if this long
                {
                    tracer.Trace( "  ERROR: String to print is too long!\n", stderr );
                    break;
                }

                uint8_t ch = memory[ i++ ];
                output_character( ch );
                fflush( stdout );
            }

            tracer.TraceBinaryData( memory + reg.D(), count, 4 );
            break;
        }
        case 10:
        {
            // read console buffer. aka buffered console input. DE: buffer address. buffer:
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
                    x80_end_emulation();
                    g_emulationEnded = true;
                    return OPCODE_NOP;
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
            // get console status. return A=0 if no characters are waiting or non-zero if a character is waiting

            if ( is_kbd_char_available() )
                reg.a = 0xff;
            else
                reg.a = 0;

            set_bdos_status(); // required for HiSoft C v3.09 and its C runtime. And lu.com.
            break;
        }
        case 12:
        {
            // return version number

            reg.h = 0; // 8080, plain CP/M
            reg.b = reg.h;
            reg.l = 0x22;   // version 2.2
            reg.a = reg.l;
            break;
        }
        case 13:
        {
            // reset disks. returns 0xff if a file exists that starts with a $ or 0 otherwise

            reg.a = 0;
            set_bdos_status();
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

            set_bdos_status();
            break;
        }
        case 15:
        {
            // open file. return 255 in a if file not found and 0..3 directory code otherwise

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
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
                    pfcb->cr = 0;
                    pfcb->SetRecordCount( fp ); // Whitesmith C 2.1 requires this to be set
                    // don't reset extent on a re-open or LK80.COM will fail. pfcb->ex = 0;
                    pfcb->s2 = 0;
                    tracer.Trace( "  open used existing file and rewound to offset 0\n" );
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

                        // Digital Research's lk80.com linker has many undocumented expectations no other apps I've tested have.
                        // including rc > 0 after an open. Whitesmith C requires the record count set correctly.

                        pfcb->SetRecordCount( fp );

                        // cr and ex can't be zeroed or whitesmith pascal's lnk produces corrupt .com files
                        // pfcb->cr = 0;
                        // pfcb->ex = 0;
                        pfcb->s2 = 0;
                        tracer.Trace( "  file opened successfully, record count: %u\n", pfcb->rc );
                    }
                    else
                        tracer.Trace( "ERROR: can't open file '%s' error %d = %s\n", acFilename, errno, strerror( errno ) );
                }
            }
            else
                tracer.Trace( "ERROR: can't parse filename in FCB\n" );

            set_bdos_status();
            break;
        }
        case 16:
        {
            // close file. return 255 on error and 0..3 directory code otherwise

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
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
                {
                    // return error 255 if the file doesn't exist.
                    // Pro Pascal Compiler v2.1 requires a 0 return code for a file that's not open but exists.

                    if ( file_exists( acFilename ) )
                    {
                        tracer.Trace( "    WARNING: file close on file that's not open but exists\n" );
                        reg.a = 0;
                    }
                    else
                        tracer.Trace( "ERROR: file close on file that's not open and doesn't exist\n" );
                }
            }
            else
                tracer.Trace( "ERROR: can't parse filename in close call\n" );

            set_bdos_status();
            break;
        }
        case 17:
        {
            // search for first. Use the FCB in de and write directory entries to the DMA address, then point to
            // which of those entries is the actual one (0-3) or 0xff for not found in A.
            // Find First on CP/M has a side-effect of flushing data to disk. Aztec C relies on this and calls
            // Find First at exit() to ensure the disk is flushed, though it does close all files first.

            fflush( 0 );
            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                tracer.Trace( "  search for first match of '%s'\n", acFilename );
                CloseFindFirst();

#ifdef _MSC_VER
                BOOL found = FALSE;
                WIN32_FIND_DATAA fd = {0};
                g_hFindFirst = FindFirstFileA( acFilename, &fd );
                if ( INVALID_HANDLE_VALUE != g_hFindFirst )
                {
                    do
                    {
                        if ( ValidCPMFilename( fd.cFileName ) && ! IsAFolder( fd.cFileName ) )
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
                    CloseFindFirst();
                    tracer.Trace( "WARNING: find first file couldn't find a single match\n" );
                }
#elif defined(WATCOMDOS)
                CloseFindFirst();
                g_FindFirst.name[ 0 ] = 0;
                unsigned result = _dos_findfirst( acFilename, _A_NORMAL, &g_FindFirst );
                tracer.Trace( "result of _dos_findfirst: %u, filename %s\n", result, ( 0 == result ) ? & g_FindFirst.name[0] : "n/a" );
                if ( 0 == result )
                {
                    g_FindActive = true;
                    ParseFoundFile( g_FindFirst.name );
                    reg.a = 0;
                }
#else
                LINUX_FIND_DATA fd = {0};
                g_FindFirst = FindFirstFileLinux( acFilename, fd );
                if ( 0 != g_FindFirst )
                {
                    ParseFoundFile( fd.cFileName );
                    reg.a = 0;
                }
                else
                    tracer.Trace( "WARNING: find first file failed, error %d = %s\n", errno, strerror( errno ) );
#endif
            }
            else
                tracer.Trace( "ERROR: can't parse filename for search for first\n" );

            set_bdos_status();
            break;
        }
        case 18:
        {
            // search for next. Use the FCB in de and write directory entries to the DMA address, then point to
            // which of those entries is the actual one (0-3) or 0xff for not found in A.

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
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
                            if ( ValidCPMFilename( fd.cFileName ) && !IsAFolder( fd.cFileName ) )
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
                        CloseFindFirst();
                    }
                }
                else
                    tracer.Trace( "ERROR: search for next without a prior successful search for first\n" );
#elif defined(WATCOMDOS)
                if ( g_FindActive )
                {
                    g_FindFirst.name[ 0 ] = 0;
                    unsigned result = _dos_findnext( &g_FindFirst );
                    tracer.Trace( "result of _dos_findnext: %u, filename %s\n", result, ( 0 == result ) ? & g_FindFirst.name[0] : "n/a" );
                    if ( 0 == result )
                    {
                        ParseFoundFile( g_FindFirst.name );
                        reg.a = 0;
                    }
                    else
                    {
                        tracer.Trace( "WARNING: find next file found no more\n" );
                        CloseFindFirst();
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
                        tracer.Trace( "WARNING: find next file found no more, error %d = %s\n", errno, strerror( errno ) );
                        CloseFindFirst();
                    }
                }
                else
                    tracer.Trace( "ERROR: search for next without a prior successful search for first\n" );
#endif
            }
            else
                tracer.Trace( "ERROR: can't parse filename for search for first\n" );

            set_bdos_status();
            break;
        }
        case 19:
        {
            // delete file. return 255 if file not found and 0..3 directory code otherwise

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
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

                int removeok = ( 0 == unlink( acFilename ) );
                tracer.Trace( "  attempt to remove/delete/unlink file '%s' result ok: %d\n", acFilename, removeok );
                if ( removeok )
                    reg.a = 0;
                else
                    tracer.Trace( "  error %d = %s\n", errno, strerror( errno ) );
            }
            else
                tracer.Trace( "ERROR: can't parse filename for delete file\n" );

            set_bdos_status();
            break;
        }
        case 20:
        {
            // read sequential. return 0 on success or non-0 on failure:
            // reads 128 bytes from cr of the extent and increments cr.
            // if cr overflows, the extent is incremented and cr is set to 0 for the next read

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    // apps like Digital Research's linker LK80 update pcb->cr and pfcb->ex between
                    // calls to sequential read and effectively does random reads using sequential I/O.
                    // This is illegal (the doc says apps can't touch this data, but that ship has sailed and sunk).

                    uint32_t file_size = portable_filelen( fp );
                    uint32_t curr = pfcb->GetSequentialOffset();
                    uint16_t dmaOffset = (uint16_t) ( g_DMA - memory );
#ifdef WATCOMDOS
                    tracer.Trace( "  file size: %#lx = %lu, current %#lx = %lu, dma %#x = %u\n",
                                  file_size, file_size, curr, curr, dmaOffset, dmaOffset );
#else
                    tracer.Trace( "  file size: %#x = %u, current %#x = %u, dma %#x = %u\n",
                                  file_size, file_size, curr, curr, dmaOffset, dmaOffset );
#endif

                    if ( curr < file_size )
                    {
                        fseek( fp, curr, SEEK_SET );

                        uint32_t to_read = get_min( file_size - curr, (uint32_t) 128 );
                        memset( g_DMA, g_readFileFillValue, 128 );

                        size_t numread = fread( g_DMA, 1, to_read, fp );
                        if ( numread > 0 )
                        {
                            tracer.TraceBinaryData( g_DMA, 128, 2 );
                            reg.a = 0;
                            pfcb->UpdateSequentialOffset( curr + 128 );
                        }
                        else
                        {
                            reg.a = 1;
                            tracer.Trace( "  read error %d = %s, so returning a = 1\n", errno, strerror( errno ) );
                        }
                    }
                    else
                    {
                        reg.a = 1;
                        tracer.Trace( "  at the end of file, so returning a = 1\n" );
                    }
                }
                else
                    tracer.Trace( "ERROR: can't read from a file that's not open\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse filename in read sequential file\n" );

            set_bdos_status();
            break;
        }
        case 21:
        {
            // write sequential. return 0 on success or non-0 on failure (out of disk space)
            // reads 128 bytes from cr of the extent and increments cr.
            // if cr overflows, the extent is incremented and cr is set to 0 for the next read

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( fp )
                {
                    uint32_t file_size = portable_filelen( fp );
                    uint32_t curr = pfcb->GetSequentialOffset();
                    uint16_t dmaOffset = (uint16_t) ( g_DMA - memory );
                    tracer.Trace( "  writing at offset %#x = %u, file size is %#x = %u, dma %#x = %u\n",
                                  curr, curr, file_size, file_size, dmaOffset, dmaOffset );
                    fseek( fp, curr, SEEK_SET );

                    tracer.TraceBinaryData( g_DMA, 128, 2 );
                    size_t numwritten = fwrite( g_DMA, 128, 1, fp );
                    if ( numwritten > 0 )
                    {
                        reg.a = 0;
                        pfcb->UpdateSequentialOffset( curr + 128 );
                        pfcb->SetRecordCount( fp ); // Whitesmith C v2.1's A80.COM assembler requires this
                    }
                    else
                        tracer.Trace( "ERROR: fwrite returned %zd, errno %d = %s\n", numwritten, errno, strerror( errno ) );
                }
                else
                    tracer.Trace( "ERROR: can't write to a file that's not open\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse filename in write sequential file\n" );

            set_bdos_status();
            break;
        }
        case 22:
        {
            // make file. return 255 if out of space or the file exists. 0..3 directory code otherwise.
            // "the Make function has the side effect of activating the FCB and thus a subsequent open is not necessary."

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
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

                    pfcb->cr = 0;
                    pfcb->rc = 0;
                    pfcb->ex = 0;
                    pfcb->s2 = 0;

                    tracer.Trace( "  successfully created fp %p for write\n", fp );
                    reg.a = 0;
                }
                else
                    tracer.Trace( "ERROR: unable to make file\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse filename in make file\n" );

            set_bdos_status();
            break;
        }
        case 23:
        {
            // rename file. 0 for success, non-zero otherwise.

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
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
                    tracer.Trace( "ERROR: can't parse new filename in rename\n" );
            }
            else
                tracer.Trace( "ERROR: can't parse old filename in rename\n" );

            set_bdos_status();
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
            set_bdos_status();
            break;
        }
        case 26:
        {
            // set the dma address (128 byte buffer for doing I/O)

            tracer.Trace( "  updating DMA address; D %u = %#x\n", reg.D(), reg.D() );
            g_DMA = memory + reg.D();
            break;
        }
        case 27:
        {
            // get addr (alloc) point the allocation vector at 0-filled memory

            reg.h = 0xff;
            reg.l = 0x80;
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

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 255;
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

            set_bdos_status();
            break;
        }
        case 31:
        {
            // get DPB address. Get Disk Parameter Block. Return the address in HL

            reg.h = DPB_OFFSET_HI;
            reg.l = DPB_OFFSET_LO;
            break;
        }
        case 32:
        {
            // get/set current user

            reg.a = 0;
            set_bdos_status();
            break;
        }
        case 33:
        {
            // read random

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 6; // seek past end of disk
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                FILE * fp = FindFileEntry( acFilename );
                if ( !fp )
                {
                    // dxforth 4.56 relies on this cp/m 2.2 behavior: open / close / read should work.
                    tracer.Trace( "  in random read but the file isn't opened. trying to open it now\n" );
                    fp = fopen( acFilename, "r+b" );
                    if ( fp )
                    {
                        FileEntry fe;
                        strcpy( fe.acName, acFilename );
                        fe.fp = fp;
                        g_fileEntries.push_back( fe );
                        tracer.Trace( "  file opened successfully for random read\n" );
                    }
                    else
                        tracer.Trace( "  file open failed for random read\n" );
                }
                if ( fp )
                {
                    uint32_t record = pfcb->GetRandomIOOffset();
                    tracer.Trace( "  read random record %u == %#x\n", record, record );
                    uint32_t file_offset = (uint32_t) record * (uint32_t) 128;
                    memset( g_DMA, g_readFileFillValue, 128 );

                    uint32_t file_size = portable_filelen( fp );

                    // OS workaround for app bug: Turbo Pascal expects a read just past the end of file to succeed.

                    if ( file_size == file_offset )
                    {
                        tracer.Trace( "  random read at eof, offset %u\n", file_size );
                        reg.a = 1;
                        break;
                    }

                    if ( file_size > file_offset )
                    {
                        uint32_t to_read = get_min( file_size - file_offset, (uint32_t) 128 );
                        ok = !fseek( fp, file_offset, SEEK_SET );
                        if ( ok )
                        {
                            tracer.Trace( "  reading random at offset %u == %#x. file size %u, to read %u\n", file_offset, file_offset, file_size, to_read );
                            size_t numread = fread( g_DMA, 1, to_read, fp );
                            if ( numread )
                            {
                                tracer.TraceBinaryData( g_DMA, to_read, 2 );
                                reg.a = 0;

                                // The CP/M spec says random read should set the file offset such
                                // that the following sequential I/O will be from the SAME location
                                // as this random read -- not 128 bytes beyond.

                                pfcb->UpdateSequentialOffset( file_offset );
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

            set_bdos_status();
            break;
        }
        case 34:
        {
            // write random

            WriteRandom();
            break;
        }
        case 35:
        {
            // Compute file size. A = 0 if ok, 0xff on failure. Sets r2 to 0 and r0/r1 to the number of 128 byte records

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 0xff;
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                //pfcb->r2 = 0; // only supported in cp/m post version 2.2

                FILE * fp = FindFileEntry( acFilename );
                bool found = false;
                if ( fp )
                    found = true;
                else
                    fp = fopen( acFilename, "r+b" );

                if ( fp )
                {
                    uint32_t file_size = portable_filelen( fp );
                    file_size = round_up( file_size, (uint32_t) 128 ); // cp/m files round up in 128 byte records
                    pfcb->SetRandomIOOffset( (uint16_t) ( file_size / 128 ) );
                    reg.a = 0;
                    tracer.Trace( "  file size is %u == %u records; r1 %#x r0 %#x\n", file_size, file_size / 128, pfcb->r1, pfcb->r0 );

                    if ( !found )
                        fclose( fp );
                }
                else
                    tracer.Trace( "ERROR: compute file size can't find file '%s'\n", acFilename );
            }

            set_bdos_status();
            break;
        }
        case 36:
        {
            // Set Random Record. no documented return code. I'm using A=0 for success and A=ff for failure

            FCB * pfcb = (FCB *) ( memory + reg.D() );
            pfcb->Trace();
            reg.a = 0xff;
            bool ok = parse_FCB_Filename( pfcb, acFilename );
            if ( ok )
            {
                // unused in cp/m 2.2: pfcb->r2 = 0;

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
            else
                tracer.Trace( "ERROR: set random record can't parse filename\n" );

            set_bdos_status();
            break;
        }
        case 37: // called by BASIC/Z
        {
            // selectively reset disc drives. DE = bitmap of drives to reset. result: A=0 if OK and ffh if error
            reg.a = 0;
            set_bdos_status();
            break;
        }
        case 40:
        {
            // write random with zero fill.
            // Just like 34 / write random except also fills any file-extending blocks with 0.
            // The 34 implementation of WriteRandom already fills with zeros, so just use that.
            // I haven't found any apps that call this, so I can't say it's really tested.

            WriteRandom();
            break;
        }
        case 45: // called by BASIC/Z
        {
            // non - CP/M 2.2: set action on hardware error
            reg.a = 0;
            set_bdos_status();
            break;
        }
        case 48: // called by various apps
        {
            // non - CP/M 2.2: drv_flush - empty disk buffers
            reg.a = 0;
            set_bdos_status();
            break;
        }
        case 102:
        {
            // Get file date and time (not in cp/m 2.2)
            break;
        }
        case 105: // get date time. cp/m 3.0 and later. Returns seconds in A as packed bcd
        case 155: // Get date and time. MP/M, Concurrent CP/M. called by rmcobol v1.5
        {
            CPM3DateTime * ptime = (CPM3DateTime *) ( memory + reg.D() );
#ifdef WATCOMDOS
            struct dostime_t time;
            _dos_gettime( &time );

            ptime->hour = packBCD( (uint8_t) time.hour );
            ptime->minute = packBCD( (uint8_t) time.minute );
            reg.a = packBCD( (uint8_t) time.second );

            struct dosdate_t date;
            _dos_getdate( &date );
            ptime->day = 1 + daysSince1978( date ); // on cp/m 3.0, jan 1 1978 is day 1.
#else
            time_t time_now;
            time( & time_now );
            struct tm * plocal = localtime( & time_now );

            ptime->day = 1 + days_since_jan1_1978(); // on cp/m 3.0, jan 1 1978 is day 1.
            ptime->hour = packBCD( (uint8_t) plocal->tm_hour );
            ptime->minute = packBCD( (uint8_t) plocal->tm_min );
            reg.a = packBCD( (uint8_t) plocal->tm_sec );
#endif

            if ( 155 == reg.c )
                ptime->second = reg.a;

#ifdef TARGET_BIG_ENDIAN
            ptime->swap_endian();
#endif

            set_bdos_status();
            break;
        }
        case BDOS_GET_TIME:
        {
            // non-standard BDOS call GetTime. DE points to a CPMTime structure

            CPMTime * ptime = (CPMTime *) ( memory + reg.D() );
#ifdef WATCOMDOS
            struct dostime_t time;
            _dos_gettime( &time );

            ptime->hour = time.hour;
            ptime->minute = time.minute;
            ptime->second = time.second;
            ptime->millisecond = time.hsecond;
#elif defined( WATCOMLINUX ) // old-school Linux
            struct timespec ts;
            clock_gettime( CLOCK_REALTIME, & ts );
            long ms =  ( ts.tv_nsec + ( ms_per_ns / 2 ) ) / ms_per_ns;
            struct tm * plocal = localtime( & ts.tv_sec );

            ptime->hour = (uint16_t) plocal->tm_hour;
            ptime->minute = (uint16_t) plocal->tm_min;
            ptime->second = (uint16_t) plocal->tm_sec;
            ptime->millisecond = (uint16_t) ( ms / 10 ); // hundredths of a second;
#else // modern Linux and Windows
            system_clock::time_point now = system_clock::now();
            uint64_t ms = duration_cast<milliseconds>( now.time_since_epoch() ).count() % 1000;
            time_t time_now = system_clock::to_time_t( now );
            struct tm * plocal = localtime( & time_now );

            ptime->hour = (uint16_t) plocal->tm_hour;
            ptime->minute = (uint16_t) plocal->tm_min;
            ptime->second = (uint16_t) plocal->tm_sec;
            ptime->millisecond = (uint16_t) ( ms / 10 ); // hundredths of a second;
#endif

#ifdef TARGET_BIG_ENDIAN
            ptime->swap_endian();
#endif
            set_bdos_status();
            break;
        }
        case BDOS_SLEEP:
        {
            // non-standard BDOS call sleep. DE contains a count of milliseconds 0-32767

            uint16_t ms = reg.D();
            sleep_ms( ms );
            break;
        }
#ifdef NTVCM_RSS_SUPPORT
        case BDOS_INITIALIZE_RSS_FEED:
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
        case BDOS_FETCH_RSS_ITEM:
        {
            // non-standard BDOS call load rss item. DE is the item to load. DMA points to
            // a 2048 byte buffer of the form name0title0description0

            char * buf = (char *) g_DMA;
            uint16_t item = reg.D();
            reg.a = g_rssFeed.fetch_rss_item( item, buf, 2048 );
            break;
        }
#endif //NTVCM_RSS_SUPPORT
        case BDOS_RAND:
        {
            // non-standard BDOS call puts a random number in A
            // also put random values in hl because the Eco-C C Compiler _bdos function assumes return values are there

            reg.a = (uint8_t) rand();
            reg.h = reg.a;
            reg.l = (uint8_t) rand();
            break;
        }
        case BDOS_ENABLE_INSTRUCTION_TRACING:
        {
            // non-standard BDOS call: enable/disable tracing if DE is non-zero/zero
            x80_trace_instructions( 0 != reg.D() );
            break;
        }
        case BDOS_GET_PUT_PROGRAM_RETURN_CODE:
        {
            // This is not a CP/M v2.2 function. It is in CP/M v3 and it's so handy that it's here too
            // if DE = 0xffff then return current code in HL
            // otherwise, set the current code to the value in DE

            if ( 0xffff == reg.D() )
                reg.SetH( g_exitCode );
            else
            {
                g_exitCodeSet = true;
                g_exitCode = reg.D();
                tracer.Trace( "  app exit code set to %u\n", g_exitCode );
            }
            break;
        }
        default:
        {
            tracer.Trace( "unhandled BDOS FUNCTION!!!!!!!!!!!!!!!: %u = %#x\n", reg.c, reg.c );
            printf( "unhandled BDOS FUNCTION!!!!!!!!!!!!!!!: %u = %#x\n", reg.c, reg.c );

            // CP/M 2.2 mandates returning a 0 status for function numbers that are out of range.

            reg.a = 0;
            set_bdos_status();
            break;
        }
    }

    return OPCODE_RET;
} //x80_invoke_hook

void usage()
{
    printf( "Usage: ntvcm [-?] [-c] [-g:<file>] [-p] [-s:X] [-t] <command> [arg1] [arg2]\n" );
    printf( "       ntvcm --interpreter=mi  (GDB/MI mode for VS Code cppdbg)\n" );
} //usage

void help()
{
    usage();
    printf ("A CP/M 2.2 emulator.\n\n");
    printf( "  -b        backspace/BS/0x08 key sends delete/DEL/0x7f.\n" );
    printf( "            (for use with Turbo Pascal).\n" );
    printf( "  -c        never auto-detect ESC characters and change\n" );
    printf( "            to to 80x24 mode.\n" );
#if defined( _WIN32 )  // Windows only
    printf( "  -C        always switch to 80x24 mode.\n" );
#endif
    printf( "  -d        don't clear the display on exit when in 80x24 mode.\n" );
    printf( "  -f:<file> specify an input file containing keystrokes.\n" );
    printf( "  -g:<file> write a per-PC execution-count profile (CSV) at exit.\n" );
    printf( "  -h        don't clear the h register after bdos calls.\n" );
    printf( "              (Andre Adrian's Sargon chess app requies -h)\n" );
    printf( "              (HiSoft C v3.09 fails with -h)\n" );
    printf( "              (most apps work either way)\n" );
    printf( "  -i        trace 8080/Z80 instructions when tracing with -t.\n" );
    printf( "  -k        translate Kaypro II extended characters to ASCII\n" );
    printf( "            equivalents.\n" );
    printf( "  -l        force CP/M filenames to be lowercase.\n" );
    printf( "  -n        don't sleep for apps in tight bdos 6 loops. (Use\n" );
    printf( "            with apps like nvbasic).\n" );
    printf( "  -p        show performance information at app exit.\n" );
    printf( "  -r:X      fill for file reads past EOF 0..255. default 26 ^z.\n" );
    printf( "  -s:X      specify clock speed in Hz.\n" );
    printf( "            defaults to 0 which is as fast as possible.\n" );
    printf( "  -t        enable debug tracing to ntvcm.log.\n" );
    printf( "  -V        display version and exit.\n" );
    printf( "  -v:X      translate escape sequences to VT-100\n" );
    printf( "            where X can be one of:\n" );
    printf( "     5:     for vt-52 escape sequences (use with CalcStar etc)\n" );
    printf( "     k:     for Kaypro II/Lear-Siegler ADM-3A escape sequences.\n" );
    printf( "            (use with strtrk).\n" );
    printf( "  -z:X      applies X as a hex mask to SetProcessAffinityMask.\n" );
    printf( "              e.g.:\n" );
    printf( "              /z:11    2 performance cores on an i7-1280P\n" );
    printf( "              /z:3000  2 efficiency cores on an i7-1280P\n" );
    printf( "              /z:11    2 random good cores on a 5950x\n" );
    printf( "  -8        use 8080 instruction set, not Z80\n\n" );
    printf( "  e.g. to assemble, load, and run test.asm:\n" );
    printf( "            ntvcm asm.com test\n" );
    printf( "            ntvcm load.com test\n" );
    printf( "            ntvcm test.com\n\n" );
    printf( "  e.g. to run test.com at 4MHz:\n" );
    printf( "            ntvcm -s:4000000 test\n\n" );
    printf( "  e.g. to run Star Trek in mbasic in 80x24 mode using i8080 emulation:\n" );
    printf( "            ntvcm -8 -C mbasic startrek.bas\n\n" );
    exit( 0 );
} //help

void version()
{
#ifdef NDEBUG
    const char * flavor = "Release";
#else
    const char * flavor = "Debug";
#endif
    printf("%s: Version %s%s%s %s Compiled: ", FILENAME, VERSION, BUILD, COMMIT_ID, flavor );
    if ( ' ' == __DATE__[4] )
        printf( "0%c %c%c%c %s %s\n", __DATE__[5], __DATE__[0], __DATE__[1], __DATE__[2], &__DATE__[7], __TIME__ );
    else
        printf( "%c%c %c%c%c %s %s\n", __DATE__[4], __DATE__[5], __DATE__[0], __DATE__[1], __DATE__[2], &__DATE__[7], __TIME__ );
} //version

void error( char const * perr = 0 )
{
    g_consoleConfig.RestoreConsole( false );
    if ( perr )
        printf( "error: %s\n", perr );
    usage();
    exit( -1 );
} //error

static void load_input_file_text( const char * file_path )
{
    FILE * fp = fopen( file_path, "rb" );
    if ( !fp )
        error( "-f file not found" );
    CFile thefile( fp );

    size_t file_size = portable_filelen( fp );
    vector<char> original;
    original.resize( file_size );
    bool ok = ( 1 == fread( original.data(), file_size, 1, fp ) );
    if ( !ok )
    {
        printf( "can't read from text input file '%s', error %d = %s\n", file_path, errno, strerror( errno ) );
        error( "can't read from -f file" );
    }

    // remove any line feeds. stop at ^z. pass through CR and TAB. fail on other non-alpha characters.

    for ( size_t cur = 0; cur < file_size; cur++ )
    {
        char c = original[ cur ];
        if ( ( c >= 0x20 && c <= 0x7e ) || ( 13 == c || 9 == c ) ) // normal, CR, TAB
            g_fileInputText.push_back( c );
        else if ( 0x1a == c ) // ^z
            break;
        else if ( 10 != c ) // line feed
        {
            tracer.Trace( "input file has byte %02x at offset %zd\n", c, cur );
            error( "-f input file can't contain binary data" );
        }
    }
} //load_input_file_text

bool valid_cpm_8_3_token( const char * tok )
{
    // Validate a single filename token against CP/M's 8.3 naming rules:
    // an optional drive prefix "X:" (X = A..P), a base name of at most 8
    // characters, and an optional type of at most 3 characters after a
    // single dot.

    // optional drive prefix "X:"

    if ( 0 != tok[ 0 ] && ':' == tok[ 1 ] )
    {
        char drive = (char) toupper( (unsigned char) tok[ 0 ] );
        if ( drive < 'A' || drive > 'P' )
            return false;
        tok += 2;
    }

    if ( 0 == *tok ) // a bare drive such as "B:" with no filename
        return false;

    const char * dot = strchr( tok, '.' );
    size_t name_len = dot ? (size_t) ( dot - tok ) : strlen( tok );
    if ( name_len > 8 )
        return false;

    if ( dot )
    {
        const char * ext = dot + 1;
        if ( strchr( ext, '.' ) ) // only a single dot is allowed
            return false;
        if ( strlen( ext ) > 3 )
            return false;
    }

    return true;
} //valid_cpm_8_3_token

bool validate_assembler_command_tail( const char * tail, char * offending, size_t offending_size )
{
    // M80/L80 (and similar CP/M assemblers/linkers) parse the command tail
    // themselves and silently corrupt memory or crash when handed a filename
    // whose base is longer than 8 characters or whose type is longer than 3.
    // Validate each filename token before launching the app. Tokens are
    // separated by spaces, commas, and '=' (e.g. "OBJ,LIST=SOURCE"). An L80
    // switch group like "/E" or "/P:100" is skipped, and switches appended to
    // a filename ("PROG/N/E") are ignored when validating the name.

    static const char * separators = " \t,=";

    while ( *tail )
    {
        while ( 0 != *tail && strchr( separators, *tail ) )
            tail++;
        if ( 0 == *tail )
            break;

        const char * start = tail;
        while ( 0 != *tail && !strchr( separators, *tail ) )
            tail++;

        char token[ 130 ];
        size_t len = (size_t) ( tail - start );
        if ( len >= sizeof( token ) )
            len = sizeof( token ) - 1;
        memcpy( token, start, len );
        token[ len ] = 0;

        if ( '/' == token[ 0 ] ) // an L80 switch group, not a filename
            continue;

        char * slash = strchr( token, '/' ); // strip any trailing L80 switches
        if ( slash )
            *slash = 0;

        if ( 0 == token[ 0 ] )
            continue;

        if ( !valid_cpm_8_3_token( token ) )
        {
            strncpy( offending, token, offending_size - 1 );
            offending[ offending_size - 1 ] = 0;
            return false;
        }
    }

    return true;
} //validate_assembler_command_tail

bool write_fcb_arg( FCB * arg, char * pc )
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
        memcpy( & ( arg->f ), pc, get_min( (size_t) 8, (size_t) ( dot - pc ) ) );
        memcpy( & ( arg->t ), dot + 1, get_min( (size_t) 3, strlen( dot + 1 ) ) );
    }
    else
        memcpy( & ( arg->f ), pc, get_min( (size_t) 8, strlen( pc ) ) );

    return true;
} //write_fcb_arg

static bool load_file( char const * file_path, long & file_size, void * buffer )
{
    bool ok = false;
    FILE * fp = fopen( file_path, "rb" );
    if ( 0 != fp )
    {
        file_size = portable_filelen( fp );

        if ( ( file_size + 0x100 ) > BDOS_ENTRY ) // save room for initial 0x100 + bdos + bios. This doesn't account for BSS; app startup code should do that.
        {
            printf( "file size %#08lx, BDOS_ENTRY %#04x\n", file_size, BDOS_ENTRY );
            error( "the input file is too large to fit in memory" );
        }

        ok = ( 1 == fread( buffer, file_size, 1, fp ) );
        fclose( fp );
        tracer.Trace( "loading success: %d, read %ld == 0x%lx bytes, addresses 256 == 0x100 through %ld == 0x%lx\n",
                      ok, file_size, file_size, file_size + 0x100, file_size + 0x100 );
    }

    return ok;
} //load_file

#ifdef TARGET_BIG_ENDIAN
static void setmword( uint16_t offset, uint16_t value ) { * (uint16_t *) & memory[ offset ] = flip_endian16( value ); }
#else
static void setmword( uint16_t offset, uint16_t value ) { * (uint16_t *) & memory[ offset ] = value; }
#endif

int main( int argc, char * argv[] )
{
    try
    {
        bump_thread_priority(); // for performance benchmarking only

        memset( memory, 0, sizeof( memory ) - 1 ); // -1 for 16-bit systems
        memory[ sizeof( memory ) - 1 ] = 0; // again, for 16-bit systems
        memset( &reg, 0, sizeof( reg ) );
        reg.fZ80Mode = true;

        for ( int i = 1; i < argc; i++ )
            if ( !strncmp( argv[ i ], "--interpreter=mi", 16 ) || !strncmp( argv[ i ], "-interpreter=mi", 15 ) )
                g_miMode = true;

        if ( g_miMode )
        {
            if ( !mi_allocate_state() )
                error( "unable to allocate debugger state" );
            mi_wait_for_program();
        }

        char * pCommandTail = (char *) memory + COMMAND_TAIL_OFFSET;
        char * pCommandTailLen = (char *) memory + COMMAND_TAIL_LEN_OFFSET;
        char * pcCOM = g_miMode ? g_miProgram : 0;
        char * pcArg1 = 0;
        char * pcArg2 = 0;
        char * pfileInputText = 0;
        bool trace = false;
        bool traceInstructions = false;
        const char * pfileProfile = 0;
        uint64_t clockrate = 0;
        bool showPerformance = false;
        bool force80x24 = false;
        bool clearDisplayOnExit = true;
        uint64_t processAffinityMask = 0; // by default let the OS decide

        for ( int i = 1; i < argc; i++ )
        {
            char *parg = argv[i];
            char c = *parg;

            if ( g_miMode )
                continue;

            // linux shell scripts pass carriage returns '\r' at the end of strings for DOS-style cr/lf files

            char * pR = strchr( parg, '\r' );
            if ( 0 != pR )
                *pR = 0;

            // append arguments past the .com file to the command tail

            if ( 0 != pcCOM )
            {
                size_t tailLen = strlen( pCommandTail ) + strlen( parg ) + 1 + 1; // +1 null termination +1 space
                if ( tailLen > 127 )
                    error( "command length is too long for the 127 char limit in CP/M" );

                // CP/M puts a space at the start of non-zero-length command tails. Also, add a space between arguments.

                strcat( pCommandTail, " " );
                strcat( pCommandTail, parg );
                strupr( pCommandTail );
            }

            if ( 0 == pcCOM && ( '-' == c
#if defined( WATCOMDOS ) || defined( _WIN32 )
                || '/' == c
#endif
                ) )
            {
                char ca = (char)( parg[1] );

                if ( 'V' == ca )
                {
                    version();
                    printf("License CC0 1.0 Universal: See <https://creativecommons.org/publicdomain/zero/1.0/>.\n");
                    exit(0);
                }
#if defined( _WIN32 )  // Windows only
                else if ( 'C' == parg[1] ) // MT - moved other wise option would be converted to lower case before it was tested
                    force80x24 = true;
#endif
                else // Try to match a lower case options
                {
#if defined( _WIN32 )  // Command line options are case sensitive on Linux/NetBSD...
                    ca = (char) tolower( ca );
#endif
                    if ( '?' == ca )
                        help();
                    else if ( '8' == ca )
                        reg.fZ80Mode = false;
                    else if ( 'b' == parg[1] )
                        g_backspaceToDel = true;
                    else if ( 'c' == parg[1] )
                        g_forceConsole = true;
                    else if ( 'd' == ca )
                        clearDisplayOnExit = false;
                    else if ( 'f' == ca )
                    {
                        if ( ( ':' != parg[2] ) || !strlen( parg + 3 ) )
                            error( ":<filename> expected with -f" );

                        pfileInputText = parg + 3;
                    }
                    else if ( 'g' == ca )
                    {
                        if ( ':' == parg[2] )
                        {
                            if ( !strlen( parg + 3 ) )
                                error( ":<filename> expected with -g" );

                            pfileProfile = parg + 3;
                        }
                        else if ( 0 == parg[2] )
                        {
                            if ( ++i >= argc )
                                error( ":<filename> expected with -g" );

                            pfileProfile = argv[i];
                        }
                        else
                            error( ":<filename> expected with -g" );
                    }
                    else if ( 'h' == ca )
                        g_clearHOnBDOSReturn = false;
                    else if ( 'i' == ca )
                        traceInstructions = true;
                    else if ( 'k' == ca )
                        g_kayproToCP437 = true;
                    else if ( 'l' == ca )
                        g_forceLowercase = true;
                    else if ( 'n' == ca )
                        g_sleepOnKbdLoop = false;
                    else if ( 'p' == ca )
                        showPerformance = true;
                    else if ( 'r' == ca )
                    {
                        if ( ':' == parg[2] )
                            g_readFileFillValue = (uint8_t) strtoul( parg + 3 , 0, 10 );
                        else
                            error( "colon required after r argument" );
                    }
                    else if ( 's' == ca )
                    {
                        if ( ':' == parg[2] )
                            clockrate = strtoull( parg + 3 , 0, 10 );
                        else
                            error( "colon required after s argument" );
                    }
                    else if ( 't' == ca )
                        trace = true;
                    else if ( 'v' == parg[1] )
                    {
                        if ( ':' != parg[2] )
                            error( "colon required after v argument" );

                        if ( 'k' == tolower( parg[3] ) )
                            g_termEscape = termKayproII;
                        else if ( '5' == parg[3] )
                            g_termEscape = termVT52;
                        else
                            error( "invalid terminal emulation identifier. Only 5 and k are supported" );
                    }
                    else if ( 'z' == ca )
                    {
                        if ( ':' == parg[2] )
                            processAffinityMask = strtoull( parg + 3 , 0, 16 );
                        else
                            error( "colon required after z argument" );
                    }
                    else
                        error( "invalid argument specified" );
                }
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

        if ( g_miMode && g_miArguments[ 0 ] )
        {
            snprintf( pCommandTail, 128, " %s", g_miArguments );
            strupr( pCommandTail );
        }

        tracer.Enable( trace, L"ntvcm.log", true );
        tracer.SetQuiet( true );
        tracer.SetFlushEachTrace( true );
        x80_trace_instructions( traceInstructions );

        if ( pfileProfile )
            x80_profile_enable( true );

        if ( pfileInputText )
        {
            load_input_file_text( pfileInputText );
            tracer.Trace( "-f input file has %ld characters\n", g_fileInputText.size() );
        }

        if ( 0 != processAffinityMask )
            set_process_affinity( processAffinityMask );

        if ( 0 == pcCOM )
        {
            error( "no CP/M command specified" );
            assume_false;
        }

        * pCommandTailLen = (char) strlen( pCommandTail );
        tracer.Trace( "command tail len %d value: '%s'\n", *pCommandTailLen, pCommandTail );

        char acCOM[ MAX_PATH ] = {0};
        strcpy( acCOM, pcCOM );

        if ( !file_exists( acCOM ) )
        {
            if ( ends_with( acCOM, ".com" ) )
                error( "can't find command file" );
            else
            {
                strcat( acCOM, ".com" );
                if ( !file_exists( acCOM ) )
                {
                    strcpy( acCOM, pcCOM );
                    strcat( acCOM, ".COM" );       // for case-sensitive file systems

                    if ( !file_exists( acCOM ) )
                        error( "can't find command file" );
                }
            }
        }

        // Cowgol's tokenise.com requires 0-fill after soft EOF.

        if ( ( ends_with( acCOM, "tokenise.com" ) ) && ( 0x1a == g_readFileFillValue ) )
            g_readFileFillValue = 0;

        // The Pascal/Z compiler's pasopt.com optimizer has a bug that depends on uninitialized RAM
        // being set to a non-zero value. The location is -30eH bytes from the BDOS address.
        // Specifically, execution is like this:
        //   pc 0db7, op 5e, op2 7b, op3 fe, op4 00, a 00, B 0001, D 009f, H fbee, ix f0dd, iy fe9f, sp f0db, SzYHXvnC, ld e,(hl)
        //   pc 0db8, op 7b, op2 fe, op3 00, op4 c2, a 00, B 0001, D 0000, H fbee, ix f0dd, iy fe9f, sp f0db, SzYHXvnC, ld a,e
        //   pc 0db9, op fe, op2 00, op3 c2, op4 ff, a 00, B 0001, D 0000, H fbee, ix f0dd, iy fe9f, sp f0db, SzYHXvnC, cp 00h
        //   pc 0dbb, op c2, op2 ff, op3 0d, op4 af, a 00, B 0001, D 0000, H fbee, ix f0dd, iy fe9f, sp f0db, sZyhxvNc, jp nz,0dffh
        // The byte at 0xfbee is uninitialized RAM and must be non-zero for the app to work.
        // Otherwise pasopt.com goes into an infinite loop writing junk to the output file.

        if ( ends_with( acCOM, "pasopt.com" ) )
            memory[ BDOS_ENTRY - 0x30e ] = 0xff; // address 0xfbee for NTVCM's BDOS address. the value is arbitrary non-zero.

        // M80/L80 parse the command tail themselves and silently crash on filenames
        // that aren't CP/M 8.3 compliant. Validate before launching so the user gets
        // a clear message instead of a hang or crash.

        if ( ends_with( acCOM, "m80.com" ) || ends_with( acCOM, "l80.com" ) )
        {
            char offending[ 130 ];
            if ( !validate_assembler_command_tail( pCommandTail, offending, sizeof( offending ) ) )
            {
                char msg[ 256 ];
                snprintf( msg, sizeof( msg ),
                          "'%s' is not a valid CP/M 8.3 filename (max 8-character name, 3-character type)", offending );
                error( msg );
            }
        }

        // setup command-line arguments

        FCB * arg1 = (FCB *) ( memory + FCB_ARG1_OFFSET );
        FCB * arg2 = (FCB *) ( memory + FCB_ARG2_OFFSET );
        memset( & ( arg1->f ), ' ', 11 ); // 8 filename + 3 type
        memset( & ( arg2->f ), ' ', 11 );

        if ( pcArg1 )
        {
            _strupr( pcArg1 );
            write_fcb_arg( arg1, pcArg1 );

            if ( pcArg2 )
            {
                _strupr( pcArg2 );
                write_fcb_arg( arg2, pcArg2 );
            }
        }

        tracer.Trace( "fcb argument 1:\n" );
        arg1->Trace( true );
        tracer.Trace( "fcb argument 2:\n" );
        arg2->Trace( true );

        // make memory look like CP/M 2.2. The first 8-byte interrupt vector has this:

        memory[0] = OPCODE_JMP;    // jump to warm boot, which likely just exits ntvcm unless overridden by an app.
        memory[1] = 3 + BIOS_JUMP_TABLE_LO; // low byte of BIOS jump table. boot is at -3 from this address. wboot is here.
        memory[2] = BIOS_JUMP_TABLE_HI;     // high byte of BIOS jump table
        memory[3] = 0;             // use TTY: for console, READER, PUNCH, and LIST
        memory[4] = 0;             // default drive 0 == A
        memory[5] = OPCODE_JMP;    // jump to the BDOS entry point unless overridden by an app
        memory[6] = BDOS_ENTRY_LO; // these two bytes also point to the first byte above app-available RAM (reserved RAM)
        memory[7] = BDOS_ENTRY_HI;

        // The real bios function table is a list of 3-byte entries containing jmp and the address of
        // each of the 16 bios functions (17 including the -1 entry to exit).
        // Here, just hook the jmp instruction and put a pointer to the hook opcode in each address.
        // Apps like mbasic.com don't call bios functions; they take the address from the vector and
        // call it directly to save a tiny bit of performance.
        // The memory map is here (giving apps a little more RAM than standard CP/M):
        //   0000-003f: CP/M global storage + RST hardware interrupt service routines. 8 entries are 8 bytes each
        //   0040-00ff: CP/M global storage
        //   0100-????: App run space growing upward until it collides with the stack
        //   ????-fefb: Stack growing downward until it collides with the app
        //   fefa-fefb: two bytes of 0 so apps can return instead of a standard app exit.
        //   fefc-fefe: BDOS_ENTRY. JMP to feff for BDOS calls. Where addresses 5-7 jumps to. Hook here breaks WordStar Spellcheck.
        //   feff-feff: OPCODE_HOOK stored here to call back to C code for BDOS calls
        //   ff00-ff33: BIOS_JUMP_TABLE. bios jump table of 3*17 bytes. (0xff03 is stored at addess 0x1).
        //   ff40-ff50: BIOS_FUNCTIONS. where bios jump table addresses point, filled with OPCODE_HOOK.
        //   ff60-ff6f: DPD_OFFSET. filled with the Disk Parameter Block for BDOS call 31 Get DPB.
        //   ff70-ffff: unused, filled with 0
        //
        // On a typical CP/M machine:
        //   0000-003f: RST hardware interrupt service routines. 8 entries are 8 bytes each
        //   0040-00ff: CP/M global storage
        //   0100-????: app space
        //   ????-e3a9: stack given to apps at start.
        //   e406-????: bdos
        //   f200-????: bios (0xf203 stored at address 1)
        //
        //   On the Z80-MBC2 machine:
        //   ????-d9a9: stack given to apps at start.
        //   da06-????: bdos
        //   e800-????: bios (0xe803 stored at address 1)

        // can't put an OPCODE_HOOK at BDOS_ENTRY because apps like WordStar's spellcheck copy this code elsewhere

        memory[ BDOS_ENTRY ] = OPCODE_JMP;                  // jump
        memory[ BDOS_ENTRY + 1 ] = BDOS_ENTRY_LO + 3;       // ..
        memory[ BDOS_ENTRY + 2 ] = BDOS_ENTRY_HI;           // ..
        memory[ BDOS_ENTRY + 3 ] = OPCODE_HOOK;             // to here
        memset( memory + BIOS_FUNCTIONS, OPCODE_HOOK, BIOS_FUNCTION_COUNT );

        // fill the BIOS jump table to jmp to unique addresses containing OPCODE_HOOK

        for ( uint16_t v = 0; v < BIOS_FUNCTION_COUNT; v++ )
        {
            uint16_t entryOffset = BIOS_JUMP_TABLE + ( 3 * v );
            memory[ entryOffset ] = OPCODE_JMP;
            setmword( entryOffset + 1, BIOS_FUNCTIONS + v );
        }

        long file_size = 0;
        bool ok = load_file( acCOM, file_size, memory + 0x100 );
        if ( !ok )
        {
            printf( "unable to load command %s\n", acCOM );
            exit( 1 );
        }

        // this old Chess app fails if H is set to 0 during BDOS calls. Apparently it only ever ran on eumulators.

        if ( ends_with( acCOM, "sargon.com" ) )
            g_clearHOnBDOSReturn = false;

        // Use made-up numbers that look believable enough to a CP/M app checking for free disk space. 107k.

        DiskParameterBlock * pdpb = (DiskParameterBlock *) ( memory + DPB_OFFSET );
        pdpb->spt = 128;
        pdpb->bsh = 3;
        pdpb->blm = 7;
        pdpb->exm = 7;
        pdpb->dsm = 127;
        pdpb->drm = 1;
        pdpb->al0 = 0xf0;
        pdpb->al1 = 0;
        pdpb->cks = 64;
        pdpb->off = 0;

#ifdef TARGET_BIG_ENDIAN
        pdpb->swap_endian();
#endif

        reg.powerOn();               // set default values of registers
        reg.pc = 0x100;
        reg.sp = BDOS_ENTRY - 2;     // the stack is written to below this address. 2 bytes here are zero for ret from app
        reg.a = reg.b = reg.c = reg.d = reg.e = reg.h = reg.l = 0;
        reg.fp = reg.ap = 0;         // apparently this is expected by CPUTEST
        reg.bp = reg.cp = reg.dp = reg.ep = reg.hp = reg.lp = 0;
        reg.ix = reg.iy = 0;

        if ( g_miMode )
            x80_debug_enable( true );

        if ( trace )
        {
            x80_trace_state();
            tracer.Trace( "starting execution of app '%s' size %ld\n", acCOM, file_size );
        }

        if ( force80x24 && !g_miMode )
            g_consoleConfig.EstablishConsoleOutput( 80, 24 );

        if ( !g_miMode )
        {
            ConsoleConfiguration::ConvertRedirectedLFToCR( true );
            g_consoleConfig.MakeKeyboardInputRaw(); // needed for non-Windows
        }

        CPUCycleDelay delay( clockrate );

#ifdef WATCOMDOS
        uint32_t tStart = DosTimeInMS();
#elif defined( WATCOMLINUX )
        uint64_t tStart = LinuxTimeInMS();
#else
        high_resolution_clock::time_point tStart = high_resolution_clock::now();
#endif

        uint64_t total_cycles = 0;
        if ( g_miMode )
        {
            MIAction action = mi_action_none;
            bool stopReported = false;
            bool debuggerExit = false;

            printf( "=thread-group-started,id=\"i1\",pid=\"1\"\n=thread-created,id=\"1\",group-id=\"i1\"\n" );
            fflush( stdout );

            while ( !g_emulationEnded && !debuggerExit )
            {
                if ( x80_debug_stopped() )
                {
                    if ( !stopReported && x80_debug_reason() == x80_debug_stop_breakpoint &&
                         !mi_prepare_breakpoint_stop() )
                    {
                        x80_debug_continue();
                        continue;
                    }
                    if ( !mi_source_step_complete() )
                    {
                        x80_debug_step();
                        continue;
                    }
                    g_miSourceStepMode = mi_source_step_none;
                    if ( !stopReported )
                    {
                        mi_emit_stop();
                        stopReported = true;
                    }

                    if ( !mi_read_command( true, &action ) || action == mi_action_exit )
                    {
                        debuggerExit = true;
                        break;
                    }
                    if ( action == mi_action_run )
                        stopReported = false;
                }
                else
                {
                    total_cycles += x80_emulate( 10000 );
                    if ( g_emulationEnded || x80_debug_stopped() )
                        continue;

                    while ( mi_read_command( false, &action ) )
                    {
                        if ( action == mi_action_exit )
                        {
                            debuggerExit = true;
                            break;
                        }
                        if ( x80_debug_stopped() )
                            break;
                    }
                }
            }

            if ( g_emulationEnded )
            {
                printf( "*stopped,reason=\"exited-normally\"\n=thread-exited,id=\"1\",group-id=\"i1\"\n=thread-group-exited,id=\"i1\",exit-code=\"0\"\n" );
                fflush( stdout );
            }
        }
        else
        {
            do
            {
                total_cycles += x80_emulate( 10000 );

                if ( g_emulationEnded )
                    break;

                delay.Delay( total_cycles );
            } while ( true );
        }

#ifdef WATCOMDOS
        uint32_t tDone = DosTimeInMS();
#elif defined( WATCOMLINUX )
        uint64_t tDone = LinuxTimeInMS();
#else
        high_resolution_clock::time_point tDone = high_resolution_clock::now();
#endif

        g_consoleConfig.RestoreConsole( clearDisplayOnExit );

        CloseFindFirst();

        if ( pfileProfile )
        {
            const uint64_t * counts = x80_profile_counts();
            if ( 0 != counts )
            {
                FILE * fp = fopen( pfileProfile, "w" );
                if ( 0 != fp )
                {
                    uint32_t addr;
                    fprintf( fp, "pc,count,asm\n" );
                    for ( addr = 0; addr < 65536; ++addr )
                        if ( 0 != counts[ addr ] )
                            fprintf( fp, "%u,%llu,%s\n", addr, (unsigned long long) counts[ addr ],
                                     x80_render_operation( (uint16_t) addr ) );
                    fclose( fp );
                }
                else
                {
                    g_profileWriteFailed = true;
                    printf( "unable to create profile file '%s': %s\n", pfileProfile, strerror( errno ) );
                }
            }
        }

        if ( showPerformance )
        {
            char ac[ 100 ];
            printf( "\n" );
#ifdef WATCOMDOS
            uint32_t elapsedMS = tDone - tStart;
#elif defined( WATCOMLINUX )
            uint32_t elapsedMS = (uint32_t) ( tDone - tStart );
#else
            uint32_t elapsedMS = (uint32_t) duration_cast<std::chrono::milliseconds>( tDone - tStart ).count();
#endif
            printf( "elapsed milliseconds: %16s\n", CDJLTrace::RenderNumberWithCommas( elapsedMS, ac ) );
            printf( "%s cycles:      %20s\n", reg.fZ80Mode ? "Z80 " : "8080", CDJLTrace::RenderNumberWithCommas( total_cycles, ac ) );

            printf( "clock rate: " );
            if ( 0 == clockrate )
            {
                printf( "      %20s\n", "unbounded" );
                uint64_t total_ms = total_cycles / ( reg.fZ80Mode ? 4000 : 2000 );
                if ( reg.fZ80Mode )
                    printf( "approx ms at 4Mhz: %19s", CDJLTrace::RenderNumberWithCommas( total_ms, ac ) );
                else
                    printf( "approx ms at 2Mhz: %19s", CDJLTrace::RenderNumberWithCommas( total_ms, ac ) );

                uint16_t days = (uint16_t) ( total_ms / 1000 / 60 / 60 / 24 );
                uint16_t hours = (uint16_t) ( ( total_ms % ( (uint32_t) 1000 * 60 * 60 * 24 ) ) / 1000 / 60 / 60 );
                uint16_t minutes = (uint16_t) ( ( total_ms % ( (uint32_t) 1000 * 60 * 60 ) ) / 1000 / 60 );
                if ( 0 != days || 0 != hours || 0 != minutes )
                {
                    uint16_t seconds = (uint16_t) ( ( total_ms % ( (uint32_t) 1000 * 60 ) ) / 1000 );
                    uint64_t milliseconds = ( ( total_ms % 1000 ) );
                    printf( " == %u days, %u hours, %u minutes, %u seconds, %llu milliseconds", days, hours, minutes, seconds, milliseconds );
                }
                printf( "\n" );
            }
            else
                printf( "      %20s Hz\n", CDJLTrace::RenderNumberWithCommas( clockrate, ac ) );
        }
    }
    catch ( bad_alloc & e )
    {
        printf( "caught exception bad_alloc -- out of RAM. If in RVOS or ARMOS use -h or -m to add RAM. %s\n", e.what() );
    }
    catch ( exception & e )
    {
        printf( "caught a standard execption: %s\n", e.what() );
    }
    catch( ... )
    {
        printf( "caught a generic exception\n" );
    }

#ifdef NTVCM_RSS_SUPPORT
    g_rssFeed.clear();
#endif //NTVCM_RSS_SUPPORT

    fflush( stdout );
    tracer.Shutdown();
    return g_haltExecuted ? -1 : g_profileWriteFailed ? 1 : g_exitCodeSet ? (int) g_exitCode : 0;
} //main


