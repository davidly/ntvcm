// Native PDP-10 runner for the recovered PL/M-80 4.1, MAC80 and INT80 programs,
// adapted from ntv8008's plm8/pdp10.cxx.
//
// A direct translation of pdp10.py's PDP10.execute and its monitor calls; the
// Python runner remains the reference. pdp10.py validates and loads the
// compiler image, then passes the loaded memory and entry point here.
//
// usage: pdp10 [--ki] memory.bin entry input.txt directory output.txt budget [final.bin]
// --ki selects KI10 stack and AOBJ arithmetic, which counts the halves separately.
// Exit 0 when the program EXITs; otherwise 2 with the error on stderr. The
// console output produced so far is written to output.txt either way, and the
// optional final.bin receives the final memory followed by the PC (for tests).
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

typedef uint64_t Word;
static const Word MASK = ( (Word) 1 << 36 ) - 1;
static const Word RH = ( (Word) 1 << 18 ) - 1;
static const Word SIGN = (Word) 1 << 35;

static Word m[ 1 << 18 ];
static uint32_t pc;
static std::string output;
static std::string input;
static size_t inputAt = 0;
static std::string directory;
// Disk buffers, one 01000-word area per channel, sit above every image's
// code and data (INT80E's reaches 0101032).
static const uint32_t OUT_BUFFERS = 0700000;
static const uint32_t IN_BUFFERS = 0740000;

static bool ki10 = false;

// Add delta to both halves: carrying across them on a KA10, not on a KI10.
static Word step( Word word, int delta )
{
    if ( ki10 )
        return ( ( ( word >> 18 ) + delta ) & RH ) << 18 | ( ( word + delta ) & RH );
    return ( word + (Word) ( delta * 01000001LL ) ) & MASK;
}

struct Finished {};

struct Channel
{
    Word mode = 0;
    uint32_t out = 0, in = 0;
    FILE * file = nullptr;
    bool hasLines = false;
    std::vector<std::string> lines;
    size_t nextLine = 0;
    long long written = 0, flushed = 0;
    bool ready = false;
};

static std::map<int, Channel> channels;
static std::vector<int> channelOrder; // Python dict insertion order

static int64_t sgn( Word value ) { return ( value & SIGN ) ? (int64_t) value - ( (int64_t) 1 << 36 ) : (int64_t) value; }

static bool pred( int64_t value, int condition )
{
    switch ( condition )
    {
        case 0: return false;
        case 1: return value < 0;
        case 2: return value == 0;
        case 3: return value <= 0;
        case 4: return true;
        case 5: return value >= 0;
        case 6: return value != 0;
        default: return value > 0;
    }
}

static long long floordiv( long long a, long long b )
{
    long long q = a / b;
    if ( ( a % b ) != 0 && ( ( a < 0 ) != ( b < 0 ) ) )
        q--;
    return q;
}

static std::string octal( long long value, int width = 0 )
{
    char text[ 32 ];
    snprintf( text, sizeof text, "%0*llo", width, value );
    return text;
}

[[noreturn]] static void fail( const std::string & message ) { throw std::runtime_error( message ); }

static void check_memory( size_t address, size_t words )
{
    const size_t count = sizeof m / sizeof m[ 0 ];
    if ( address > count || words > count - address )
        fail( "IndexError: list index out of range" );
}

static Channel * find( int ac )
{
    auto it = channels.find( ac );
    return it == channels.end() ? nullptr : &it->second;
}

static Channel & require( int ac )
{
    Channel * c = find( ac );
    if ( !c )
        fail( "KeyError: " + std::to_string( ac ) );
    return *c;
}

static uint32_t ea( Word ir )
{
    for ( int i = 0; i < 30; i++ )
    {
        uint32_t a = (uint32_t) ( ir & RH );
        unsigned x = (unsigned) ( ir >> 18 & 15 );
        if ( x )
            a = (uint32_t) ( ( a + m[ x ] ) & RH );
        if ( !( ir & ( (Word) 1 << 22 ) ) )
            return a;
        ir = m[ a ];
    }
    fail( "PDP-10 indirection limit exceeded" );
}

static void flush_channel( int ac )
{
    Channel * c = find( ac );
    if ( !c || !c->ready )
        return;
    uint32_t base = OUT_BUFFERS + ac * 01000;
    long long n = c->written;
    std::string text;
    for ( long long i = c->flushed; i < ( n < 640 ? n : 640 ); i++ )
        text += (char) ( m[ base + i / 5 ] >> ( 29 - 7 * ( i % 5 ) ) & 127 );
    if ( c->file )
    {
        if ( fwrite( text.data(), 1, text.size(), c->file ) != text.size() || fflush( c->file ) )
            fail( "OSError: work file write failed" );
    }
    else
        output += text;
    c->flushed = n;
}

static std::string sixbit( Word w )
{
    std::string name;
    for ( int sh = 30; sh >= 0; sh -= 6 )
        name += (char) ( ( w >> sh & 63 ) + 32 );
    size_t first = name.find_first_not_of( ' ' ), last = name.find_last_not_of( ' ' );
    return first == std::string::npos ? std::string() : name.substr( first, last - first + 1 );
}

// str.splitlines() for ASCII text.
static std::vector<std::string> splitlines( const std::string & text )
{
    std::vector<std::string> lines;
    std::string line;
    for ( size_t i = 0; i < text.size(); i++ )
    {
        char ch = text[ i ];
        if ( ch == '\n' || ch == '\r' || ch == '\v' || ch == '\f' || ch == 0x1c || ch == 0x1d || ch == 0x1e )
        {
            if ( ch == '\r' && i + 1 < text.size() && text[ i + 1 ] == '\n' )
                i++;
            lines.push_back( line );
            line.clear();
        }
        else
            line += ch;
    }
    if ( !line.empty() )
        lines.push_back( line );
    return lines;
}

static int depth = 0; // XCT and local-UUO nesting, bounded like Python's recursion

static void execute( Word ir, uint32_t at );

static void nested( Word ir, uint32_t at )
{
    if ( ++depth > 900 )
        fail( "RecursionError: maximum recursion depth exceeded" );
    execute( ir, at );
    depth--;
}

static void execute( Word ir, uint32_t at )
{
    unsigned op = (unsigned) ( ir >> 27 );
    unsigned ac = (unsigned) ( ir >> 23 & 15 );
    uint32_t e;
    if ( ir & ( (Word) 1 << 22 ) )
        e = ea( ir );
    else
    {
        e = (uint32_t) ( ir & RH );
        unsigned x = (unsigned) ( ir >> 18 & 15 );
        if ( x )
            e = (uint32_t) ( ( e + m[ x ] ) & RH );
    }
    Word v = m[ e ];
    Word av = m[ ac ];
    unsigned form = op & 3;
    if ( op == 0200 )
        m[ ac ] = v;
    else if ( op == 0202 )
        m[ e ] = av;
    else if ( op == 0201 )
        m[ ac ] = e;
    else if ( op >= 0200 && op <= 0217 ) // MOVE, MOVS, MOVN, MOVM
    {
        Word val = form == 1 ? e : form == 2 ? av : v;
        unsigned grp = op >> 2 & 3;
        if ( grp == 1 )
            val = ( val << 18 | val >> 18 ) & MASK;
        else if ( grp == 2 )
            val = ( 0 - val ) & MASK;
        else if ( grp == 3 )
        {
            int64_t s = sgn( val );
            val = (Word) ( s < 0 ? -s : s );
        }
        if ( form < 2 )
            m[ ac ] = val;
        else
        {
            m[ e ] = val;
            if ( form == 3 && ac )
                m[ ac ] = val;
        }
    }
    else if ( ( op >= 0270 && op <= 0277 ) || ( op >= 0220 && op <= 0237 ) ) // integer arithmetic
    {
        Word arg = form == 1 ? e : v;
        Word val;
        if ( op >= 0270 )
            val = op < 0274 ? av + arg : av - arg;
        else if ( op < 0224 )
            val = (Word) sgn( av ) * (Word) sgn( arg );
        else if ( op >= 0230 && op < 0234 )
        {
            int64_t a = sgn( av ), d = sgn( arg );
            if ( !d )
                fail( "integer division or modulo by zero" );
            int64_t q = ( a < 0 ? -a : a ) / ( d < 0 ? -d : d ) * ( ( a < 0 ) != ( d < 0 ) ? -1 : 1 );
            m[ ( ac + 1 ) & 15 ] = (Word) ( a - q * d ) & MASK;
            val = (Word) q;
        }
        else
            fail( "('arithmetic', '0o" + octal( op ) + "')" );
        val &= MASK;
        if ( form < 2 )
            m[ ac ] = val;
        else
        {
            m[ e ] = val;
            if ( form == 3 )
                m[ ac ] = val;
        }
    }
    else if ( op >= 0300 && op <= 0377 ) // compare, skip, jump, increment/decrement
    {
        unsigned grp = op >> 3 & 7;
        int k = op & 7;
        if ( grp < 2 )
        {
            if ( pred( sgn( av ) - sgn( grp == 0 ? e : v ), k ) )
                pc = ( pc + 1 ) & RH;
        }
        else if ( grp == 2 )
        {
            if ( pred( sgn( av ), k ) )
                pc = e;
        }
        else if ( grp == 3 )
        {
            if ( ac )
                m[ ac ] = v;
            if ( pred( sgn( v ), k ) )
                pc = ( pc + 1 ) & RH;
        }
        else
        {
            bool accumulator = grp == 4 || grp == 6;
            Word val = ( ( accumulator ? av : v ) + ( grp == 4 || grp == 5 ? 1 : (Word) -1 ) ) & MASK;
            if ( accumulator )
                m[ ac ] = val;
            else
            {
                m[ e ] = val;
                if ( ac )
                    m[ ac ] = val;
            }
            if ( pred( sgn( val ), k ) )
                pc = accumulator ? e : ( pc + 1 ) & RH;
        }
    }
    else if ( op >= 0400 && op <= 0477 ) // Boolean operations
    {
        Word arg = form == 1 ? e : v;
        unsigned f = op >> 2 & 15;
        Word val = 0;
        if ( f & 1 ) val |= av & arg;
        if ( f & 2 ) val |= ~av & arg;
        if ( f & 4 ) val |= av & ~arg;
        if ( f & 8 ) val |= ~( av | arg );
        val &= MASK;
        if ( form < 2 )
            m[ ac ] = val;
        else
        {
            m[ e ] = val;
            if ( form == 3 )
                m[ ac ] = val;
        }
    }
    else if ( op >= 0500 && op <= 0577 ) // halfword transfers
    {
        Word src = form == 1 ? e : form == 2 ? av : v;
        Word dest = form < 2 ? av : v;
        bool leftdest = !( op & 32 );
        bool leftsrc = leftdest != (bool) ( op & 4 );
        Word half = ( leftsrc ? src >> 18 : src ) & RH;
        unsigned fill = op >> 3 & 3;
        Word other = fill == 0 ? ( leftdest ? dest & RH : dest >> 18 ) : fill == 1 ? 0
                   : ( fill == 2 || ( half & ( (Word) 1 << 17 ) ) ) ? RH : 0;
        Word val = leftdest ? ( half << 18 | other ) : ( other << 18 | half );
        if ( form < 2 )
            m[ ac ] = val;
        else
        {
            m[ e ] = val;
            if ( form == 3 && ac )
                m[ ac ] = val;
        }
    }
    else if ( op >= 0600 && op <= 0677 ) // bit tests and modifications
    {
        unsigned k = op >> 1 & 3;
        unsigned mod = op >> 4 & 3;
        Word arg = !( op & 8 ) ? ( op & 1 ? (Word) e << 18 : e ) : ( op & 1 ? ( v << 18 | v >> 18 ) & MASK : v );
        bool yes = ( av & arg ) != 0;
        if ( k == 2 || ( k == 1 && !yes ) || ( k == 3 && yes ) )
            pc = ( pc + 1 ) & RH;
        if ( mod == 1 )
            m[ ac ] = av & ~arg;
        else if ( mod == 2 )
            m[ ac ] = av ^ arg;
        else if ( mod == 3 )
            m[ ac ] = av | arg;
    }
    else if ( op == 0254 )
    {
        if ( ac == 4 )
            fail( "('HALT', '0o" + octal( at ) + "', '0o" + octal( e ) + "')" );
        pc = e;
    }
    else if ( op == 0255 )
        ;
    else if ( op == 0260 )
    {
        m[ ac ] = step( av, 1 );
        m[ m[ ac ] & RH ] = pc;
        pc = e;
    }
    else if ( op == 0261 )
    {
        m[ ac ] = step( av, 1 );
        m[ m[ ac ] & RH ] = v;
    }
    else if ( op == 0262 )
    {
        m[ e ] = m[ av & RH ];
        m[ ac ] = step( av, -1 );
    }
    else if ( op == 0263 )
    {
        pc = (uint32_t) ( m[ av & RH ] & RH );
        m[ ac ] = step( av, -1 );
    }
    else if ( op == 0264 )
    {
        m[ e ] = pc;
        pc = ( e + 1 ) & RH;
    }
    else if ( op == 0265 )
    {
        m[ ac ] = pc;
        pc = e;
    }
    else if ( op == 0266 )
    {
        m[ e ] = av;
        m[ ac ] = (Word) e << 18 | pc;
        pc = ( e + 1 ) & RH;
    }
    else if ( op == 0267 )
    {
        m[ ac ] = m[ av >> 18 & RH ];
        pc = e;
    }
    else if ( op == 0256 )
        nested( v, e );
    else if ( op == 0250 )
    {
        m[ ac ] = v;
        m[ e ] = av;
    }
    else if ( op == 0251 )
    {
        Word a = av >> 18;
        Word d = av & RH;
        while ( d <= e )
        {
            if ( d > RH || a > RH )
                fail( "IndexError: list index out of range" );
            m[ d ] = m[ a ];
            a = ( a + 1 ) & RH;
            d++;
        }
        m[ ac ] = ( a << 18 | d ) & MASK;
    }
    else if ( op == 0242 || op == 0240 || op == 0241 )
    {
        long long n = e < 131072 ? (long long) e : (long long) e - ( 1 << 18 );
        Word val;
        if ( op == 0242 )
            val = ( n >= 0 ? av << ( n < 36 ? n : 36 ) : av >> ( -n < 36 ? -n : 36 ) ) & MASK;
        else if ( op == 0240 )
        {
            int64_t s = sgn( av );
            val = ( n >= 0 ? (Word) s << ( n < 36 ? n : 36 ) : (Word) ( s >> ( -n < 36 ? -n : 36 ) ) ) & MASK;
        }
        else
        {
            n = ( ( n % 36 ) + 36 ) % 36;
            val = ( av << n | av >> ( 36 - n ) ) & MASK;
        }
        m[ ac ] = val;
    }
    else if ( op == 0246 )
    {
        long long n = e < 131072 ? (long long) e : (long long) e - ( 1 << 18 );
        Word low = m[ ( ac + 1 ) & 15 ];
        Word hi = av | low >> 36; // av << 36 | low
        Word lo = low & MASK;
        if ( n >= 0 )
        {
            long long k = n < 72 ? n : 72;
            if ( k >= 72 ) hi = lo = 0;
            else if ( k >= 36 ) { hi = lo << ( k - 36 ); lo = 0; }
            else if ( k ) { hi = hi << k | lo >> ( 36 - k ); lo = lo << k; }
        }
        else
        {
            long long k = -n < 72 ? -n : 72;
            if ( k >= 72 ) hi = lo = 0;
            else if ( k >= 36 ) { lo = hi >> ( k - 36 ); hi = 0; }
            else if ( k ) { lo = lo >> k | hi << ( 36 - k ); hi = hi >> k; }
        }
        m[ ac ] = hi & MASK;
        m[ ( ac + 1 ) & 15 ] = lo & MASK;
    }
    else if ( op == 0252 || op == 0253 )
    {
        m[ ac ] = step( av, 1 );
        if ( ( sgn( m[ ac ] ) < 0 ) == ( op == 0253 ) )
            pc = e;
    }
    else if ( op >= 0133 && op <= 0137 ) // IBP, ILDB, LDB, IDPB, DPB
    {
        Word bp = v;
        if ( op == 0133 || op == 0134 || op == 0136 )
        {
            long long pos = (long long) ( bp >> 30 ) - (long long) ( bp >> 24 & 63 );
            if ( pos < 0 )
            {
                pos = 36 - (long long) ( bp >> 24 & 63 );
                bp = ( bp & ~RH ) | ( ( bp + 1 ) & RH );
            }
            bp = ( bp & ( ( (Word) 1 << 30 ) - 1 ) ) | (Word) pos << 30;
            m[ e ] = bp;
        }
        if ( op != 0133 )
        {
            uint32_t a = ea( bp );
            unsigned pos = (unsigned) ( bp >> 30 );
            unsigned sz = (unsigned) ( bp >> 24 & 63 );
            Word mask = sz >= 64 ? ~(Word) 0 : ( (Word) 1 << sz ) - 1;
            if ( op == 0134 || op == 0135 )
                m[ ac ] = m[ a ] >> pos & mask;
            else
            {
                m[ a ] = ( m[ a ] & ~( mask << pos ) ) | ( av & mask ) << pos;
                if ( sz == 7 )
                    for ( int ch : channelOrder )
                    {
                        Channel & c = channels[ ch ];
                        uint32_t base = OUT_BUFFERS + ch * 01000;
                        if ( c.ready && base <= a && a < base + 128 )
                        {
                            long long count = (long long) ( a - base ) * 5 + floordiv( 29 - (long long) pos, 7 ) + 1;
                            if ( count > c.written )
                                c.written = count;
                        }
                    }
            }
        }
    }
    else if ( op < 040 ) // Fortran local UUOs, dispatched through the saved vector
    {
        m[ 040 ] = (Word) op << 27 | (Word) ac << 23 | e;
        nested( m[ 041 ], 041 );
    }
    else if ( op == 047 && ( e == 0 || e == 016 ) ) // RESET, APRENB
        ;
    else if ( op == 047 && e == 011 ) // CORE: memory is fixed, so any request succeeds
    {
        m[ ac ] = 0400;
        pc = ( pc + 1 ) & RH;
    }
    else if ( op == 047 && ( e == 023 || e == 027 || e == 030 || e == 024 ) ) // timing/job queries
        m[ ac ] = 0;
    else if ( op == 047 && e >= 0777736 && e <= 0777745 )
    {
        // Tymshare software-interrupt setup (CALLI -42 to -33) in newer
        // runtimes such as PLM81's. They halt unless these succeed; no
        // interrupt is ever raised.
        pc = ( pc + 1 ) & RH;
    }
    else if ( op == 047 && e >= 0400000 )
        ;
    else if ( op == 051 ) // TTCALL
    {
        if ( ac == 011 || ac == 012 || ac == 013 )
            ; // CLRBFI, CLRBFO; SKPINC doesn't skip: nothing has been typed
        else if ( ac == 3 )
        {
            for ( uint32_t a = e;; a++ )
            {
                if ( a > RH )
                    fail( "IndexError: list index out of range" );
                bool end = false;
                for ( int sh = 29; sh >= 1; sh -= 7 )
                {
                    char c = (char) ( m[ a ] >> sh & 127 );
                    if ( !c )
                    {
                        end = true;
                        break;
                    }
                    output += c;
                }
                if ( end )
                    break;
            }
        }
        else if ( ac == 1 )
            output += (char) ( v & 127 );
        else
            fail( "('TTCALL', " + std::to_string( ac ) + ", '0o" + octal( e ) + "')" );
    }
    else if ( op == 047 && e == 041 )
        ; // GETTAB is unavailable; use the runtime's fallback
    else if ( op == 050 ) // OPEN
    {
        check_memory( e, 3 );
        flush_channel( ac );
        Channel * old = find( ac );
        if ( old && old->file )
            fclose( old->file );
        if ( !old )
            channelOrder.push_back( ac );
        Channel fresh;
        fresh.mode = m[ e ];
        fresh.out = (uint32_t) ( m[ e + 2 ] >> 18 );
        fresh.in = (uint32_t) ( m[ e + 2 ] & RH );
        channels[ ac ] = fresh;
        pc = ( pc + 1 ) & RH;
    }
    else if ( op == 076 || op == 077 ) // LOOKUP, ENTER (extended or original block)
    {
        uint32_t f = v < 64 ? e + 2 : e;
        check_memory( f, 2 );
        std::string name = sixbit( m[ f ] ) + "." + sixbit( m[ f + 1 ] >> 18 << 18 );
        std::string path = directory + "/" + name;
        if ( op == 077 )
        {
            Channel & c = require( ac );
            FILE * file = fopen( path.c_str(), "w+b" );
            if ( !file )
                fail( "OSError: cannot create " + path );
            if ( c.file )
                fclose( c.file );
            c.file = file;
            pc = ( pc + 1 ) & RH;
        }
        else
        {
            Channel & c = require( ac );
            FILE * file = fopen( path.c_str(), "rb" );
            if ( file )
            {
                std::string text;
                char buffer[ 4096 ];
                size_t got;
                while ( ( got = fread( buffer, 1, sizeof buffer, file ) ) > 0 )
                    text.append( buffer, got );
                if ( c.file )
                    fclose( c.file );
                c.file = file;
                c.lines = splitlines( text );
                c.nextLine = 0;
                c.hasLines = true;
                pc = ( pc + 1 ) & RH;
            }
            else
                m[ f + 1 ] = m[ f + 1 ] & ~RH;
        }
    }
    else if ( op == 047 && e == 4 )
    {
        std::string name = sixbit( av );
        // DEVCHR: available, input/output capable TTY or directory DSK.
        Word flags = name == "TTY" ? 053 : 0200047;
        m[ ac ] = ( name == "TTY" || name == "DSK" ) ? ( flags << 18 | 0640003 ) : 0;
    }
    else if ( op == 047 && ( e == 053 || e == 0101 ) )
        ; // optional DEVTYP/DEVSIZ; runtime falls back to DEVCHR
    else if ( op == 057 || op == 067 ) // OUT, OUTPUT
    {
        Channel & c = require( ac );
        uint32_t h = c.out;
        check_memory( h, 3 );
        uint32_t base = OUT_BUFFERS + ac * 01000;
        flush_channel( ac );
        c.written = 0;
        c.flushed = 0;
        c.ready = true;
        // One 128-word buffer in a ring of one: the header points at its
        // link word, whose size field selects the runtime's CRLF line mode.
        m[ base - 2 ] = (Word) 0201 << 18 | ( base - 2 );
        m[ h ] = base - 2;
        m[ h + 1 ] = (Word) 7 << 24 | ( base - 1 );
        m[ h + 2 ] = 640;
    }
    else if ( op == 056 || op == 066 ) // IN, INPUT
    {
        Channel & c = require( ac );
        uint32_t h = c.in;
        check_memory( h, 3 );
        uint32_t base = IN_BUFFERS + ac * 01000;
        std::string line;
        if ( c.hasLines )
        {
            if ( c.nextLine >= c.lines.size() )
                fail( "unexpected end of compiler work file" );
            line = c.lines[ c.nextLine++ ];
        }
        else
        {
            if ( inputAt >= input.size() )
                fail( "unexpected end of compiler input" );
            size_t end = input.find( '\n', inputAt );
            end = end == std::string::npos ? input.size() : end + 1;
            line = input.substr( inputAt, end - inputAt );
            inputAt = end;
        }
        while ( !line.empty() && line.back() == '\n' )
            line.pop_back();
        std::string data = line + "\r\n";
        check_memory( base, data.size() / 5 + ( data.size() % 5 != 0 ) );
        for ( int i = 0; i < 128; i++ )
            m[ base + i ] = 0;
        for ( size_t i = 0; i < data.size(); i++ )
            m[ base + i / 5 ] |= (Word) (unsigned char) data[ i ] << ( 29 - 7 * ( i % 5 ) );
        m[ h ] = base;
        m[ h + 1 ] = (Word) 7 << 24 | ( base - 1 );
        m[ h + 2 ] = data.size();
    }
    else if ( op == 062 )
        m[ e ] = 0;
    else if ( op == 070 || op == 071 ) // CLOSE, RELEASE
    {
        flush_channel( ac );
        Channel * c = find( ac );
        if ( c && c->file )
            fflush( c->file );
    }
    else if ( op == 047 && e == 012 ) // EXIT
    {
        for ( int ch : channelOrder )
            flush_channel( ch );
        throw Finished();
    }
    else
        fail( "unsupported PDP-10 opcode " + octal( op, 3 ) + " at " + octal( at, 6 ) );
}

static bool read_file( const char * path, std::string & text )
{
    FILE * file = fopen( path, "rb" );
    if ( !file )
        return false;
    char buffer[ 65536 ];
    size_t got;
    while ( ( got = fread( buffer, 1, sizeof buffer, file ) ) > 0 )
        text.append( buffer, got );
    fclose( file );
    return true;
}

int main( int argc, char ** argv )
{
    if ( argc > 1 && !strcmp( argv[ 1 ], "--ki" ) )
    {
        ki10 = true;
        argc--;
        argv++;
    }
    if ( argc != 7 && argc != 8 )
    {
        fprintf( stderr, "usage: pdp10 [--ki] memory.bin entry input.txt directory output.txt budget [final.bin]\n" );
        return 2;
    }
    std::string image;
    if ( !read_file( argv[ 1 ], image ) || image.size() != sizeof m )
    {
        fprintf( stderr, "invalid memory image\n" );
        return 2;
    }
    for ( size_t i = 0; i < ( (size_t) 1 << 18 ); i++ )
    {
        Word w = 0;
        for ( int b = 7; b >= 0; b-- )
            w = w << 8 | (unsigned char) image[ i * 8 + b ];
        m[ i ] = w;
    }
    pc = (uint32_t) ( strtoul( argv[ 2 ], nullptr, 10 ) & RH );
    if ( !read_file( argv[ 3 ], input ) )
    {
        fprintf( stderr, "cannot read input\n" );
        return 2;
    }
    directory = argv[ 4 ];
    long long budget = strtoll( argv[ 6 ], nullptr, 10 );
    int status = 0;
    std::string error;
    try
    {
        for ( long long n = 0; n < budget; n++ )
        {
            uint32_t address = pc;
            pc = ( address + 1 ) & RH;
            execute( m[ address ], address );
        }
        fail( "compiler instruction limit exceeded" );
    }
    catch ( Finished & )
    {
    }
    catch ( std::exception & failure )
    {
        error = failure.what();
        status = 2;
    }
    for ( auto & entry : channels )
        if ( entry.second.file )
            fclose( entry.second.file );
    FILE * out = fopen( argv[ 5 ], "wb" );
    if ( !out || fwrite( output.data(), 1, output.size(), out ) != output.size() || fclose( out ) )
    {
        fprintf( stderr, "cannot write output\n" );
        return 2;
    }
    if ( argc == 8 )
    {
        std::string state;
        for ( size_t i = 0; i <= ( (size_t) 1 << 18 ); i++ )
        {
            Word w = i < ( (size_t) 1 << 18 ) ? m[ i ] : pc;
            for ( int b = 0; b < 8; b++ )
                state += (char) ( w >> ( 8 * b ) & 255 );
        }
        FILE * final = fopen( argv[ 7 ], "wb" );
        if ( !final || fwrite( state.data(), 1, state.size(), final ) != state.size() || fclose( final ) )
        {
            fprintf( stderr, "cannot write final state\n" );
            return 2;
        }
    }
    if ( status )
        fprintf( stderr, "%s\n", error.c_str() );
    return status;
}
