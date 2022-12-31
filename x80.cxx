// 8080 and Z80 emulator.
// Written by David Lee
// useful:  https://pastraiser.com/cpu/i8080/i8080_opcodes.html
//          https://altairclone.com/downloads/manuals/8080%20Programmers%20Manual.pdf
//          http://popolony2k.com.br/xtras/programming/asm/nemesis-lonestar/8080-z80-instruction-set.html
//          https://www.zilog.com/docs/z80/um0080.pdf
//          http://www.z80.info/z80time.txt
//          http://www.z80.info/zip/z80-documented.pdf
//          http://www.z80.info/z80undoc.htm
//          http://www.z80.info/z80sflag.htm
//          http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
//          https://mdfs.net/Software/Z80/Exerciser/
// symbols that start with z80_ are (unsurprisingly) Z80-specific. 80% of this code is Z80-specific.
// 8080 emulation would be >20% faster if not for the z80 checks
// Validated 100% pass for 8080 with 8080ex1.com, 8080pre.com, Microcosm v1.0, and cputest.com Diagnostics II V1.2 in 8080 mode.
// Validated 100% pass for Z80 with zexall.com, zexdoc.com, and cputest.com in Z80 mode.

#include <stdio.h>
#include <memory.h>
#include <assert.h>
#include <vector>
#include <djltrace.hxx>

using namespace std;

#include "x80.hxx"

static bool g_traceInstructions = false;
uint8_t memory[ 65536 ];
registers reg;
static const char * reg_strings[ 8 ] = { "b", "c", "d", "e", "h", "l", "m", "a" };
static const char * rp_strings[ 4 ] = { "bc", "de", "hl", "sp" };
static const char * z80_math_strings[ 8 ] = { "add", "adc", "sub", "sbb", "and", "xor", "or", "cp" };
static const char * z80_rotate_strings[ 8 ] = { "rlc", "rrc", "rl", "rr", "sla", "sra", "sll", "srl" };
enum z80_value_source { vs_register, vs_memory, vs_indexed }; // this impacts how Z80 undocumented Y and X flags are updated
void x80_trace_instructions( bool t ) { g_traceInstructions = t; }

struct Instruction
{
    uint8_t cycles;  // base cycles. 8080 conditional calls take 11 (not 17) if not taken. conditional returns take 5 (not 11) if not taken.
    const char assembly[ 15 ];
};

#define cyclesnt 6  // cycles not taken when a conditional call, jump, or return isn't taken

// instructions starting with '*' are undocumented for 8080, and not implemented here. They also signal likely Z80 instructions

static const Instruction ins_8080[ 256 ] =
{
    /*00*/  4, "nop",      10, "lxi b, d16",   7, "stax b",   5, "inx b",    5, "inr b",     5, "dcr b",     7, "mvi b, d8", 4, "rlc",
    /*08*/  4, "*nop",     10, "dad b",        7, "ldax b",   5, "dcx b",    5, "inr c",     5, "dcr c",     7, "mvi c, d8", 4, "rrc",
    /*10*/  4, "*nop",     10, "lxi d, d16",   7, "stax d",   5, "inx d",    5, "inr d",     5, "dcr d",     7, "mvi d, d8", 4, "ral",
    /*18*/  4, "*nop",     10, "dad d",        7, "ldax d",   5, "dcx d",    5, "inr e",     5, "dcr e",     7, "mvi e, d8", 4, "rar",
    /*20*/  4, "*nop",     10, "lxi h, d16",  16, "shld a16", 5, "inx h",    5, "inr h",     5, "dcr h",     7, "mvi h, d8", 4, "daa",
    /*28*/  4, "*nop",     10, "dad h",       16, "lhld a16", 5, "dcx h",    5, "inr l",     5, "dcr l",     7, "mvi l, d8", 4, "cma",
    /*30*/  4, "*nop",     10, "lxi sp, d16", 13, "sta a16",  5, "inx sp",  10, "inr m",    10, "dcr m",    10, "mvi m, d8", 4, "stc",
    /*38*/  4, "*nop",     10, "dad sp",      13, "lda a16",  5, "dcx sp",   5, "inr a",     5, "dcr a",     7, "mvi a, d8", 4, "cmc",
    /*40*/  5, "mov b, b",  5, "mov b, c",     5, "mov b, d", 5, "mov b, e", 5, "mov b, h",  5, "mov b, l",  7, "mov b, m",  5, "mov b, a",
    /*48*/  5, "mov c, b",  5, "mov c, c",     5, "mov c, d", 5, "mov c, e", 5, "mov c, h",  5, "mov c, l",  7, "mov c, m",  5, "mov c, a",
    /*50*/  5, "mov d, b",  5, "mov d, c",     5, "mov d, d", 5, "mov d, e", 5, "mov d, h",  5, "mov d, l",  7, "mov d, m",  5, "mov d, a",
    /*58*/  5, "mov e, b",  5, "mov e, c",     5, "mov e, d", 5, "mov e, e", 5, "mov e, h",  5, "mov e, l",  7, "mov e, m",  5, "mov e, a",
    /*60*/  5, "mov h, b",  5, "mov h, c",     5, "mov h, d", 5, "mov h, e", 0, "(hook)",    5, "mov h, l",  7, "mov h, m",  5, "mov h, a",
    /*68*/  5, "mov l, b",  5, "mov l, c",     5, "mov l, d", 5, "mov l, e", 5, "mov l, h",  5, "mov l, l",  7, "mov l, m",  5, "mov l, a",
    /*70*/  7, "mov m, b",  7, "mov m, c",     7, "mov m, d", 7, "mov m, e", 7, "mov m, h",  7, "mov m, l",  7, "hlt",       7, "mov m, a",
    /*78*/  5, "mov a, b",  5, "mov a, c",     5, "mov a, d", 5, "mov a, e", 5, "mov a, h",  5, "mov a, l",  7, "mov a, m",  5, "mov a, a",
    /*80*/  4, "add b",     4, "add c",        4, "add d",    4, "add e",    4, "add h",     4, "add l",     7, "add m",     4, "add a",
    /*88*/  4, "adc b",     4, "adc c",        4, "adc d",    4, "adc e",    4, "adc h",     4, "adc l",     7, "adc m",     4, "adc a",
    /*90*/  4, "sub b",     4, "sub c",        4, "sub d",    4, "sub e",    4, "sub h",     4, "sub l",     7, "sub m",     4, "sub a",
    /*98*/  4, "sbb b",     4, "sbb c",        4, "sbb d",    4, "sbb e",    4, "sbb h",     4, "sbb l",     7, "sbb m",     4, "sbb a",
    /*a0*/  4, "ana b",     4, "ana c",        4, "ana d",    4, "ana e",    4, "ana h",     4, "ana l",     7, "ana m",     4, "ana a",
    /*a8*/  4, "xra b",     4, "xra c",        4, "xra d",    4, "xra e",    4, "xra h",     4, "xra l",     7, "xra m",     4, "xra a",
    /*b0*/  4, "ora b",     4, "ora c",        4, "ora d",    4, "ora e",    4, "ora h",     4, "ora l",     7, "ora m",     4, "ora a",
    /*b8*/  4, "cmp b",     4, "cmp c",        4, "cmp d",    4, "cmp e",    4, "cmp h",     4, "cmp l",     7, "cmp m",     4, "cmp a",
    /*c0*/ 11, "rnz",      10, "pop b",       10, "jnz a16", 10, "jmp a16", 17, "cnz a16",  11, "push b",    7, "adi d8",   11, "rst 0",
    /*c8*/ 11, "rz",       10, "ret",         10, "jz a16",  10, "*jmp",    17, "cz a16",   17, "call a16",  7, "aci d8",   11, "rst 1",
    /*d0*/ 11, "rnc",      10, "pop d",       10, "jnc a16", 10, "out d8",  17, "cnc a16",  11, "push d",    7, "sui d8",   11, "rst 2",
    /*d8*/ 11, "rc",       10, "*ret",        10, "jc a16",  10, "in d8",   17, "cc a16",   17, "*call a16", 7, "sbi d8",   11, "rst 3",
    /*e0*/ 11, "rpo",      10, "pop h",       10, "jpo a16", 18, "xthl",    17, "cpo a16",  11, "push h",    7, "ani d8",   11, "rst 4",
    /*e8*/ 11, "rpe",       5, "pchl",        10, "jpe a16",  5, "xchg",    17, "cpe a16",  17, "*call a16", 7, "xri d8",   11, "rst 5",
    /*f0*/ 11, "rp",       10, "pop psw",     10, "jp a16",   4, "di",      17, "cp a16",   11, "push psw",  7, "ori d8",   11, "rst 6",
    /*f8*/ 11, "rm",        5, "sphl",        10, "jm a16",   4, "ei",      17, "cm a16",   17, "*call a16", 7, "cpi d8",   11, "rst 7",
};

// instructions starting with '*' are Z80-specific, generally multi-byte, and handled separately.
// instructions listed here are the overlap with 8080 but with the Z80 naming and cycle counts.

static const Instruction z80_ins[ 256 ] =
{
    /*00*/  4, "nop",      10, "ld bc,d16",  7, "ld (bc),a",   6, "inc bc",      4, "inc b",        4, "dec b",     7, "ld b,d8",    4, "rlca",
    /*08*/  4, "*'",       11, "add hl,bc",  7, "ld a,(bc)",   6, "dec bc",      4, "inc c",        4, "dec c",     7, "ld c,d8",    4, "rrca",
    /*10*/  0, "*",        10, "ld de,d16",  7, "ld (de),a",   6, "inc de",      4, "inc d",        4, "dec d",     7, "ld d,d8",    4, "rla",
    /*18*/  0, "*",        11, "add hl,de",  7, "ld a(de)",    6, "dec de",      4, "inc e",        4, "dec e",     7, "ld e,d8",    4, "rra",
    /*20*/  0, "*",        10, "ld hl,d16", 16, "ld (a16),hl", 6, "inc hl",      4, "inc h",        4, "dec h",     7, "ld h,d8",    4, "daa",
    /*28*/  0, "*",        11, "add hl,hl", 20, "ld hl,(a16)", 6, "dec hl",      4, "inc l",        4, "dec l",     7, "ld l,d8",    4, "cpl",
    /*30*/  0, "*",        10, "ld sp,d16", 13, "ld (a16),a",  6, "inc sp",     11, "inc (hl)",    11, "dec (hl)", 10, "ld m,d8",    4, "scf",
    /*38*/  0, "*",        11, "add hl,sp", 13, "ld a,(a16)",  6, "dec sp",      4, "inc a",        4, "dec a",     7, "ld a,d8",    4, "ccf",
    /*40*/  4, "ld b,b",    4, "ld b,c",     4, "ld b,d",      4, "ld b,e",      4, "ld b,h",       4, "ld b,l",    7, "ld b, (hl)", 4, "ld b,a",
    /*48*/  4, "ld c,b",    4, "ld c,c",     4, "ld c,d",      4, "ld c,e",      4, "ld c,h",       4, "ld c,l",    7, "ld c, (hl)", 4, "ld c,a",
    /*20*/  4, "ld d,b",    4, "ld d,c",     4, "ld d,d",      4, "ld d,e",      4, "ld d,h",       4, "ld d,l",    7, "ld d, (hl)", 4, "ld d,a",
    /*28*/  4, "ld e,b",    4, "ld e,c",     4, "ld e,d",      4, "ld e,e",      4, "ld e,h",       4, "ld e,l",    7, "ld e, (hl)", 4, "ld e,a",
    /*60*/  4, "ld h,b",    4, "ld h,c",     4, "ld h,d",      4, "ld h,e",      0, "(hook)",       4, "ld h,l",    7, "ld h, (hl)", 4, "ld h,a",
    /*68*/  4, "ld l,b",    4, "ld l,c",     4, "ld l,d",      4, "ld l,e",      4, "ld l,h",       4, "ld l,l",    7, "ld l, (hl)", 4, "ld l,a",
    /*70*/  7, "ld (hl),b", 7, "ld (hl),c",  7, "ld (hl),d",   7, "ld (hl),e",   7, "ld (hl),h",    7, "ld (hl),l", 4, "halt",       7, "ld (hl),a",
    /*78*/  4, "ld a,b",    4, "ld a,c",     4, "ld a,d",      4, "ld a,e",      4, "ld a,h",       4, "ld a,l",    7, "ld a,(hl)",  4, "ld a,a",
    /*80*/  4, "add a,b",   4, "add a,c",    4, "add a,d",     4, "add a,e",     4, "add a,h",      4, "add a,l",   7, "add a,(hl)", 4, "add a,a",
    /*88*/  4, "adc a,b",   4, "adc a,c",    4, "adc a,d",     4, "adc a,e",     4, "adc a,h",      4, "adc a,l",   7, "adc a,(hl)", 4, "adc a,a",
    /*90*/  4, "sub b",     4, "sub c",      4, "sub d",       4, "sub e",       4, "sub h",        4, "sub l",     7, "sub (hl)",   4, "sub a",
    /*98*/  4, "sbc b",     4, "sbc c",      4, "sbc d",       4, "sbc e",       4, "sbc h",        4, "sbc l",     7, "sbc (hl)",   4, "sbc a",
    /*a0*/  4, "and b",     4, "and c",      4, "and d",       4, "and e",       4, "and h",        4, "and l",     7, "and (hl)",   4, "and a",
    /*a8*/  4, "xor b",     4, "xor c",      4, "xor d",       4, "xor e",       4, "xor h",        4, "xor l",     7, "xor (hl)",   4, "xor a",
    /*b0*/  4, "or b",      4, "or c",       4, "or d",        4, "or e",        4, "or h",         4, "or l",      7, "or (hl)",    4, "or a",
    /*b8*/  4, "cp b",      4, "cp c",       4, "cp d",        4, "cp e",        4, "cp h",         4, "cp l",      7, "cp (hl)",    4, "cp a",
    /*c0*/ 11, "ret nz",   10, "pop bc",    10, "jp nz,a16",  10, "jp a16",     17, "call nz,a16", 11, "push bc",   7, "add a,d8",  11, "rst 0",
    /*c8*/ 11, "ret z",    10, "ret",       10, "jp z,a16",    0, "*",          17, "call z,a16",  17, "call a16",  7, "adc a,d8",  11, "rst 1",
    /*d0*/ 11, "ret nc",   10, "pop de",    10, "jp nc,a16",  11, "out (d8),a", 17, "call nc,a16", 11, "push de",   7, "sub d8",    11, "rst 2",
    /*d8*/ 11, "ret c",     0, "*",         10, "jp c,a16",   11, "in a,(d8)",  17, "call c,a16",   0, "*",         7, "sbc d8",    11, "rst 3",
    /*e0*/ 11, "ret po",   10, "pop hl",    10, "jp po,a16",  19, "ex (sp),hl", 17, "call po,a16", 11, "push hl",   7, "and d8",    11, "rst 4",
    /*e8*/ 11, "ret pe",    4, "jp (hl)",   10, "jp pe,a16",   4, "ex de,hl",   17, "call pe,a16",  0, "*",         7, "xor d8",    11, "rst 5",
    /*f0*/ 11, "ret p",    10, "pop af",    10, "jp p,a16",    4, "di",         17, "call p,a16",  11, "push af",   7, "or d8",     11, "rst 6",
    /*f8*/ 11, "ret m",     5, "ld sp,hl",  10, "jp m,a16",    4, "ei",         17, "call m,a16",   0, "*",         7, "cp d8",     11, "rst 7",
};

uint16_t mword( uint16_t offset ) { return * ( (uint16_t * ) & memory[ offset ] ); }
void setmword( uint16_t offset, uint16_t value ) { * ( uint16_t * ) & memory[ offset ] = value; }
uint8_t pcbyte() { return memory[ reg.pc++ ]; }
uint16_t pcword() { uint16_t r = mword( reg.pc ); reg.pc += 2; return r; }
void pushword( uint16_t val ) { reg.sp -= 2; setmword( reg.sp, val ); }
uint16_t popword() {  uint16_t val = mword( reg.sp ); reg.sp += 2; return val; }

bool is_parity_even( uint8_t x )
{
    // it seems counter-intuitive, but this is much faster than a lookup table. 10% overall emulator perf faster for my benchmark.

    size_t c = !!( x & 0x01 );
    c += !!( x & 0x02 );
    c += !!( x & 0x04 );
    c += !!( x & 0x08 );
    c += !!( x & 0x10 );
    c += !!( x & 0x20 );
    c += !!( x & 0x40 );
    c += !!( x & 0x80 );

    return ( 0 == ( c & 1 ) );
} //is_parity_even

void set_sign_zero_parity( uint8_t x )
{
    reg.fSign = ( 0 != ( 0x80 & x ) );
    reg.fZero = ( 0 == x );
    reg.fParityEven_Overflow = is_parity_even( x );
} //set_sign_zero_parity

void z80_set_sign_zero_parity_16( uint16_t x )
{
    reg.fSign = ( 0 != ( 0x8000 & x ) );
    reg.fZero = ( 0 == x );
    reg.fParityEven_Overflow = ( is_parity_even( x & 0xff ) == is_parity_even( ( x >> 8 ) & 0xff ) );
} //z80_set_sign_zero_parity_16

uint8_t op_inc( uint8_t x )
{
    uint8_t prior = x;
    x++;
    reg.fAuxCarry = ( 0 == ( x & 0xf ) );
    set_sign_zero_parity( x );

    if ( reg.fZ80Mode )
    {
        reg.fParityEven_Overflow = ( prior == 0x7f );
        reg.fWasSubtract = false;
        reg.assignYX( x );
    }
    return x;
} //op_inc

uint8_t op_dec( uint8_t x )
{
    uint8_t result = x - 1;
    set_sign_zero_parity( result ); // parity will be overwritten on Z80 below

    if ( reg.fZ80Mode )
    {
        reg.fParityEven_Overflow = ( x == 0x80 );
        reg.fWasSubtract = true;

        // Aux / Half carry returns the opposite value on Z80 as on 8080
        reg.fAuxCarry = ( 0xf == ( result & 0xf ) );
        reg.assignYX( result );
    }
    else
        reg.fAuxCarry = ( 0xf != ( result & 0xf ) );

    return result;
} //op_dec

void op_add( uint8_t x, bool carry = false )
{
    uint16_t carry_int = carry ? 1 : 0;
    uint16_t r16 = (uint16_t) reg.a + (uint16_t) x + carry_int;
    uint8_t r8 = r16 & 0xff;
    reg.fCarry = ( 0 != ( r16 & 0x0100 ) );

    // low nibble add + carry overflows to high nibble

    reg.fAuxCarry = ( 0 != ( ( ( 0xf & reg.a ) + ( 0xf & x ) + carry_int ) & 0x10 ) );
    set_sign_zero_parity( r8 );

    // if not ( ( one of lhs and rhs are negative ) and ( one of lhs and result are negative ) )

    if ( reg.fZ80Mode )
    {
        reg.fWasSubtract = false;
        reg.fParityEven_Overflow = ( ! ( ( reg.a ^ x ) & 0x80 ) ) && ( ( reg.a ^ r8 ) & 0x80 );
        reg.assignYX( r8 );
    }

    reg.a = r8;
} //op_add

void op_adc( uint8_t x )
{
    op_add( x, reg.fCarry );
} //op_adc

void op_sub( uint8_t x, bool borrow = false )
{
    // com == ones-complement

    uint8_t com_x = ~x;
    uint8_t borrow_int = borrow ? 0 : 1;
    uint16_t res16 =  (uint16_t) reg.a + (uint16_t) com_x + (uint16_t) borrow_int;
    uint8_t res8 = res16 & 0xff;

    reg.fCarry = ( 0 == ( res16 & 0x100 ) );
    set_sign_zero_parity( res8 );

    if ( reg.fZ80Mode )
    {
        // if not ( ( one of lhs and com_x are negative ) and ( one of lhs and result are negative ) )

        reg.fParityEven_Overflow = ! ( ( reg.a ^ com_x ) & 0x80 ) && ( ( reg.a ^ res8 ) & 0x80 );
        reg.fWasSubtract = true;

        uint8_t resultH = reg.a - x - ( borrow ? 1 : 0 );
        uint8_t xorResultH = reg.a ^ x ^ resultH;
        reg.fAuxCarry = ( 0 != ( 0x10 & xorResultH ) );
        reg.assignYX( res8 );
    }
    else
    {
        reg.fAuxCarry = ( 0 != ( ( ( reg.a & 0xf ) + ( com_x & 0xf ) + borrow_int ) & 0x10 ) );
    }

    reg.a = res8;
} //op_sub

void op_sbb( uint8_t x )
{
    op_sub( x, reg.fCarry );
} //op_sbb

void op_cmp( uint8_t x )
{
    // fake "a - x". restore the original reg.a

    uint8_t tmp = reg.a;
    op_sub( x, false );
    reg.a = tmp;

    if ( reg.fZ80Mode )
        reg.assignYX( x ); // done on operand, not the result or reg.a
} //op_cmp

void op_ana( uint8_t x )
{
    reg.fAuxCarry = ( 0 != ( 0x8 & ( reg.a | x ) ) ); // documented for 8080, not true for 8085
    reg.fCarry = false;
    reg.a &= x;
    set_sign_zero_parity( reg.a );

    if ( reg.fZ80Mode )
    {
        reg.fWasSubtract = false;
        reg.fAuxCarry = true;
        reg.assignYX( reg.a );
    }
} //op_ana

void op_ora( uint8_t x )
{
    reg.a |= x;
    reg.fAuxCarry = false;
    reg.fCarry = false;
    set_sign_zero_parity( reg.a );

    if ( reg.fZ80Mode )
    {
        reg.fWasSubtract = false;
        reg.assignYX( reg.a );
    }
} //op_ora

void op_xra( uint8_t x )
{
    reg.a ^= x;
    reg.fAuxCarry = false;
    reg.fCarry = false;
    set_sign_zero_parity( reg.a );

    if ( reg.fZ80Mode )
    {
        reg.fWasSubtract = false;
        reg.assignYX( reg.a );
    }
} //op_xra

void op_math( uint8_t math, uint8_t src )
{
    assert( math <= 7 );
    if ( 7 == math ) op_cmp( src );         // in order of usage for performance
    else if ( 6 == math ) op_ora( src );
    else if ( 4 == math ) op_ana( src );
    else if ( 5 == math ) op_xra( src );
    else if ( 0 == math ) op_add( src );
    else if ( 2 == math ) op_sub( src );
    else if ( 1 == math ) op_adc( src );
    else op_sbb( src ); // 3
} //op_math

void op_dad( uint16_t x )
{
    // add x to H and set Carry if warranted

    uint32_t result = (uint32_t) reg.H() + (uint32_t) x;
    reg.fCarry = ( 0 != ( 0xffff0000 & result ) );

    if ( reg.fZ80Mode )
    {
        uint32_t auxResult = ( reg.H() & 0xfff ) + ( x & 0xfff );
        reg.fAuxCarry = ( 0 != ( auxResult & 0xf000 ) );
        reg.fWasSubtract = false;
        reg.assignYX( result >> 8 );
    }

    reg.SetH( (uint16_t) ( result & 0xffff ) );
} //op_dad

uint8_t * dst_address_rm( uint8_t rm )
{
    assert( rm <= 7 );
    if ( 6 == rm )
        return & memory[ reg.H() ];
  
    return reg.regOffset( rm );
} //dst_address_rm

uint8_t * dst_address( uint8_t op )
{
    uint8_t rm = 0x7 & ( op >> 3 );
    return dst_address_rm( rm );
} //dst_address

uint8_t src_value_rm( uint8_t rm )
{
    if ( 6 == rm )
        return memory[ reg.H() ];
  
    return * ( reg.regOffset( rm ) );
} //src_value_rm

uint8_t src_value( uint8_t op )
{
    uint8_t rm = 0x7 & op;
    return src_value_rm( rm );
} //src_value

void z80_ni( uint8_t op, uint8_t op2 )
{
    tracer.Trace( "bugbug: not-implemented z80 instruction: %#x, next byte is %#x\n", op, op2 );
    printf( "bugbug not-implemented z80 instruction: %#x, next byte is %#x\n", op, op2 );
    exit( 1 );
} //z80_ni

void z80_op_bit( uint8_t val, uint8_t bit, z80_value_source vs )
{
    assert( bit <= 7 );
    reg.fAuxCarry = true; // per doc
    reg.fWasSubtract = false;
    
    reg.fSign = ( ( 7 == bit ) && ( 0 != ( 0x80 & val ) ) ); // Zilog doc says fSign is "unknown", but hardware does this
    uint8_t cmp = ( 1 << bit );
    reg.fZero = ( 0 == ( val & cmp ) );
    reg.fParityEven_Overflow = reg.fZero;  // non-documented

    // Y and X are set from the source value, not as 4.1 from "The Undocumented Z80 Documented"
    // has from the value resulting from the bit operation.

    if ( vs_register == vs )
        reg.assignYX( val );
} //z80_op_bit

void z80_op_rlc( uint8_t * pval )
{
    // rotate left carry. 7 bit to both C and 0 bit. flags: S, Z, H reset, Parity, N reset, and C

    uint8_t x = *pval;
    bool bit7 = ( 0 != ( x & 0x80 ) );
    x <<= 1;
    reg.fCarry = bit7;
    if ( bit7 )
        x |= 1;
    else
        x &= 0xfe;
    set_sign_zero_parity( x );
    reg.fAuxCarry = false;
    reg.fWasSubtract = false;
    reg.assignYX( x );
    *pval = x;
} //z80_op_rlc

void z80_op_rl( uint8_t * pval )
{
    // rotate left. 7 bit to C. Old carry bit to 0. flags: S, Z, H reset, Parity, N reset, and C

    uint8_t x = *pval;
    bool bit7 = ( 0 != ( x & 0x80 ) );
    x <<= 1;
    if ( reg.fCarry )
        x |= 1;
    else
        x &= 0xfe;
    reg.fCarry = bit7;
    set_sign_zero_parity( x );
    reg.fAuxCarry = false;
    reg.fWasSubtract = false;
    reg.assignYX( x );
    *pval = x;
} //z80_op_rl

void z80_op_rrc( uint8_t * pval )
{
    // rotate right carry. 0 bit to both C and 7 bit. flags: S, Z, H reset, Parity, N reset, and C

    uint8_t x = *pval;
    bool bit0 = ( 0 != ( x & 1 ) );
    x >>= 1;
    reg.fCarry = bit0;
    if ( bit0 )
        x |= 0x80;
    else
        x &= 0x7f;
    set_sign_zero_parity( x );
    reg.fAuxCarry = false;
    reg.fWasSubtract = false;
    reg.assignYX( x );
    *pval = x;
} //z80_op_rrc

void z80_op_rr( uint8_t * pval )
{
    // rotate right. 0 bit to C. Old C to 7 bit. flags: S, Z, H reset, Parity, N reset, and C

    uint8_t x = *pval;
    bool bit0 = ( 0 != ( x & 1 ) );
    x >>= 1;
    if ( reg.fCarry )
        x |= 0x80;
    else
        x &= 0x7f;
    reg.fCarry = bit0;
    set_sign_zero_parity( x );
    reg.fAuxCarry = false;
    reg.fWasSubtract = false;
    reg.assignYX( x );
    *pval = x;
} //z80_op_rr

uint16_t z80_op_sub_16( uint16_t lhs, uint16_t rhs, bool borrow = false )
{
    // com == ones-complement

    uint16_t com_rhs = ~rhs;
    uint16_t borrow_int = borrow ? 0 : 1;
    uint32_t res32 =  (uint32_t) lhs + (uint32_t) com_rhs + (uint32_t) borrow_int;
    uint16_t res16 = res32 & 0xffff;

    reg.fCarry = ( 0 == ( res32 & 0x10000 ) );
    z80_set_sign_zero_parity_16( res16 );

    // if not ( ( one of lhs and com_rhs are negative ) and ( one of lhs and result are negative ) )

    reg.fParityEven_Overflow = ! ( ( lhs ^ com_rhs ) & 0x8000 ) && ( ( lhs ^ res16 ) & 0x8000 );
    reg.fWasSubtract = true;
    if ( borrow )
        rhs++;
    reg.fAuxCarry = ( ( rhs & 0xfff ) > ( lhs & 0xfff ) );
    reg.assignYX( res16 >> 8 );

    return res16;
} //z80_op_sub_16

uint16_t z80_op_add_16( uint16_t a, uint16_t b )
{
    uint32_t resultAux = ( (uint32_t) ( a & 0xfff ) + (uint32_t) ( b & 0xfff ) );
    reg.fAuxCarry = ( 0 != ( resultAux & 0xfffff000 ) );
    reg.fWasSubtract = false;

    uint32_t result = ( (uint32_t) a + (uint32_t) b );
    reg.fCarry = ( 0 != ( result & 0xffff0000 ) );
    reg.assignYX( result >> 8 );

    return result;
} //z80_op_add_16

uint16_t z80_op_adc_16( uint16_t l, uint16_t r )
{
    uint16_t carryOut, result, resultAux;

    if ( reg.fCarry )
    {
        carryOut = ( l >= ( 0xffff - r ) );
        result = r + l + 1;
        resultAux = ( 0xfff & r ) + ( 0xfff & l )  + 1;
    }
    else
    {
        carryOut = ( l > ( 0xffff - r ) );
        result = r + l;
        resultAux = ( 0xfff & r ) + ( 0xfff & l );
    }

    reg.assignYX( result >> 8 );
    reg.fAuxCarry = ( 0 != ( 0xf000 & resultAux ) );
    uint16_t carryIns = result ^ l ^ r;
    z80_set_sign_zero_parity_16( result );
    reg.fParityEven_Overflow = ( carryIns >> 15 ) ^ carryOut;
    reg.fCarry = ( 0 != carryOut );
    reg.fWasSubtract = false;
    return result;
} //z80_op_adc_16

void z80_op_sla( uint8_t * pval )
{
    uint8_t val = *pval;
    reg.fCarry = ( 0 != ( val & 0x80 ) );
    val <<= 1;
    val &= 0xfe;
    set_sign_zero_parity( val );
    reg.clearHN();
    reg.assignYX( val );
    *pval = val;
} //z80_op_sla

void z80_op_sll( uint8_t * pval ) // not a documented opcode
{
    uint8_t val = *pval;
    reg.fCarry = ( 0 != ( val & 0x80 ) );
    val <<= 1;
    val |= 1;
    set_sign_zero_parity( val );
    reg.clearHN();
    reg.assignYX( val );
    *pval = val;
} //z80_op_sll

void z80_op_sra( uint8_t * pval )
{
    uint8_t val = *pval;
    reg.fCarry = ( 0 != ( val & 1 ) );
    val >>= 1;
    val &= 0x7f;
    val |= ( ( *pval ) & 0x80 ); // leave high bit unchanged
    set_sign_zero_parity( val );
    reg.clearHN();
    reg.assignYX( val );
    *pval = val;
} //z80_op_sra

void z80_op_srl( uint8_t * pval )
{
    uint8_t val = *pval;
    reg.fCarry = ( val & 1 );
    val >>= 1;
    val &= 0x7f;
    set_sign_zero_parity( val );
    reg.clearHN();
    reg.assignYX( val );
    *pval = val;
} //z80_op_srl

uint64_t z80_emulate( uint8_t op )    // this is just for instructions that aren't shared with 8080
{
    uint16_t opaddress = reg.pc - 1;
    uint8_t op2 = memory[ reg.pc ];
    uint8_t op3 = memory[ reg.pc + 1 ];
    uint8_t op4 = memory[ reg.pc + 2 ];
    uint8_t op5 = memory[ reg.pc + 3 ];
    uint16_t op34 = (uint16_t) op3 + ( (uint16_t) op4 << 8 );
    int op3int = (int) (char) op3;
    uint16_t cycles = 4; // general-purpose default

    if ( op <= 0x38 )
    {
        if ( 0x08 == op ) // ex af and af'
        {
            swap( reg.a, reg.ap );
            reg.materializeFlags();
            swap( reg.f, reg.fp );
            reg.unmaterializeFlags();
        }
        else if ( 0x10 == op ) // djnz
        {
            uint8_t offset = pcbyte();
            reg.b = reg.b - 1;
            if ( 0 != reg.b )
            {
                reg.pc = opaddress + 2 + (int16_t) (char) offset;
                cycles = 13;
            }
            else
                cycles = 8;
        }
        else if ( 0x18 == op ) // jr n
        {
            uint8_t offset = pcbyte();
            reg.pc = opaddress + 2 + (int16_t) (char) offset;
            cycles = 12;
        }
        else if ( 0x20 == op ) // jr nz, n
        {
            uint8_t offset = pcbyte();
            if ( !reg.fZero )
            {
                reg.pc = opaddress + 2 + (int16_t) (char) offset;
                cycles = 12;
            }
            else
                cycles = 7;
        }
        else if ( 0x28 == op ) // jr z, n
        {
            uint8_t offset = pcbyte();
            if ( reg.fZero )
            {
                reg.pc = opaddress + 2 + (int16_t) (char) offset;
                cycles = 12;
            }
            else
                cycles = 7;
        }
        else if ( 0x30 == op )   // jr nc, n
        {
            uint8_t offset = pcbyte();
            if ( !reg.fCarry )
            {
                reg.pc = opaddress + 2 + (int16_t) (char) offset;
                cycles = 12;
            }
            else
                cycles = 7;
        }
        else if ( 0x38 == op )    // jr c, n
        {
            uint8_t offset = pcbyte();
            if ( reg.fCarry )
            {
                reg.pc = opaddress + 2 + (int16_t) (char) offset;
                cycles = 12;
            }
            else
                cycles = 7;
        }
        else
            z80_ni( op, op2 );
    }
    else if ( 0xcb == op ) // rotate / bits
    {
        pcbyte(); // get past op2

        if ( 0x20 == ( op2 & 0xf8 ) ) // sla
        {
            cycles = 8;
            uint8_t rm = op2 & 0x7;
            if ( 6 == rm )
                cycles += 2;
            uint8_t * pdst = dst_address_rm( rm );
            z80_op_sla( pdst );
        }
        else if ( 0x28 == ( op2 & 0xf8 ) ) // sra
        {
            cycles = 8;
            uint8_t rm = op2 & 0x7;
            if ( 6 == rm )
                cycles += 2;
            uint8_t * pdst = dst_address_rm( rm );
            z80_op_sra( pdst );
        }
        else if ( op2 >= 0x30 && op2 <= 0x3f )
        {
            uint8_t rm = op2 & 0x7;
            uint8_t * pdst = dst_address_rm( rm );
            if ( op2 <= 0x37 )
                z80_op_sll( pdst );
            else
                z80_op_srl( pdst );
        }
        else if ( 0x38 == ( op2 & 0xf8 ) ) // srl r = shift right logical
        {
            cycles = 8;
            uint8_t rm = op2 >> 4;
            if ( 6 == rm )
                cycles += 2;
            uint8_t * pdst = dst_address_rm( rm );
            z80_op_srl( pdst );
        }
        else if ( op2 >= 0x40 && op2 <= 0x7f ) // bit #, rm
        {
            cycles = 8;
            uint8_t rm = op2 & 0x7;
            uint8_t bit = ( op2 >> 3 ) & 0x7;
            uint8_t val = src_value_rm( rm );
            z80_op_bit( val, bit, ( 6 == rm ) ? vs_memory : vs_register );
        }
        else if ( op2 >= 0x80 && op2 <= 0xbf ) // res bit #, rm  AKA reset
        {
            cycles = 8;
            uint8_t rm = op2 & 0x7;
            uint8_t bit = ( op2 >> 3 ) & 0x7;
            uint8_t val = src_value_rm( rm );
            uint8_t mask = ~ ( 1 << bit );
            val &= mask;
            * dst_address_rm( rm ) = val;
        }
        else if ( op2 >= 0xc0 && op2 <= 0xff ) // set bit #, rm
        {
            cycles = 8;
            uint8_t rm = op2 & 0x7;
            uint8_t bit = ( op2 >> 3 ) & 0x7;
            uint8_t val = src_value_rm( rm );
            uint8_t mask = 1 << bit;
            val |= mask;
            * dst_address_rm( rm ) = val;
        }
        else if ( op2 <= 0x1f ) // rlc, rrc, rl, rr on rm
        {
            cycles = 8;
            uint8_t mod = op2;
            uint8_t rot = ( mod >> 3 ) & 0x3;
            uint8_t rm = mod & 0x7;
            if ( 6 == rm )
                cycles += 2;
            uint8_t * pval = dst_address_rm( rm );
            if ( 0 == rot )
                z80_op_rlc( pval );
            else if ( 1 == rot )
                z80_op_rrc( pval );
            else if ( 2 == rot )
                z80_op_rl( pval );
            else // if ( 3 == rot )
                z80_op_rr( pval );
        }
        else
            z80_ni( op, op2 );
    }
    else if ( 0xd9 == op )   // exx   B, D, H with B', D', H'
    {
        swap( reg.b, reg.bp );
        swap( reg.c, reg.cp );
        swap( reg.d, reg.dp );
        swap( reg.e, reg.ep );
        swap( reg.h, reg.hp );
        swap( reg.l, reg.lp );
    }
    else if ( 0xdd == op || 0xfd == op ) // ix & iy operations
    {
        reg.incR();
        pcbyte(); // consume op2: the dd or fd

        if ( 0x21 == op2 )  // ld ix/iy word
            * reg.indexAddress( op ) = pcword();
        else if ( 0x22 == op2 ) // ld (address), ix/iy
        {
            cycles = 20;
            uint16_t address = pcword();
            setmword( address, reg.getIndex( op ) );
        }
        else if ( 0x23 == op2 ) // inc ix/iy
        {
            cycles = 10;
            * reg.indexAddress( op ) = reg.getIndex( op ) + 1; // no flags are affected
        }
        else if ( 0x26 == op2 ) // ld ix/iy h. not documented
            * reg.getIndexByteAddress( op, 0 ) = pcbyte();
        else if ( 0x2a == op2 )  // ld ix, (address)
        {
            cycles = 20;
            uint16_t address = pcword();
            reg.setIndex( op, mword( address ) );
        }
        else if ( 0x2b == op2 ) // dec ix/iy
        {
            cycles = 10;
            * reg.indexAddress( op ) = reg.getIndex( op ) - 1; // no flags are affected
        }
        else if ( 0x2e == op2 ) // ld ix/iy l. not documented
        {
            cycles = 20; // guess
            * reg.getIndexByteAddress( op, 1 ) = pcbyte();
        }
        else if ( 0x34 == op2 ) // inc (i + index)
        {
            cycles = 23;
            uint16_t i = reg.getIndex( op ) + (int16_t) (char) pcbyte();
            uint8_t x = memory[ i ];
            memory[i] = op_inc( x );
        }
        else if ( 0x35 == op2 ) // dec (i + index)
        {
            cycles = 23;
            uint16_t i = reg.getIndex( op ) + (int16_t) (char) pcbyte();
            uint8_t x = memory[ i ];
            memory[ i ] = op_dec( x );
        }
        else if ( 0x36 == op2 )  // ld (ix/iy + index), immediate byte
        {
            cycles = 19;
            uint16_t i = reg.getIndex( op ) + (int16_t) (char) pcbyte();
            uint8_t val = pcbyte();
            memory[ i ] = val;
        }
        else if ( ( ( op2 >= 0x40 && op2 <= 0x6f ) || ( op2 >= 0x78 && op2 <= 0x7f ) ) && // ld [bcdeIhIla][bcdeIhIla]
                  ( ( ( op2 & 0xf ) != 6 ) && ( ( op2 & 0xf ) != 0xe ) ) )
        {
            uint8_t fromval = op2 & 0xf;
            if ( fromval >= 8 )
                fromval -= 8;

            uint8_t tmp = ( 0 == fromval ) ? reg.b :
                          ( 1 == fromval ) ? reg.c :
                          ( 2 == fromval ) ? reg.d :
                          ( 3 == fromval ) ? reg.e :
                          ( 4 == fromval ) ? reg.getIndexByte( op, 0 ) :
                          ( 5 == fromval ) ? reg.getIndexByte( op, 1 ) :
                          reg.a;

            if ( op2 <= 0x47 )
                reg.b = tmp;
            else if ( op2 <= 0x4f )
                reg.c = tmp;
            else if ( op2 <= 0x57 )
                reg.d = tmp;
            else if ( op2 <= 0x5f )
                reg.e = tmp;
            else if ( op2 <= 0x67 )
                *reg.getIndexByteAddress( op, 0 ) = tmp;
            else if ( op2 <= 0x6f )
                *reg.getIndexByteAddress( op, 1 ) = tmp;
            else
                reg.a = tmp;
        }
        else if ( 0x46 == ( op2 & 0x47 ) ) // ld r, (i + #)
        {
            cycles = 19;
            uint8_t index = pcbyte();
            uint16_t address = reg.getIndex( op ) + op3int;
            uint8_t src = ( ( op2 >> 3 ) & 0x7 );
            * dst_address_rm( src ) = memory[ address ];
        }
        else if ( 0x70 == ( op2 & 0xf8 ) )  // ld (i+#), r/#
        {
            cycles = 19;
            pcbyte(); // consume op3

            // if 6, there is an op4 for the index (not hl-indexed memory); otherwise use a register value

            uint8_t src = op2 & 0x7;
            uint8_t val = ( 6 == src ) ? pcbyte() : src_value_rm( src );
            uint16_t i = reg.getIndex( op );
            i += op3int;
            memory[ i ] = val;
        }
        else if ( 0x80 == ( op2 & 0xc2 ) ) // math on il and ih with a. 84/85/8c/8d/94/95/a4/a5/b4/b5/bc/bd
        {
            uint8_t math = ( ( op2 >> 3 ) & 0x7 );
            uint8_t value = reg.getIndexByte( op, op2 & 1 );
            op_math( math, value );
        }
        else if ( 0x86 == ( op2 & 0xc7 ) ) // math on [ ix/iy + index ]
        {
            cycles = 19;
            uint8_t math = ( ( op2 >> 3 ) & 0x7 );
            uint16_t x = reg.getIndex( op );
            x += (int16_t) (char) pcbyte();
            op_math( math, memory[ x ] );
        }
        else if ( 0x24 == ( op2 & 0xf6 ) ) // inc/dec ixh, ixl, iyh, iyl
        {
            uint8_t *pval = reg.getIndexByteAddress( op, ( op2 >> 3 ) & 1 );
            if ( op2 & 1 )
                *pval = op_dec( *pval );
            else
                *pval = op_inc( *pval );
        }
        else if ( 0x09 == ( op2 & 0xcf ) ) // add ix/iy, rp
        {
            // only sets H (carry from bit 11) and C (carry from bit 15) flags. ignores C flag on input. N is reset
            // for add ix, rp is 0..3 bc, de, ix, sp.
            // for add iy, rp is 0..3 bc, de, iy, sp.

            uint16_t rpval = * reg.rpAddressFromOp( op2 );
            uint8_t rp = ( 0x3 & ( op2 >> 4 ) );
            if ( 2 == rp )
                rpval = ( 0xdd == op ) ? reg.ix : reg.iy;

            uint16_t oldval = reg.getIndex( op );
            uint16_t newval = z80_op_add_16( oldval, rpval );
            reg.setIndex( op, newval );
        }
        else if ( 0xcb == op2 ) // bit operations
        {
            reg.incR();
            if ( 0x26 == op4 || 0x2e == op4 || 0x3e == op4 ) // sla, sra, srl [ix/iy + offset]
            {
                uint8_t offset = pcbyte(); // the op3
                pcbyte(); // the op4
                uint16_t index = reg.getIndex( op );
                index += (int16_t) (char) offset;
                if ( 0x26 == op4 ) // sla
                    z80_op_sla( & memory[ index ] );
                else if ( 0x2e == op4 ) // sra
                    z80_op_sra( & memory[ index ] );
                else // ( 0x3e == op4 ) srl
                    z80_op_srl( & memory[ index ] );
            }
            else if ( op4 <= 0x3f ) // bit shift on memory
            {
                cycles = 23; // this is a guess -- it's undocumented
                uint8_t offset = pcbyte();
                pcbyte();
                uint16_t index = reg.getIndex( op );
                index += (int16_t) (char) offset;
    
                if ( op4 <= 0x07 )
                    z80_op_rlc( & memory[ index ] );
                else if ( op4 <= 0x0f )
                    z80_op_rrc( & memory[ index ] );
                else if ( op4 <= 0x17 )
                    z80_op_rl( & memory[ index ] );
                else if ( op4 <= 0x1f )
                    z80_op_rr( & memory[ index ] );
                else if ( op4 <= 0x27 )
                    z80_op_sla( & memory[ index ] );
                else if ( op4 <= 0x2f )
                    z80_op_sra( & memory[ index ] );
                else if ( op4 <= 0x37 )
                    z80_op_sll( & memory[ index ] );
                else if ( op4 <= 0x3f )
                    z80_op_srl( & memory[ index ] );
        
                uint8_t rm = op4 & 0x7;
                if ( 6 != rm )          // no write to memory variant
                    * dst_address_rm( rm ) = memory[ index ];
            }
            else if ( ( ( op4 & 0xf ) == 0xe ) || ( ( op4 & 0xf ) == 0x6 ) ) // bit/res/set b, (ix/iy + d)
            {
                cycles = 23;
                uint8_t index = pcbyte();
                uint8_t mod = pcbyte();
                uint8_t bit = ( mod >> 3 ) & 0x7;
                uint8_t mask = 1 << bit;
                uint8_t top2bits = mod & 0xc0;
                uint16_t offset = reg.getIndex( op ) + (int16_t) (char) index;
                uint8_t val = memory[ offset ];
    
                if ( 0x40 == top2bits ) // bit
                {
                    cycles++;
                    z80_op_bit( val, bit, vs_indexed );
                }
                else if ( 0x80 == top2bits ) // reset
                {
                    mask = ~mask;
                    val &= mask;
                    memory[ offset ] = val;
                }
                else if ( 0xc0 == top2bits ) // set
                {
                    val |= mask;
                    memory[ offset ] = val;
                }
                else if ( 0x00 == top2bits ) // rlc/rl/rrc/rr rotate of (i + index)
                {
                    if ( 0x06 == mod )
                        z80_op_rlc( & memory[ offset ] );
                    else if ( 0x16 == mod )
                        z80_op_rl( & memory[ offset ] );
                    else if ( 0x0e == mod )
                        z80_op_rrc( & memory[ offset ] );
                    else if ( 0x1e == mod )
                        z80_op_rr( & memory[ offset ] );
                    else
                        z80_ni( op, op2 );
                }
                else
                    z80_ni( op, op2 );
            }
        }
        else if ( 0xe1 == op2 ) // pop ix/iy
        {
            cycles = 14;
            uint16_t val = popword();
            reg.setIndex( op, val );
        }
        else if ( 0xe5 == op2 )  // push ix/iy
        {
            cycles = 15;
            uint16_t val = reg.getIndex( op );
            pushword( val );
        }
        else if ( 0xe9 == op2 ) // jp (ix/iy) // the Z80 name makes it look indirect. It's not.
        {
            cycles = 8;
            reg.pc = reg.getIndex( op );
        }
        else if ( 0xf9 == op2 ) // ld sp, ix/iy
        {
            cycles = 10;
            reg.sp = reg.getIndex( op );
        }
        else
            z80_ni( op, op2 );
    }
    else if ( 0xed == op ) // 16-bit load/store and i/o operations
    {
        reg.incR();
        pcbyte();  // consume op2

        if ( 0xb == ( op2 & 0xf ) )      // ld rp, (nn) AKA ld dd, (nn)
        {
            cycles = 20;
            uint8_t rp = op2 & 0x30;
            uint16_t * prp = reg.rpAddressFromOp( rp );
            *prp = mword( pcword() );
        }
        else if ( 0x3 == ( op2 & 0xf ) ) // ld (mw), rp AKA ld (nn), dd
        {
            cycles = 20;
            uint8_t rp = op2 & 0x30;
            uint16_t * prp = reg.rpAddressFromOp( rp );
            setmword( pcword(), *prp );
        }
        else if ( 0x44 == op2 ) // neg
        {
            uint8_t prior = reg.a;
            reg.a = 0 - reg.a;
            set_sign_zero_parity( reg.a );
            reg.fParityEven_Overflow = ( 0x80 == prior );
            reg.fWasSubtract = true;
            reg.fCarry = ( 0 != prior );
            reg.fAuxCarry = ( 0 != ( prior & 0xf ) );
            reg.assignYX( reg.a );
        }
        else if ( 0x47 == op2 ) // ld i,a
            reg.i = reg.a;
        else if ( 0x4f == op2 ) // ld r,a
        {
            reg.r = reg.a;
            reg.incR();
        }
        else if ( 0x57 == op2 ) // ld a,i
        {
            reg.a = reg.i;
            set_sign_zero_parity( reg.a );
            reg.fParityEven_Overflow = false; // no iff2
            reg.fWasSubtract = false;
            reg.fAuxCarry = false;
            reg.assignYX( reg.a );
        }
        else if ( 0x5f == op2 ) // ld a,r
        {
            reg.a = reg.r - 1;
            set_sign_zero_parity( reg.a );
            reg.fParityEven_Overflow = false; // no iff2
            reg.fWasSubtract = false;
            reg.fAuxCarry = false;
            reg.assignYX( reg.a );
        }
        else if ( 0x67 == op2 ) // rrd
        {
            uint8_t mem = memory[ reg.H() ];
            uint8_t a = reg.a;

            reg.a = ( a & 0xf0 ) | ( mem & 0x0f );
            uint8_t newmem = ( ( a << 4 ) & 0xf0 ) | ( ( mem >> 4 ) & 0x0f );
            memory[ reg.H() ] = newmem;

            set_sign_zero_parity( reg.a );
            reg.clearHN();
            reg.assignYX( reg.a );
        }
        else if ( 0x6f == op2 ) // rld
        {
            uint8_t mem = memory[ reg.H() ];
            uint8_t a = reg.a;

            uint8_t newmem = ( ( mem << 4 ) & 0xf0 ) | ( a & 0x0f );
            reg.a = ( ( mem >> 4 ) & 0xf ) | ( a & 0xf0 );
            memory[ reg.H() ] = newmem;

            set_sign_zero_parity( reg.a );
            reg.clearHN();
            reg.assignYX( reg.a );
        }
        else if ( 0x4a == ( op2 & 0xcf ) ) // adc hl, rp
        {
            uint8_t rp = ( ( op2 >> 4 ) & 3 );
            uint16_t result = z80_op_adc_16( reg.H(), * reg.rpAddress( rp ) );
            reg.SetH( result );
        }
        else if ( 0xa0 == op2 ) // ldi
        {
            cycles = 16;
            memory[ reg.D() ] = memory[ reg.H() ];
            reg.fY = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x02 ) );
            reg.fX = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x08 ) );
            reg.SetH( reg.H() + 1 );
            reg.SetD( reg.D() + 1 );
            reg.SetB( reg.B() - 1 );
            reg.fParityEven_Overflow = ( 0 != reg.B() );
            reg.fAuxCarry = 0;
            reg.fWasSubtract = 0;
        }
        else if ( 0xa8 == op2 ) // ldd
        {
            cycles = 16;
            memory[ reg.D() ] = memory[ reg.H() ];
            reg.fY = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x02 ) );
            reg.fX = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x08 ) );
            reg.SetH( reg.H() - 1 );
            reg.SetD( reg.D() - 1 );
            reg.SetB( reg.B() - 1 );
            reg.fParityEven_Overflow = ( 0 != reg.B() );
            reg.clearHN();
        }
        else if ( 0xb0 == op2 ) // ldir
        {
            uint8_t special;
            cycles = 0;
            do
            {
                cycles += 21;
                memory[ reg.D() ] = memory[ reg.H() ];
                reg.fY = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x02 ) );
                reg.fX = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x08 ) );
                reg.SetH( reg.H() + 1 );
                reg.SetD( reg.D() + 1 );
                reg.SetB( reg.B() - 1 );
            } while ( 0 != reg.B() );

            cycles -= 5; // the last iteration is cheaper
            reg.fParityEven_Overflow = false;
            reg.fAuxCarry = 0;
            reg.fWasSubtract = 0;
        }
        else if ( 0xa1 == op2 ) // cpi
        {
            bool oldCarry = reg.fCarry;
            cycles = 16;
            uint8_t memval = memory[ reg.H() ];
            op_cmp( memval );
            reg.SetH( reg.H() + 1 );
            reg.SetB( reg.B() - 1 );
            reg.fParityEven_Overflow = ( 0 != reg.B() );
            uint8_t n = reg.a - memval - ( reg.fAuxCarry ? 1 : 0 ); // n = A - (HL) - HF
            reg.fY = ( 0 != ( n & 0x02 ) );
            reg.fX = ( 0 != ( n & 0x08 ) );
            reg.fCarry = oldCarry; // carry is not affected
        }
        else if ( 0xa9 == op2 ) // cpd
        {
            bool oldCarry = reg.fCarry;
            cycles = 16;
            uint8_t memval = memory[ reg.H() ];
            op_cmp( memval );
            reg.SetH( reg.H() - 1 );
            reg.SetB( reg.B() - 1 );
            reg.fParityEven_Overflow = ( 0 != reg.B() );
            uint8_t n = reg.a - memval - ( reg.fAuxCarry ? 1 : 0 ); // n = A - (HL) - HF
            reg.fY = ( 0 != ( n & 0x02 ) );
            reg.fX = ( 0 != ( n & 0x08 ) );
            reg.fCarry = oldCarry; // carry is not affected
        }
        else if ( 0xb1 == op2 ) // cpir
        {
            bool oldCarry = reg.fCarry;
            cycles = 0;
            do
            {
                cycles += 21;
                uint8_t memval = memory[ reg.H() ];
                op_cmp( memval );
                uint8_t n = reg.a - memval - ( reg.fAuxCarry ? 1 : 0 ); // n = A - (HL) - HF
                reg.fY = ( 0 != ( n & 0x02 ) );
                reg.fX = ( 0 != ( n & 0x08 ) );
                reg.SetH( reg.H() + 1 );
                reg.SetB( reg.B() - 1 );
            } while ( !reg.fZero && ( 0 != reg.B() ) );

            cycles -= 5; // the last iteration is cheaper
            reg.fParityEven_Overflow = ( 0 != reg.B() ); // not what the Zilog doc says, but it's what works
            reg.fCarry = oldCarry; // carry is not affected
        }
        else if ( 0xb9 == op2 ) // cpdr
        {
            bool oldCarry = reg.fCarry;
            cycles = 0;
            do
            {
                cycles += 21;
                uint8_t memval = memory[ reg.H() ];
                op_cmp( memval );
                uint8_t n = reg.a - memval - ( reg.fAuxCarry ? 1 : 0 ); // n = A - (HL) - HF
                reg.fY = ( 0 != ( n & 0x02 ) );
                reg.fX = ( 0 != ( n & 0x08 ) );
                reg.SetH( reg.H() - 1 );
                reg.SetB( reg.B() - 1 );
            } while ( !reg.fZero && ( 0 != reg.B() ) );

            cycles -= 5; // the last iteration is cheaper
            reg.fParityEven_Overflow = ( 0 != reg.B() );
            reg.fCarry = oldCarry; // carry is not affected
        }
        else if ( 0xb8 == op2 ) // lddr
        {
            cycles = 0;
            do
            {
                cycles += 21;
                memory[ reg.D() ] = memory[ reg.H() ];
                reg.fY = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x02 ) );
                reg.fX = ( 0 != ( ( memory[ reg.H() ] + reg.a ) & 0x08 ) );
                reg.SetH( reg.H() - 1 );
                reg.SetD( reg.D() - 1 );
                reg.SetB( reg.B() - 1 );
            } while ( 0 != reg.B() );

            cycles -= 5; // the last iteration is cheaper
            reg.fParityEven_Overflow = false; // unlike similar functions
            reg.clearHN();
        }
        else if ( 0x02 == ( op2 & 0x8f ) ) // sbc hl, rp AKA sbc hl, ss
        {
            cycles = 15;
            uint8_t rp = op2 & 0x30;
            uint16_t val = * reg.rpAddressFromOp( rp );
            uint16_t result = z80_op_sub_16( reg.H(), val, reg.fCarry );
            reg.SetH( result );
        }
        else if ( 0x04 == ( op2 & 0x8f ) ) // adc hl, rp AKA sbc hl, ss
        {
            cycles = 15;
            uint16_t val = * reg.rpAddressFromOp( op );
            uint16_t result = z80_op_adc_16( reg.H(), val );
            reg.SetH( result );
        }
        else if ( 0x44 == op2 ) // neg
        {
            cycles = 8;
            uint8_t orig = reg.a;
            reg.a = 0 - reg.a;
            z80_set_sign_zero_parity_16( reg.a );
            reg.fParityEven_Overflow = ( 0x80 == orig );
            reg.assignYX( reg.a );
        }
        else
            z80_ni( op, op2 );
    }
    else
        z80_ni( op, op2 );

    return cycles;
} //z80_emulate

void z80_renderByteReg( char * acfrom, uint8_t op, uint8_t fromval )
{
    memset( acfrom, 0, 4 );
    assert( 0xdd == op || 0xfd == op );
    assert( fromval <= 7 );
    if ( fromval < 4 )
        acfrom[ 0 ] = 'b' + fromval;
    else if ( 7 == fromval )
        acfrom[0] = 'a';
    else
        sprintf( acfrom, "%s%c", 0xdd == op ? "ix" : "iy", 4 == fromval ? 'h' : 'l' );
} //z80_renderByteRef

void z80_render( char * ac, uint8_t op, uint16_t address )
{
    uint8_t op2 = memory[ address + 1 ];
    uint8_t op3 = memory[ address + 2 ];
    uint8_t op4 = memory[ address + 3 ];
    uint16_t op34 = (uint16_t) op3 + ( (uint16_t) op4 << 8 );
    int16_t op2int = (int16_t) (char) op2;
    int16_t op3int = (int16_t) (char) op3;
    sprintf( ac, "z80 %02x %02x %02x NYI", op, op2, op3 );

    if ( 0xdd == op || 0xfd == op ) // ix & iy operations
    {
        const char * i = ( 0xdd == op ) ? "ix" : "iy";

        if ( 0x46 == ( op2 & 0x47 ) )
        {
            uint8_t src = ( ( op2 >> 3 ) & 0x7 );
            sprintf( ac, "ld %s, (%s%s%d)", reg_strings[ src ], i, op3int >= 0 ? "+" : "", op3int );
        }                             
        else if ( 0x70 == ( op2 & 0xf8 ) )  
        {
            uint8_t src = op2 & 0x7;
            if ( 6 == src )
                sprintf( ac, "ld (%s%s%d), %02x", i, op3int >= 0 ? "+" : "", op3int, op4 );
            else
                sprintf( ac, "ld (%s%s%d), %s", i, op3int >= 0 ? "+" : "", op3int, reg_strings[ src ] );
        }
        else if ( 0x09 == ( op2 & 0xcf ) )
            sprintf( ac, "add %s, %s\n", i, rp_strings[ ( op2 >> 4 ) & 0x3 ] );
        else if ( 0x21 == op2 )
            sprintf( ac, "ld %s, %04xh", i, op34 );
        else if ( 0x22 == op2 )
            sprintf( ac, "ld (%04xh), %s", op34, i );
        else if ( 0x23 == op2 )
            sprintf( ac, "inc %s", i );
        else if ( 0x26 == op2 )
            sprintf( ac, "ld %sh, %02x", i, op3 );
        else if ( 0x2a == op2 )
            sprintf( ac, "ld %s, %04xh", i, op34 );
        else if ( 0x2b == op2 )
            sprintf( ac, "dec %s", i );
        else if ( 0x2e == op2 )
            sprintf( ac, "ld %sl, %02x", i, op3 );
        else if ( 0x34 == op2 )
            sprintf( ac, "inc (%s%s%d)\n", i, op3int >= 0 ? "+" : "", op3int );
        else if ( 0x35 == op2 )
            sprintf( ac, "dec (%s%s%d)\n", i, op3int >= 0 ? "+" : "", op3int );
        else if ( 0x36 == op2 )
            sprintf( ac, "ld (%s+%d), %#xh", i, op3, op4 );
        else if ( ( op2 >= 0x40 && op2 <= 0x6f ) || ( op2 >= 0x78 && op2 <= 0x7f ) )
        {
            char acto[ 4 ] = {0};
            char acfrom[ 4 ] = {0};
            uint8_t fromval = op2 & 0xf;
            if ( fromval >= 8 )
                fromval -= 8;
            z80_renderByteReg( acfrom, op, fromval );

            if ( op2 <= 0x47 )
                acto[0] = 'b';
            else if ( op2 <= 0x4f )
                acto[0] = 'c';
            else if ( op2 <= 0x57 )
                acto[0] = 'd';
            else if ( op2 <= 0x5f )
                acto[0] = 'e';
            else if ( op2 <= 0x67 )
                sprintf( acto, "%sh", i );
            else if ( op2 <= 0x6f )
                sprintf( acto, "%sl", i );
            else
                acto[0] = 'a';

            sprintf( ac, "ld %s, %s", acto, acfrom );
        }
        else if ( 0x2a == op2 )
            sprintf( ac, "ld %s, (%04x)", i, op34 );
        else if ( 0x22 == op2 )
            sprintf( ac, "ld (%04x), %s", op34, i );
        else if ( 0x80 == ( op2 & 0xc2 ) ) // math on il and ih with a. 84/85/8c/8d/94/95/a4/a5/b4/b5/bc/bd
        {
            uint8_t math = ( ( op2 >> 3 ) & 0x7 );
            sprintf( ac, "%s a,%s%c", z80_math_strings[ math ], i, ( op2 & 1 ) ? 'l' : 'h' );
        }
        else if ( 0x86 == ( op2 & 0xc7 ) ) // math on [ ix/iy + index ]. 86, 8e, 96, 9e, a6, ae, b6, be
        {
            uint8_t math = ( ( op2 >> 3 ) & 0x7 );
            sprintf( ac, "%s a, (%s%s%d)", z80_math_strings[ math ], i, op3int >=0 ? "+" : "", op3int );
        }
        else if ( 0xcb == op2 && ( op4 <= 0x3f ) )
        {
            uint8_t rot = ( ( op4 >> 3 ) & 0x7 );
            int offset = (int) (char) op3;
            sprintf( ac, "%s %s, (%s%s%d)", z80_rotate_strings[ rot ], reg_strings[ op4 & 0x7 ], i, offset >= 0 ? "+" : "", offset );
        }
        else if ( 0xcb == op2 && ( ( ( op4 & 0xf ) == 0xe ) || ( ( op4 & 0xf ) == 0x6 ) ) ) // bit operations on indexed memory
        {
            uint8_t index = op3;
            int32_t index32 = (int32_t) (char) index;
            uint8_t mod = op4;
            uint8_t bit = ( mod >> 3 ) & 0x7;
            uint8_t top2bits = mod & 0xc0;

            if ( 0x40 == top2bits ) // bit
                sprintf( ac, "bit %d, ( %s%s%d )", bit, i, index32 >= 0 ? "+" : "", index32 ); 
            else if ( 0x80 == top2bits ) // reset
                sprintf( ac, "res %d, ( %s%s%d )", bit, i, index32 >= 0 ? "+" : "", index32 ); 
            else if ( 0xc0 == top2bits ) // set
                sprintf( ac, "set %d, ( %s%s%d )", bit, i, index32 >= 0 ? "+" : "", index32 );
            else if ( 0x26 == op4 ) // sla
                sprintf( ac, "sla (%s%s%d)", i, index32 >= 0 ? "+" : "", index32 );
            else if ( 0x2e == op4 ) // sra
                sprintf( ac, "sra (%s%s%d)", i, index32 >= 0 ? "+" : "", index32 );
            else if ( 0x3e == op4 ) // srl
                sprintf( ac, "srl (%s%s%d)", i, index32 >= 0 ? "+" : "", index32 );
            else if ( 0x00 == top2bits ) // rlc/rl/rrc/rl rotate of (i + index)
            {
                const char * pc = "n/a";
                if ( 0x06 == op4 )
                    pc = "rlc";
                else if ( 0x0e == op4 )
                    pc = "rrc";
                else if ( 0x16 == op4 )
                    pc = "rl";
                else if ( 0x1e == op4 )
                    pc = "rr";
                sprintf( ac, "%s (%s%s%d)", pc, i, index32 >= 0 ? "+" : "", index32 );
            }
        }
        else if ( 0xe1 == op2 )
            sprintf( ac, "pop %s", i );
        else if ( 0xe5 == op2 )
            sprintf( ac, "push %s", i );
        else if ( 0xe9 == op2 )
            sprintf( ac, "jp (%s)", i );    // this official syntax is bad; it's not indirect
        else if ( 0xf9 == op2 )
            sprintf( ac, "ld sp, %s", i );
        else
            sprintf( ac, "unknown 0xdd / 0xfd instruction, op2 %2x", op2 );
    }
    else if ( 0xed == op ) // load and compare operations
    {
        if ( 0xb == ( op2 & 0xf ) )
            sprintf( ac, "ld %s, (%04xh)", rp_strings[ ( ( op2 & 0xf0 ) >> 4 ) - 4 ], op34 );
        else if ( 0x3 == ( op2 & 0xf ) )
            sprintf( ac, "ld (%04xh), %s", op34, rp_strings[ ( ( op2 & 0xf0 ) >> 4 ) - 4 ] );
        else if ( 0x44 == op2 )
            strcpy( ac, "neg" );
        else if ( 0x47 == op2 )
            sprintf( ac, "ld i,a" );
        else if ( 0x4f == op2 )
            sprintf( ac, "ld r,a" );
        else if ( 0x57 == op2 )
            sprintf( ac, "ld a,i" );
        else if ( 0x5f == op2 )
            sprintf( ac, "ld a,r" );
        else if ( 0x67 == op2 )
            strcpy( ac, "rrd" );
        else if ( 0x6f == op2 )
            strcpy( ac, "rld" );
        else if ( 0xa0 == op2 )
            strcpy( ac, "ldi" );
        else if ( 0xa8 == op2 )
            strcpy( ac, "ldd" );
        else if ( 0xb0 == op2 )
            strcpy( ac, "ldir" );
        else if ( 0xb8 == op2 )
            strcpy( ac, "lddr" );
        else if ( 0x02 == ( op2 & 0x8f ) )
        {
            uint8_t rp = ( ( ( op2 & 0x30 ) >> 4 ) & 3 );
            sprintf( ac, "sbc hl, %s", rp_strings[ rp ] );
        }
        else if ( 0x4a == ( op2 & 0xcf ) ) // adc hl, rp
        {
            uint8_t rp = ( ( op2 >> 4 ) & 3 );
            sprintf( ac, "adc hl, %s", rp_strings[ rp ] );
        }
        else if ( 0xa1 == op2 )
            strcpy( ac, "cpi" );
        else if ( 0xa9 == op2 )
            strcpy( ac, "cpd" );
        else if ( 0xb1 == op2 )
            strcpy( ac, "cpir" );
        else if ( 0xb9 == op2 )
            strcpy( ac, "cpdr" );
    }
    else if ( 0xcb == op ) // rotate / bits. There is no sll in documented Z80
    {
        if ( 0x20 == ( op2 & 0xf8 ) ) // sla = shift left arithmetic
        {
            uint8_t rm = op2 & 0x7;
            sprintf( ac, "sla %s", reg_strings[ rm ] );
        }
        else if ( 0x28 == ( op2 & 0xf8 ) ) // sra = shift right arithmetic
        {
            uint8_t rm = op2 & 0x7;
            sprintf( ac, "sra %s", reg_strings[ rm ] );
        }
        else if ( op2 >= 0x30 && op2 <= 0x3f )
        {
            uint8_t rm = op2 & 0x7;
            if ( op2 <= 0x37 )
                sprintf( ac, "sll %s", reg_strings[ rm ] );
            else
                sprintf( ac, "srl %s", reg_strings[ rm ] );
        }
        else if ( 0x38 == ( op2 & 0xf8 ) ) // srl = shift right logical
        {
            uint8_t rm = op2 >> 4;
            sprintf( ac, "srl %s", reg_strings[ rm ] );
        }
        else if ( op2 <= 0x1f ) // rlc, rl, rrc, rr on rm
        {
            const char * rotateType = "rlc";
            if ( op2 >= 0x10 && op2 <= 0x17 )
                rotateType = "rl";
            else if ( op2 >= 0x08 && op2 <= 0x0f )
                rotateType = "rrc";
            else if ( op2 >= 0x18 && op2 <= 0x1f )
                rotateType = "rr";

            uint8_t rm = ( op2 & 0x7 );
            sprintf( ac, "%s %s", rotateType, reg_strings[ rm ] );
        }
        else if ( op2 >= 0x40 && op2 <= 0x7f ) // bit #, rm
        {
            uint8_t rm = op2 & 0x7;
            uint8_t bit = ( op2 >> 3 ) & 0x7;
            sprintf( ac, "bit %d, %s", bit, reg_strings[ rm ] );
        }
        else if ( op2 >= 0x80 && op2 <= 0xbf ) // res bit #, rm
        {
            uint8_t rm = op2 & 0x7;
            uint8_t bit = ( op2 >> 3 ) & 0x7;
            sprintf( ac, "res %d, %s", bit, reg_strings[ rm ] ); 
        }
        else if ( op2 >= 0xc0 && op2 <= 0xff ) // set bit #, rm
        {
            uint8_t rm = op2 & 0x7;
            uint8_t bit = ( op2 >> 3 ) & 0x7;
            sprintf( ac, "set %d, %s", bit, reg_strings[ rm ] ); 
        }
    }
    else if ( 0x08 == op )
        strcpy( ac, "ex af, af'" );
    else if ( 0x10 == op )
        sprintf( ac, "djnz %d", op2int );
    else if ( 0x18 == op )
        sprintf( ac, "jr %d", op2int );    
    else if ( 0x20 == op )
        sprintf( ac, "jr nz, %d", op2int );
    else if ( 0x28 == op )
        sprintf( ac, "jr z, %d", op2int );
    else if ( 0x30 == op )
        sprintf( ac, "jr nc, %d", op2int );
    else if ( 0x38 == op )
        sprintf( ac, "jr c, %d", op2int );
    else if ( 0xd9 == op )
        strcpy( ac, "exx" );
} //z80_render

void replace_with_num( char * pc, const char * psearch, uint16_t num, uint8_t width )
{
    char actemp[ 60 ];
    sprintf( actemp, ( 16 == width ) ? "0%04xh" : "0%02xh", (uint32_t) num );
    size_t len = strlen( actemp );
    strcat( actemp, pc + strlen( psearch ) );
    strcpy( pc, actemp );
} //replace_with_num

const char * x80_render_operation( uint16_t address )
{
    static char ac[ 60 ];
    uint8_t op = memory[ address ];
    bool renderData = true;

    if ( reg.fZ80Mode )
    {
        strcpy( ac, z80_ins[ op ].assembly );
        if ( '*' == ac[ 0 ] )
        {
            z80_render( ac, op, address );
            renderData = false;
        }
    }
    else
        strcpy( ac, ins_8080[ op ].assembly );

    if ( renderData )
    {
        char * p;
        if ( p = strstr( ac, "d16" ) )
            replace_with_num( p, "d16", mword( address + 1 ), 16 );
        else if ( p = strstr( ac, "a16" ) )
            replace_with_num( p, "a16", mword( address + 1 ), 16 );
        else if ( p = strstr( ac, "d8" ) )
            replace_with_num( p, "d8", memory[ address + 1 ], 8 );
    }

    return ac;
} //x80_render_operation

void x80_trace_state()
{
    uint8_t op = memory[ reg.pc ];
    uint8_t op2 = memory[ reg.pc + 1 ];
    uint8_t op3 = memory[ reg.pc + 2 ];
    uint8_t op4 = memory[ reg.pc + 3 ];

    if ( reg.fZ80Mode )
        tracer.Trace( "pc %04x, op %02x, op2 %02x, op3 %02x, op4 %02x, a %02x, B %04x, D %04x, H %04x, ix %04x, iy %04x, sp %04x, %s, %s\n",
                      (uint32_t) reg.pc, (uint32_t) op, (uint32_t) op2, (uint32_t) op3, (uint32_t) op4,
                      (uint32_t) reg.a, (uint32_t) reg.B(), (uint32_t) reg.D(),
                      (uint32_t) reg.H(), (uint32_t) reg.ix, (uint32_t) reg.iy, (uint32_t) reg.sp,
                      reg.renderFlags(), x80_render_operation( reg.pc ) );
    else
        tracer.Trace( "pc %04x, op %02x, op2 %02x, op3 %02x, a %02x, B %04x, D %04x, H %04x, sp %04x, %s, %s\n",
                      (uint32_t) reg.pc, (uint32_t) op, (uint32_t) op2, (uint32_t) op3,
                      (uint32_t) reg.a, (uint32_t) reg.B(), (uint32_t) reg.D(),
                      (uint32_t) reg.H(), (uint32_t) reg.sp,
                      reg.renderFlags(), x80_render_operation( reg.pc ) );
} //x80_trace_state

bool check_conditional( uint8_t op ) // checks for conditional jump, call, and return
{
    bool condition = reg.getFlag( ( op & 0x30 ) >> 4 );
    if ( ( 0 == ( op & 0x8 ) ) )
        condition = !condition;

    return condition;
} //check_conditional

uint64_t x80_emulate( uint64_t maxcycles )
{
    uint64_t cycles = 0;

    while ( cycles < maxcycles )        // 10.5% of runtime checking this for zexall.com
    {
        if ( g_traceInstructions )      // 2% of runtime is checking this flag
            x80_trace_state();

        uint8_t op = memory[ reg.pc ];  // 2% of runtime here
        reg.pc++;

_restart_op:
        if ( '*' == ins_8080[ op ].assembly[0] ) // 17% of runtime is checking this
        {
            if ( reg.fZ80Mode )
            {
                reg.incR();
                cycles += z80_emulate( op );
                continue;
            }
            else
            {
                tracer.Trace( "Error: 8080 undocumented instruction: %#x\n", op );
                printf( "Error: 8080 undocumented instruction: %#x\n", op );
                exit( 1 );
            }
        }
        else if ( reg.fZ80Mode )        // 4% of runtime is checking this
        {
            reg.incR();
            cycles += z80_ins[ op ].cycles;
        }
        else
            cycles += ins_8080[ op ].cycles;

        bool handled = true;            // 4% of runtime clearing this
        switch ( op )                   // 10.5% of runtime is setting up for the jump table jump
        {
            case 0x00: /*nop*/ break;
            case 0x01: /*lxi b, d16*/ reg.SetB( pcword() ); break;
            case 0x02: /*stax b*/ memory[ reg.B() ] = reg.a; break;
            case 0x07: /*rlc*/
            {
                reg.fCarry = ( 0 != ( reg.a & 0x80 ) );
                reg.a <<= 1;
                if ( reg.fCarry)
                    reg.a |= 1;
                else
                    reg.a &= 0xfe;

                if ( reg.fZ80Mode )
                {
                    reg.clearHN();
                    reg.assignYX( reg.a );
                }
                break;
            }
            case 0x09: /*dad b*/ { op_dad( reg.B() ); break; }
            case 0x0a: /*ldax b*/ reg.a = memory[ reg.B() ]; break;
            case 0x0f: /*rrc*/
            {
                reg.fCarry = ( 0 != ( reg.a & 1 ) );
                reg.a >>= 1;
                if ( reg.fCarry )
                    reg.a |= 0x80;
                if ( reg.fZ80Mode )
                {
                    reg.clearHN();
                    reg.assignYX( reg.a );
                }
                break;
            }
            case 0x11: /*lxi d, d16*/ reg.SetD( pcword() ); break;
            case 0x12: /*stax d*/ memory[ reg.D() ] = reg.a; break;
            case 0x17: /*ral*/
            {
                bool c = reg.fCarry;
                reg.fCarry = ( 0 != ( 0x80 & reg.a ) );
                reg.a <<= 1;
                if ( c )
                    reg.a |= 1;
                else
                    reg.a &= 0xfe;
                if ( reg.fZ80Mode )
                {
                    reg.clearHN();
                    reg.assignYX( reg.a );
                }
                break;
            }
            case 0x19: /*dad d*/ { op_dad( reg.D() ); break; }
            case 0x1a: /*ldax d*/ reg.a = memory[ reg.D() ]; break;
            case 0x1f: /*rar*/
            {
                bool c = reg.fCarry;
                reg.fCarry = ( 0 != ( reg.a & 1 ) );
                reg.a >>= 1;
                if ( c )
                    reg.a |= 0x80;
                else
                    reg.a &= 0x7f;
                if ( reg.fZ80Mode )
                {
                    reg.clearHN();
                    reg.assignYX( reg.a );
                }
                break;
            }
            case 0x21: /*lxi h, d16*/ reg.SetH( pcword() ); break;
            case 0x22: /*shld*/ { uint16_t off = pcword(); setmword( off, reg.H() ); break; }
            case 0x27: /*daa*/
            {
                if ( reg.fZ80Mode ) // this logic pulled from Sean Young's doc
                {
                    uint8_t diff = 0x66;
                    uint8_t hn = ( ( reg.a >> 4 ) & 0xf );
                    uint8_t ln = ( reg.a & 0xf );

                    if ( reg.fCarry )
                    {
                        if ( !reg.fAuxCarry && ln <= 9 )
                            diff = 0x60;
                    }
                    else
                    {
                        if ( hn <= 9 && !reg.fAuxCarry && ln <= 9 )
                            diff = 0;
                        else if ( ( hn <= 9 && reg.fAuxCarry && ln <= 9 ) || ( hn <= 8 && ln > 9 ) )
                            diff = 6;
                        else if ( hn > 9 && !reg.fAuxCarry && ln <= 9 )
                            diff = 0x60;
                    }

                    bool newCarry = reg.fCarry;
                    if ( !reg.fCarry )
                    {
                        if ( ( hn <= 9 && ln <= 9 ) || ( hn <= 8 && ln > 9 ) )
                            newCarry = false;
                        else if ( ( hn >= 9 && ln > 9 ) || ( hn > 9 && ln <= 9 ) )
                            newCarry = true;
                    }

                    bool newAuxCarry = reg.fAuxCarry;
                    if ( reg.fWasSubtract )
                    {
                        if ( !reg.fAuxCarry )
                            newAuxCarry = false;
                        else
                            newAuxCarry = ( ln < 6 );
                    }
                    else
                        newAuxCarry = ( ln > 9 );

                    if ( reg.fWasSubtract )
                        reg.a -= diff;
                    else
                        reg.a += diff;

                    set_sign_zero_parity( reg.a );
                    reg.assignYX( reg.a );
                    reg.fCarry = newCarry;
                    reg.fAuxCarry = newAuxCarry;
                }
                else
                {
                    uint8_t loNibble = reg.a & 0xf;
                    uint8_t toadd = 0;
                    if ( reg.fAuxCarry || ( loNibble > 9 ) )
                        toadd = 6;
    
                    bool carry = reg.fCarry;
                    uint8_t hiNibble = reg.a & 0xf0;
                    if ( ( hiNibble > 0x90 ) || ( hiNibble >= 0x90 && loNibble > 0x9 ) || carry )
                    {
                        toadd |= 0x60;
                        carry = true;
                    }
    
                    op_add( toadd );
                    reg.fCarry = carry; // this doesn't change regardless of the result
                }
                break;
            }
            case 0x29: /*dad h*/ { op_dad( reg.H() ); break; }
            case 0x2a: /*lhld*/ { reg.SetH( mword( pcword() ) ); break; }
            case 0x2f: /*cma*/
            {
                reg.a = ~reg.a;
                if ( reg.fZ80Mode )
                {
                    reg.fAuxCarry = true;
                    reg.fWasSubtract = true;
                    reg.assignYX( reg.a );
                }
                break;
            }
            case 0x31: /*lxi sp, d16*/ reg.sp = pcword(); break;
            case 0x32: /*sta a16*/ memory[ pcword() ] = reg.a; break;
            case 0x37: /*stc*/
            {
                reg.fCarry = 1;
                if ( reg.fZ80Mode )
                {
                    reg.clearHN();
                    reg.assignYX( reg.a );
                }
                break;
            }
            case 0x39: /*dad sp*/ { op_dad( reg.sp ); break; }
            case 0x3a: /*lda a16*/ { reg.a = memory[ pcword() ]; break; }
            case 0x3f: /*cmc*/
            {
                reg.fCarry = !reg.fCarry;
                if ( reg.fZ80Mode )
                {
                    reg.fWasSubtract = false;
                    reg.fAuxCarry = !reg.fCarry;
                    reg.assignYX( reg.a );
                }
                break;
            }
            case 0x64: /*hook*/ { op = x80_invoke_hook(); if ( reg.fZ80Mode ) reg.decR(); goto _restart_op; }
            case 0x76: /*hlt*/ { x80_invoke_halt(); goto _all_done; }
            case 0xc3: /*jmp a16*/ { reg.pc = pcword(); break; }
            case 0xc9: /*ret*/ { reg.pc = popword(); break; }
            case 0xcd: /*call a16*/ { uint16_t v = pcword(); pushword( reg.pc ); reg.pc = v; break; }
            case 0xd3: /*out d8*/ { x80_invoke_out( pcbyte() ); break; }
            case 0xdb: /*in d8*/ { x80_invoke_in( pcbyte() ); break; }
            case 0xe3: /*xthl*/ { uint16_t t = reg.H(); reg.SetH( mword( reg.sp ) ); setmword( reg.sp, t ); break; }
            case 0xe9: /*pchl*/ { reg.pc = reg.H(); break; }
            case 0xeb: /*xchg*/ { uint16_t t = reg.H(); reg.SetH( reg.D() ); reg.SetD( t ); break; }
            case 0xf3: /*di*/ break;
            case 0xf9: /*sphl*/ { reg.sp = reg.H(); break; }
            case 0xfb: /*di*/ break;
            default:
            {
                handled = false;
                break;
            }
        }

        if ( !handled )
        {
            uint8_t op2reg3 = ( op & 0xc7 ); // two bits, 3 reg bits, 3 bits. 2% of runtime
            uint8_t op2rp4 = ( op & 0xcf );  // two bits, 2 rp bits, 4 bits

            if ( 0x80 == ( op & 0xc0 ) ) // math
            {
                // op: 5..3. reg 2..0
                // op: 0 add, 1 adc, 2 sub, 3 sbb, 4 ana, 5 xra, 6 ora, 7 cmp

                uint8_t math = ( op >> 3 ) & 0x7;
                uint8_t src = src_value( op );
                op_math( math, src );             // 4% of runtime
            }
            else if ( 5 == op2reg3 ) // dcr r. does not set carry
            {
                uint8_t * pdst = dst_address( op );
                *pdst = op_dec( * pdst );         // 5% of runtime
            }
            else if  ( op >= 0x40 && op <= 0x7f ) // mov r, r.  hlt is in this range but handled above.
            {
                uint8_t src = 0x7 & op;
                *dst_address( op ) = ( 6 == src ) ? memory[ reg.H() ] : * reg.regOffset( src ); // 5% of runtime
            }
            else if ( 0xc6 == op2reg3 ) // immediate math. 0 adi, 1 aci, 2 sui, 3 sbi, 4 ani, 5 xri, 6 ori, 7 cpi  
            {
                uint8_t math = ( op >> 3 ) & 0x7;
                op_math( math, pcbyte() );         // 4% of runtime
            }
            else if ( 6 == op2reg3 ) // mvi r, d8
            {
                uint8_t *pdst = dst_address( op );
                *pdst = pcbyte();
            }
            else if ( 0xc0 == op2reg3 ) // conditional return
            {
                if ( check_conditional( op ) )
                    reg.pc = popword();
                else
                    cycles -= cyclesnt;
            }
            else if ( 0xc2 == op2reg3 ) // conditional jump
            {
                uint16_t address = pcword();
                if ( check_conditional( op ) )
                    reg.pc = address;
                else
                    cycles -= cyclesnt;
            }
            else if ( 0xc4 == op2reg3 ) // conditional call
            {
                uint16_t address = pcword();

                if ( check_conditional( op ) )
                {
                    pushword( reg.pc );
                    reg.pc = address;
                }
                else
                    cycles -= cyclesnt;
            }
            else if ( 0xc1 == op2rp4 ) // pop
            {
                uint16_t val = popword();
                if ( 0xc1 == op )
                    reg.SetB( val );
                else if ( 0xd1 == op )
                    reg.SetD( val );
                else if ( 0xe1 == op )
                    reg.SetH( val );
                else
                    reg.SetPSW( val );
            }
            else if ( 0xc5 == op2rp4 ) // push
            {
                if ( 0xe5 == op )            // HL is most frequent push
                    pushword( reg.H() );
                else if ( 0xc5 == op )
                    pushword( reg.B() );
                else if ( 0xd5 == op )
                    pushword( reg.D() );
                else
                    pushword( reg.PSW() );
            }
            else if ( 3 == op2rp4 ) // inx. no status flag updates
            {
                uint16_t * pdst = reg.rpAddressFromOp( op );
                *pdst = 1 + *pdst;
            }
            else if ( 0x0b == op2rp4 ) // dcx. no status flag updates
            {
                uint16_t * pdst = reg.rpAddressFromOp( op );
                *pdst = *pdst - 1;
            }
            else if ( 4 == op2reg3 ) // inr r. does not set carry
            {
                uint8_t * pdst = dst_address( op );
                *pdst = op_inc( *pdst );
            }
            else if ( 0xc3 == ( 0xc3 & op ) ) // rst
            {
                // bits 5..3 are exp, which form an address 0000000000exp000 that is called

                pushword( reg.pc );
                reg.pc = 0x38 & (uint16_t) op;
            }
            else
            {
                tracer.Trace( "unimplemented instruction: %#x, next byte is %#x\n", op, memory[ reg.pc ] );
                printf( "unimplemented instruction: %#x, next byte is %#x\n", op, memory[ reg.pc ] );
                exit( 1 );
            }
        }
    }
_all_done:
    return cycles;
} //x80_emulate
