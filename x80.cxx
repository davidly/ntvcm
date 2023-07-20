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
//          https://onlinedisassembler.com/odaweb/
// symbols that start with z80_ are (unsurprisingly) Z80-specific. 80% of this code is Z80-specific.
// 8080 emulation would be >20% faster if not for the z80 checks
// Validated 100% pass for 8080 with 8080ex1.com, 8080pre.com, Microcosm v1.0, and cputest.com Diagnostics II V1.2 in 8080 mode.
// Validated 100% pass for Z80 with zexall.com, zexdoc.com, and cputest.com in Z80 mode.

#include <stdio.h>
#include <memory.h>
#include <assert.h>
#include <vector>
#include <cstring>
#include <bitset>
#include <djl_os.hxx>
#include <djltrace.hxx>

using namespace std;

#include "x80.hxx"

uint8_t memory[ 65536 ];
registers reg;
static const char * reg_strings[ 8 ] = { "b", "c", "d", "e", "h", "l", "m", "a" };
static const char * rp_strings[ 4 ] = { "bc", "de", "hl", "sp" };
static const char * z80_math_strings[ 8 ] = { "add", "adc", "sub", "sbb", "and", "xor", "or", "cp" };
static const char * z80_rotate_strings[ 8 ] = { "rlc", "rrc", "rl", "rr", "sla", "sra", "sll", "srl" };
static bool g_traceInstructions = false;
enum z80_value_source { vs_register, vs_memory, vs_indexed }; // this impacts how Z80 undocumented Y and X flags are updated
void x80_trace_instructions( bool t ) { g_traceInstructions = t; }

const uint8_t cyclesnt = 6;  // cycles not taken when a conditional call, jump, or return isn't taken

// instructions starting with '*' are undocumented for 8080, and not implemented here. They also signal likely Z80 instructions
static const char i8080_instructions[ 256 ][ 16 ] =
{
    /*00*/  "nop",      "lxi b, d16",  "stax b",   "inx b",      "inr b",    "dcr b",     "mvi b, d8", "rlc",
    /*08*/  "*nop",     "dad b",       "ldax b",   "dcx b",      "inr c",    "dcr c",     "mvi c, d8", "rrc",
    /*10*/  "*nop",     "lxi d, d16",  "stax d",   "inx d",      "inr d",    "dcr d",     "mvi d, d8", "ral",
    /*18*/  "*nop",     "dad d",       "ldax d",   "dcx d",      "inr e",    "dcr e",     "mvi e, d8", "rar",
    /*20*/  "*nop",     "lxi h, d16",  "shld a16", "inx h",      "inr h",    "dcr h",     "mvi h, d8", "daa",
    /*28*/  "*nop",     "dad h",       "lhld a16", "dcx h",      "inr l",    "dcr l",     "mvi l, d8", "cma",
    /*30*/  "*nop",     "lxi sp, d16", "sta a16",  "inx sp",     "inr m",    "dcr m",     "mvi m, d8", "stc",
    /*38*/  "*nop",     "dad sp",      "lda a16",  "dcx sp",     "inr a",    "dcr a",     "mvi a, d8", "cmc",
    /*40*/  "mov b, b", "mov b, c",    "mov b, d", "mov b, e",   "mov b, h", "mov b, l",  "mov b, m",  "mov b, a",
    /*48*/  "mov c, b", "mov c, c",    "mov c, d", "mov c, e",   "mov c, h", "mov c, l",  "mov c, m",  "mov c, a",
    /*50*/  "mov d, b", "mov d, c",    "mov d, d", "mov d, e",   "mov d, h", "mov d, l",  "mov d, m",  "mov d, a",
    /*58*/  "mov e, b", "mov e, c",    "mov e, d", "mov e, e",   "mov e, h", "mov e, l",  "mov e, m",  "mov e, a",
    /*60*/  "mov h, b", "mov h, c",    "mov h, d", "mov h, e",   "(hook)",   "mov h, l",  "mov h, m",  "mov h, a",
    /*68*/  "mov l, b", "mov l, c",    "mov l, d", "mov l, e",   "mov l, h", "mov l, l",  "mov l, m",  "mov l, a",
    /*70*/  "mov m, b", "mov m, c",    "mov m, d", "mov m, e",   "mov m, h", "mov m, l",  "hlt",       "mov m, a",
    /*78*/  "mov a, b", "mov a, c",    "mov a, d", "mov a, e",   "mov a, h", "mov a, l",  "mov a, m",  "mov a, a",
    /*80*/  "add b",    "add c",       "add d",    "add e",      "add h",    "add l",     "add m",     "add a",
    /*88*/  "adc b",    "adc c",       "adc d",    "adc e",      "adc h",    "adc l",     "adc m",     "adc a",
    /*90*/  "sub b",    "sub c",       "sub d",    "sub e",      "sub h",    "sub l",     "sub m",     "sub a",
    /*98*/  "sbb b",    "sbb c",       "sbb d",    "sbb e",      "sbb h",    "sbb l",     "sbb m",     "sbb a",
    /*a0*/  "ana b",    "ana c",       "ana d",    "ana e",      "ana h",    "ana l",     "ana m",     "ana a",
    /*a8*/  "xra b",    "xra c",       "xra d",    "xra e",      "xra h",    "xra l",     "xra m",     "xra a",
    /*b0*/  "ora b",    "ora c",       "ora d",    "ora e",      "ora h",    "ora l",     "ora m",     "ora a",
    /*b8*/  "cmp b",    "cmp c",       "cmp d",    "cmp e",      "cmp h",    "cmp l",     "cmp m",     "cmp a",
    /*c0*/  "rnz",      "pop b",       "jnz a16",  "jmp a16",    "cnz a16",  "push b",    "adi d8",    "rst 0",
    /*c8*/  "rz",       "ret",         "jz a16",   "*jmp",       "cz a16",   "call a16",  "aci d8",    "rst 1",
    /*d0*/  "rnc",      "pop d",       "jnc a16",  "out d8",     "cnc a16",  "push d",    "sui d8",    "rst 2",
    /*d8*/  "rc",       "*ret",        "jc a16",   "in d8",      "cc a16",   "*call a16", "sbi d8",    "rst 3",
    /*e0*/  "rpo",      "pop h",       "jpo a16",  "xthl",       "cpo a16",  "push h",    "ani d8",    "rst 4",
    /*e8*/  "rpe",      "pchl",        "jpe a16",  "xchg",       "cpe a16",  "*call a16", "xri d8",    "rst 5",
    /*f0*/  "rp",       "pop psw",     "jp a16",   "di",         "cp a16",   "push psw",  "ori d8",    "rst 6",
    /*f8*/  "rm",       "sphl",        "jm a16",   "ei",         "cm a16",   "*call a16", "cpi d8",    "rst 7",
};

// instructions starting with '*' are Z80-specific, generally multi-byte, and handled separately.
// instructions listed here are the overlap with 8080 but with the Z80 naming.
static const char z80_instructions[ 256 ][ 16 ] =
{
    /*00*/ "nop",       "ld bc,d16", "ld (bc),a",   "inc bc",       "inc b",       "dec b",     "ld b,d8",    "rlca",
    /*08*/ "*'",        "add hl,bc", "ld a,(bc)",   "dec bc",       "inc c",       "dec c",     "ld c,d8",    "rrca",
    /*10*/ "*",         "ld de,d16", "ld (de),a",   "inc de",       "inc d",       "dec d",     "ld d,d8",    "rla",
    /*18*/ "*",         "add hl,de", "ld a,(de)",   "dec de",       "inc e",       "dec e",     "ld e,d8",    "rra",
    /*20*/ "*",         "ld hl,d16", "ld (a16),hl", "inc hl",       "inc h",       "dec h",     "ld h,d8",    "daa",
    /*28*/ "*",         "add hl,hl", "ld hl,(a16)", "dec hl",       "inc l",       "dec l",     "ld l,d8",    "cpl",
    /*30*/ "*",         "ld sp,d16", "ld (a16),a",  "inc sp",       "inc (hl)",    "dec (hl)",  "ld m,d8",    "scf",
    /*38*/ "*",         "add hl,sp", "ld a,(a16)",  "dec sp",       "inc a",       "dec a",     "ld a,d8",    "ccf",
    /*40*/ "ld b,b",    "ld b,c",    "ld b,d",      "ld b,e",       "ld b,h",      "ld b,l",    "ld b, (hl)", "ld b,a",
    /*48*/ "ld c,b",    "ld c,c",    "ld c,d",      "ld c,e",       "ld c,h",      "ld c,l",    "ld c, (hl)", "ld c,a",
    /*20*/ "ld d,b",    "ld d,c",    "ld d,d",      "ld d,e",       "ld d,h",      "ld d,l",    "ld d, (hl)", "ld d,a",
    /*28*/ "ld e,b",    "ld e,c",    "ld e,d",      "ld e,e",       "ld e,h",      "ld e,l",    "ld e, (hl)", "ld e,a",
    /*60*/ "ld h,b",    "ld h,c",    "ld h,d",      "ld h,e",       "(hook)",      "ld h,l",    "ld h, (hl)", "ld h,a",
    /*68*/ "ld l,b",    "ld l,c",    "ld l,d",      "ld l,e",       "ld l,h",      "ld l,l",    "ld l, (hl)", "ld l,a",
    /*70*/ "ld (hl),b", "ld (hl),c", "ld (hl),d",   "ld (hl),e",    "ld (hl),h",   "ld (hl),l", "halt",       "ld (hl),a",
    /*78*/ "ld a,b",    "ld a,c",    "ld a,d",      "ld a,e",       "ld a,h",      "ld a,l",    "ld a,(hl)",  "ld a,a",
    /*80*/ "add a,b",   "add a,c",   "add a,d",     "add a,e",      "add a,h",     "add a,l",   "add a,(hl)", "add a,a",
    /*88*/ "adc a,b",   "adc a,c",   "adc a,d",     "adc a,e",      "adc a,h",     "adc a,l",   "adc a,(hl)", "adc a,a",
    /*90*/ "sub b",     "sub c",     "sub d",       "sub e",        "sub h",       "sub l",     "sub (hl)",   "sub a",
    /*98*/ "sbc b",     "sbc c",     "sbc d",       "sbc e",        "sbc h",       "sbc l",     "sbc (hl)",   "sbc a",
    /*a0*/ "and b",     "and c",     "and d",       "and e",        "and h",       "and l",     "and (hl)",   "and a",
    /*a8*/ "xor b",     "xor c",     "xor d",       "xor e",        "xor h",       "xor l",     "xor (hl)",   "xor a",
    /*b0*/ "or b",      "or c",      "or d",        "or e",         "or h",        "or l",      "or (hl)",    "or a",
    /*b8*/ "cp b",      "cp c",      "cp d",        "cp e",         "cp h",        "cp l",      "cp (hl)",    "cp a",
    /*c0*/ "ret nz",    "pop bc",    "jp nz,a16",   "jp a16",       "call nz,a16", "push bc",   "add a,d8",   "rst 0",
    /*c8*/ "ret z",     "ret",       "jp z,a16",    "*",            "call z,a16",  "call a16",  "adc a,d8",   "rst 1",
    /*d0*/ "ret nc",    "pop de",    "jp nc,a16",   "out (d8),a",   "call nc,a16", "push de",   "sub d8",     "rst 2",
    /*d8*/ "ret c",     "*",         "jp c,a16",    "in a,(d8)",    "call c,a16",  "*",         "sbc d8",     "rst 3",
    /*e0*/ "ret po",    "pop hl",    "jp po,a16",   "ex (sp),hl",   "call po,a16", "push hl",   "and d8",     "rst 4",
    /*e8*/ "ret pe",    "jp (hl)",   "jp pe,a16",   "ex de,hl",     "call pe,a16", "*",         "xor d8",     "rst 5",
    /*f0*/ "ret p",     "pop af",    "jp p,a16",    "di",           "call p,a16",  "push af",   "or d8",      "rst 6",
    /*f8*/ "ret m",     "ld sp,hl",  "jp m,a16",    "ei",           "call m,a16",  "*",         "cp d8",      "rst 7",
};

// base cycles. 8080 conditional calls take 11 (not 17) if not taken. conditional returns take 5 (not 11) if not taken.
typedef uint8_t acycles_t[ 256 ];
static const acycles_t i8080_cycles =
{
    /*00*/  4, 10,  7,  5,  5,  5,  7,  4,    4, 10,  7,  5,  5,  5,  7,  4,
    /*10*/  4, 10,  7,  5,  5,  5,  7,  4,    4, 10,  7,  5,  5,  5,  7,  4,
    /*20*/  4, 10, 16,  5,  5,  5,  7,  4,    4, 10, 16,  5,  5,  5,  7,  4,
    /*30*/  4, 10, 13,  5, 10, 10, 10,  4,    4, 10, 13,  5,  5,  5,  7,  4,
    /*40*/  5,  5,  5,  5,  5,  5,  7,  5,    5,  5,  5,  5,  5,  5,  7,  5,
    /*50*/  5,  5,  5,  5,  5,  5,  7,  5,    5,  5,  5,  5,  5,  5,  7,  5,
    /*60*/  5,  5,  5,  5,  0,  5,  7,  5,    5,  5,  5,  5,  5,  5,  7,  5,
    /*70*/  7,  7,  7,  7,  7,  7,  7,  7,    5,  5,  5,  5,  5,  5,  7,  5,
    /*80*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*90*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*a0*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*b0*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*c0*/ 11, 10, 10, 10, 17, 11,  7, 11,   11, 10, 10, 10, 17, 17,  7, 11,
    /*d0*/ 11, 10, 10, 10, 17, 11,  7, 11,   11, 10, 10, 10, 17, 17,  7, 11,
    /*e0*/ 11, 10, 10, 18, 17, 11,  7, 11,   11,  5, 10,  5, 17, 17,  7, 11,
    /*f0*/ 11, 10, 10,  4, 17, 11,  7, 11,   11,  5, 10,  4, 17, 17,  7, 11,
};

static const acycles_t z80_cycles =
{
    /*00*/  4, 10,  7,  6,  4,  4,  7,  4,    4, 11,  7,  6,  4,  4,  7,  4,
    /*10*/  0, 10,  7,  6,  4,  4,  7,  4,    0, 11,  7,  6,  4,  4,  7,  4,
    /*20*/  0, 10, 16,  6,  4,  4,  7,  4,    0, 11, 20,  6,  4,  4,  7,  4,
    /*30*/  0, 10, 13,  6, 11, 11, 10,  4,    0, 11, 13,  6,  4,  4,  7,  4,
    /*40*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*20*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*60*/  4,  4,  4,  4,  0,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*70*/  7,  7,  7,  7,  7,  7,  4,  7,    4,  4,  4,  4,  4,  4,  7,  4,
    /*80*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*90*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*a0*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*b0*/  4,  4,  4,  4,  4,  4,  7,  4,    4,  4,  4,  4,  4,  4,  7,  4,
    /*c0*/ 11, 10, 10, 10, 17, 11,  7, 11,   11, 10, 10,  0, 17, 17,  7, 11,
    /*d0*/ 11, 10, 10, 11, 17, 11,  7, 11,   11,  0, 10, 11, 17,  0,  7, 11,
    /*e0*/ 11, 10, 10, 19, 17, 11,  7, 11,   11,  4, 10,  4, 17,  0,  7, 11,
    /*f0*/ 11, 10, 10,  4, 17, 11,  7, 11,   11,  5, 10,  4, 17,  0,  7, 11,
};

static uint16_t mword( uint16_t offset ) { return * ( (uint16_t *) & memory[ offset ] ); }
static void setmword( uint16_t offset, uint16_t value ) { * (uint16_t *) & memory[ offset ] = value; }
static uint8_t pcbyte() { return memory[ reg.pc++ ]; }
static uint16_t pcword() { uint16_t r = mword( reg.pc ); reg.pc += 2; return r; }
static void pushword( uint16_t val ) { reg.sp -= 2; setmword( reg.sp, val ); }
static uint16_t popword() {  uint16_t val = mword( reg.sp ); reg.sp += 2; return val; }

bool is_parity_even( uint8_t x )
{
#if defined(_M_AMD64) && !defined(__GNUC__)
    return ( ! ( __popcnt16( x ) & 1 ) ); // less portable, but faster
#else
    return ( ! ( std::bitset<8>( x ).count() & 1 ) );
#endif
} //is_parity_even

void set_parity( uint8_t x ) { reg.fParityEven_Overflow = is_parity_even( x ); }

void set_sign_zero( uint8_t x )
{
    reg.fSign = ( 0 != ( 0x80 & x ) );
    reg.fZero = ( 0 == x );
} //set_sign_zero

void set_sign_zero_parity( uint8_t x )
{
    set_sign_zero( x );
    set_parity( x );
} //set_sign_zero_parity

void z80_set_sign_zero_16( uint16_t x )
{
    reg.fSign = ( 0 != ( 0x8000 & x ) );
    reg.fZero = ( 0 == x );
} //z80_set_sign_zero_16

uint8_t op_inc( uint8_t x )
{
    x++;
    reg.fAuxCarry = ( 0 == ( x & 0xf ) );
    set_sign_zero( x );

    if ( reg.fZ80Mode )
    {
        reg.fParityEven_Overflow = ( x == 0x80 );
        reg.fWasSubtract = false;
        reg.z80_assignYX( x );
    }
    else
        set_parity( x );
    return x;
} //op_inc

uint8_t op_dec( uint8_t x )
{
    uint8_t result = x - 1;
    set_sign_zero( result );

    if ( reg.fZ80Mode )
    {
        reg.fParityEven_Overflow = ( x == 0x80 );
        reg.fWasSubtract = true;

        // Aux / Half carry returns the opposite value on Z80 as on 8080
        reg.fAuxCarry = ( 0xf == ( result & 0xf ) );
        reg.z80_assignYX( result );
    }
    else
    {
        set_parity( result );
        reg.fAuxCarry = ( 0xf != ( result & 0xf ) );
    }

    return result;
} //op_dec

force_inlined void op_add( uint8_t x, bool carry = false )
{
    uint16_t carry_int = carry ? 1 : 0;
    uint16_t r16 = (uint16_t) reg.a + (uint16_t) x + carry_int;
    uint8_t r8 = r16 & 0xff;
    reg.fCarry = ( 0 != ( r16 & 0x0100 ) );

    // low nibble add + carry overflows to high nibble

    reg.fAuxCarry = ( 0 != ( ( ( 0xf & reg.a ) + ( 0xf & x ) + carry_int ) & 0x10 ) );
    set_sign_zero( r8 );

    // if not ( ( one of lhs and rhs are negative ) and ( one of lhs and result are negative ) )

    if ( reg.fZ80Mode )
    {
        reg.fParityEven_Overflow = ( ! ( ( reg.a ^ x ) & 0x80 ) ) && ( ( reg.a ^ r8 ) & 0x80 );
        reg.fWasSubtract = false;
        reg.z80_assignYX( r8 );
    }
    else
        set_parity( r8 );

    reg.a = r8;
} //op_add

void op_adc( uint8_t x )
{
    op_add( x, reg.fCarry );
} //op_adc

force_inlined uint8_t op_sub( uint8_t x, bool borrow = false )
{
    // com == ones-complement
    uint8_t com_x = ~x;
    uint8_t borrow_int = borrow ? 0 : 1;
    uint16_t res16 =  (uint16_t) reg.a + (uint16_t) com_x + (uint16_t) borrow_int;
    uint8_t res8 = res16 & 0xff;
    reg.fCarry = ( 0 == ( res16 & 0x100 ) );
    set_sign_zero( res8 );

    if ( reg.fZ80Mode )
    {
        // if not ( ( one of lhs and com_x are negative ) and ( one of lhs and result are negative ) )

        reg.fParityEven_Overflow = ! ( ( reg.a ^ com_x ) & 0x80 ) && ( ( reg.a ^ res8 ) & 0x80 );
        reg.fWasSubtract = true;
        uint8_t resultH = reg.a - x - ( borrow ? 1 : 0 );
        uint8_t xorResultH = reg.a ^ x ^ resultH;
        reg.fAuxCarry = ( 0 != ( 0x10 & xorResultH ) );
        reg.z80_assignYX( res8 );
    }
    else
    {
        set_parity( res8 );
        reg.fAuxCarry = ( 0 != ( ( ( reg.a & 0xf ) + ( com_x & 0xf ) + borrow_int ) & 0x10 ) );
    }

    // op_sub is the only op_X that doesn't update reg.a because it's also used for op_cmp
    return res8;
} //op_sub

void op_sbb( uint8_t x )
{
    reg.a = op_sub( x, reg.fCarry );
} //op_sbb

void op_cmp( uint8_t x )
{
    op_sub( x, false );
    if ( reg.fZ80Mode )
        reg.z80_assignYX( x ); // done on operand, not the result or reg.a
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
        reg.z80_assignYX( reg.a );
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
        reg.z80_assignYX( reg.a );
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
        reg.z80_assignYX( reg.a );
    }
} //op_xra

void op_math( uint8_t opcode, uint8_t src )
{
    uint8_t math = ( opcode >> 3 ) & 7;
    assert( math <= 7 );
    if ( 7 == math ) op_cmp( src );         // in order of usage for performance
    else if ( 6 == math ) op_ora( src );
    else if ( 4 == math ) op_ana( src );
    else if ( 5 == math ) op_xra( src );
    else if ( 0 == math ) op_add( src );
    else if ( 2 == math ) reg.a = op_sub( src ); // sub doesn't update reg.a
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
        reg.z80_assignYX( result >> 8 );
    }

    reg.SetH( (uint16_t) ( result & 0xffff ) );
} //op_dad

not_inlined void op_daa()
{
    if ( reg.fZ80Mode ) // this BCD add logic pulled from Sean Young's doc
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
        reg.z80_assignYX( reg.a );
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
} //op_daa

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
    x80_hard_exit( "bugbug: not-implemented z80 instruction: %#x, next byte is %#x\n", op, op2 );
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
    // has from the value resulting from the bit operation. But only if the source is a register.

    if ( vs_register == vs )
        reg.z80_assignYX( val );
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
    reg.z80_assignYX( x );
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
    reg.z80_assignYX( x );
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
    reg.z80_assignYX( x );
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
    reg.z80_assignYX( x );
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
    z80_set_sign_zero_16( res16 );

    // if not ( ( one of lhs and com_rhs are negative ) and ( one of lhs and result are negative ) )

    reg.fParityEven_Overflow = ! ( ( lhs ^ com_rhs ) & 0x8000 ) && ( ( lhs ^ res16 ) & 0x8000 );
    reg.fWasSubtract = true;
    if ( borrow )
        rhs++;
    reg.fAuxCarry = ( ( rhs & 0xfff ) > ( lhs & 0xfff ) );
    reg.z80_assignYX( res16 >> 8 );

    return res16;
} //z80_op_sub_16

uint16_t z80_op_add_16( uint16_t a, uint16_t b )
{
    uint32_t resultAux = ( (uint32_t) ( a & 0xfff ) + (uint32_t) ( b & 0xfff ) );
    reg.fAuxCarry = ( 0 != ( resultAux & 0xfffff000 ) );
    reg.fWasSubtract = false;

    uint32_t result = ( (uint32_t) a + (uint32_t) b );
    reg.fCarry = ( 0 != ( result & 0xffff0000 ) );
    reg.z80_assignYX( result >> 8 );

    return result;
} //z80_op_add_16

uint16_t z80_op_adc_16( uint16_t l, uint16_t r )
{
    uint16_t carryOut, result, resultAux;

    if ( reg.fCarry )
    {
        carryOut = ( l >= ( 0xffff - r ) );
        result = r + l + 1;
        resultAux = ( 0xfff & r ) + ( 0xfff & l ) + 1;
    }
    else
    {
        carryOut = ( l > ( 0xffff - r ) );
        result = r + l;
        resultAux = ( 0xfff & r ) + ( 0xfff & l );
    }

    reg.z80_assignYX( result >> 8 );
    reg.fAuxCarry = ( 0 != ( 0xf000 & resultAux ) );
    uint16_t carryIns = result ^ l ^ r;
    z80_set_sign_zero_16( result );
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
    reg.z80_assignYX( val );
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
    reg.z80_assignYX( val );
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
    reg.z80_assignYX( val );
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
    reg.z80_assignYX( val );
    *pval = val;
} //z80_op_srl

uint64_t z80_emulate( uint8_t op )    // this is just for instructions that aren't shared with 8080
{
    uint16_t opaddress = reg.pc - 1;
    uint8_t op2 = memory[ reg.pc ];
    uint8_t op3 = memory[ reg.pc + 1 ];
    uint8_t op4 = memory[ reg.pc + 2 ];
    int op3int = (int) (int8_t) op3;
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
                reg.pc = opaddress + 2 + (int16_t) (int8_t) offset;
                cycles = 13;
            }
            else
                cycles = 8;
        }
        else if ( 0x18 == op ) // jr n
        {
            uint8_t offset = pcbyte();
            reg.pc = opaddress + 2 + (int16_t) (int8_t) offset;
            cycles = 12;
        }
        else if ( 0x20 == op ) // jr nz, n
        {
            uint8_t offset = pcbyte();
            if ( !reg.fZero )
            {
                reg.pc = opaddress + 2 + (int16_t) (int8_t) offset;
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
                reg.pc = opaddress + 2 + (int16_t) (int8_t) offset;
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
                reg.pc = opaddress + 2 + (int16_t) (int8_t) offset;
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
                reg.pc = opaddress + 2 + (int16_t) (int8_t) offset;
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
        reg.r++;
        pcbyte(); // consume op2: the dd or fd

        if ( 0x21 == op2 )  // ld ix/iy word
            * reg.z80_indexAddress( op ) = pcword();
        else if ( 0x22 == op2 ) // ld (address), ix/iy
        {
            cycles = 20;
            uint16_t address = pcword();
            setmword( address, reg.z80_getIndex( op ) );
        }
        else if ( 0x23 == op2 ) // inc ix/iy
        {
            cycles = 10;
            * reg.z80_indexAddress( op ) = reg.z80_getIndex( op ) + 1; // no flags are affected
        }
        else if ( 0x26 == op2 ) // ld ix/iy h. not documented
            * reg.z80_getIndexByteAddress( op, 0 ) = pcbyte();
        else if ( 0x2a == op2 )  // ld ix, (address)
        {
            cycles = 20;
            uint16_t address = pcword();
            reg.z80_setIndex( op, mword( address ) );
        }
        else if ( 0x2b == op2 ) // dec ix/iy
        {
            cycles = 10;
            * reg.z80_indexAddress( op ) = reg.z80_getIndex( op ) - 1; // no flags are affected
        }
        else if ( 0x2e == op2 ) // ld ix/iy l. not documented
        {
            cycles = 20; // guess
            * reg.z80_getIndexByteAddress( op, 1 ) = pcbyte();
        }
        else if ( 0x34 == op2 ) // inc (i + index)
        {
            cycles = 23;
            uint16_t i = reg.z80_getIndex( op ) + (int16_t) (int8_t) pcbyte();
            uint8_t x = memory[ i ];
            memory[i] = op_inc( x );
        }
        else if ( 0x35 == op2 ) // dec (i + index)
        {
            cycles = 23;
            uint16_t i = reg.z80_getIndex( op ) + (int16_t) (int8_t) pcbyte();
            uint8_t x = memory[ i ];
            memory[ i ] = op_dec( x );
        }
        else if ( 0x36 == op2 )  // ld (ix/iy + index), immediate byte
        {
            cycles = 19;
            uint16_t i = reg.z80_getIndex( op ) + (int16_t) (int8_t) pcbyte();
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
                          ( 4 == fromval ) ? reg.z80_getIndexByte( op, 0 ) :
                          ( 5 == fromval ) ? reg.z80_getIndexByte( op, 1 ) :
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
                *reg.z80_getIndexByteAddress( op, 0 ) = tmp;
            else if ( op2 <= 0x6f )
                *reg.z80_getIndexByteAddress( op, 1 ) = tmp;
            else
                reg.a = tmp;
        }
        else if ( 0x46 == ( op2 & 0x47 ) ) // ld r, (i + #)
        {
            cycles = 19;
            uint8_t index = pcbyte();
            uint16_t address = reg.z80_getIndex( op ) + op3int;
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
            uint16_t i = reg.z80_getIndex( op );
            i += op3int;
            memory[ i ] = val;
        }
        else if ( 0x80 == ( op2 & 0xc2 ) ) // math on il and ih with a. 84/85/8c/8d/94/95/a4/a5/b4/b5/bc/bd
        {
            uint8_t value = reg.z80_getIndexByte( op, op2 & 1 );
            op_math( op2, value );
        }
        else if ( 0x86 == ( op2 & 0xc7 ) ) // math on [ ix/iy + index ]
        {
            cycles = 19;
            uint16_t x = reg.z80_getIndex( op );
            x += (int16_t) (int8_t) pcbyte();
            op_math( op2, memory[ x ] );
        }
        else if ( 0x24 == ( op2 & 0xf6 ) ) // inc/dec ixh, ixl, iyh, iyl
        {
            uint8_t *pval = reg.z80_getIndexByteAddress( op, ( op2 >> 3 ) & 1 );
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

            uint16_t oldval = reg.z80_getIndex( op );
            uint16_t newval = z80_op_add_16( oldval, rpval );
            reg.z80_setIndex( op, newval );
        }
        else if ( 0xcb == op2 ) // bit operations
        {
            reg.r++;
            if ( 0x26 == op4 || 0x2e == op4 || 0x3e == op4 ) // sla, sra, srl [ix/iy + offset]
            {
                uint8_t offset = pcbyte(); // the op3
                pcbyte(); // the op4
                uint16_t index = reg.z80_getIndex( op );
                index += (int16_t) (int8_t) offset;
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
                uint16_t index = reg.z80_getIndex( op );
                index += (int16_t) (int8_t) offset;
    
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
                uint16_t offset = reg.z80_getIndex( op ) + (int16_t) (int8_t) index;
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
            reg.z80_setIndex( op, val );
        }
        else if ( 0xe5 == op2 )  // push ix/iy
        {
            cycles = 15;
            uint16_t val = reg.z80_getIndex( op );
            pushword( val );
        }
        else if ( 0xe9 == op2 ) // jp (ix/iy) // the Z80 name makes it look indirect. It's not.
        {
            cycles = 8;
            reg.pc = reg.z80_getIndex( op );
        }
        else if ( 0xf9 == op2 ) // ld sp, ix/iy
        {
            cycles = 10;
            reg.sp = reg.z80_getIndex( op );
        }
        else
            z80_ni( op, op2 );
    }
    else if ( 0xed == op ) // 16-bit load/store and i/o operations
    {
        reg.r++;
        pcbyte();  // consume op2

        if ( 0x3 == ( op2 & 0xf ) ) // ld (mw), rp AKA ld (nn), dd
        {
            cycles = 20;
            uint16_t * prp = reg.rpAddressFromOp( op2 );
            setmword( pcword(), *prp );
        }
        else if ( 0xb == ( op2 & 0xf ) )      // ld rp, (nn) AKA ld dd, (nn)
        {
            cycles = 20;
            uint16_t * prp = reg.rpAddressFromOp( op2 );
            *prp = mword( pcword() );
        }
        else if ( 0x44 == op2 ) // neg
        {
            cycles = 8;
            uint8_t prior = reg.a;
            reg.a = 0 - reg.a;
            set_sign_zero( reg.a );
            reg.fParityEven_Overflow = ( 0x80 == prior );
            reg.fWasSubtract = true;
            reg.fCarry = ( 0 != prior );
            reg.fAuxCarry = ( 0 != ( prior & 0xf ) );
            reg.z80_assignYX( reg.a );
        }
        else if ( 0x47 == op2 ) // ld i,a
            reg.i = reg.a;
        else if ( 0x4f == op2 ) // ld r,a
            reg.r = reg.a;
        else if ( 0x57 == op2 ) // ld a,i
        {
            reg.a = reg.i;
            set_sign_zero( reg.a );
            reg.fParityEven_Overflow = false; // no iff2
            reg.fWasSubtract = false;
            reg.fAuxCarry = false;
            reg.z80_assignYX( reg.a );
        }
        else if ( 0x5f == op2 ) // ld a,r
        {
            reg.a = ( 0x7f & reg.r ); // the high bit is always 0 on Z80
            set_sign_zero( reg.a );
            reg.fParityEven_Overflow = false; // no iff2
            reg.fWasSubtract = false;
            reg.fAuxCarry = false;
            reg.z80_assignYX( reg.a );
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
            reg.z80_assignYX( reg.a );
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
            reg.z80_assignYX( reg.a );
        }
        else if ( 0x4a == ( op2 & 0xcf ) ) // adc hl, rp
        {
            uint16_t result = z80_op_adc_16( reg.H(), * reg.rpAddressFromOp( op2 ) );
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
        else if ( 0xb0 == op2 ) // ldir
        {
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
        else if ( 0x02 == ( op2 & 0x8f ) ) // sbc hl, rp AKA sbc hl, ss
        {
            cycles = 15;
            uint16_t val = * reg.rpAddressFromOp( op2 );
            reg.SetH( z80_op_sub_16( reg.H(), val, reg.fCarry ) );
        }
        else if ( 0x04 == ( op2 & 0x8f ) ) // adc hl, rp AKA sbc hl, ss
        {
            cycles = 15;
            uint16_t val = * reg.rpAddressFromOp( op2 );
            reg.SetH(  z80_op_adc_16( reg.H(), val ) );
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
} //z80_renderByteReg

void z80_render( char * ac, uint8_t op, uint16_t address )
{
    uint8_t op2 = memory[ address + 1 ];
    uint8_t op3 = memory[ address + 2 ];
    uint8_t op4 = memory[ address + 3 ];
    uint16_t op34 = (uint16_t) op3 + ( (uint16_t) op4 << 8 );
    int16_t op2int = (int16_t) (int8_t) op2;
    int16_t op3int = (int16_t) (int8_t) op3;
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
            int offset = (int) (int8_t) op3;
            sprintf( ac, "%s %s, (%s%s%d)", z80_rotate_strings[ rot ], reg_strings[ op4 & 0x7 ], i, offset >= 0 ? "+" : "", offset );
        }
        else if ( 0xcb == op2 && ( ( ( op4 & 0xf ) == 0xe ) || ( ( op4 & 0xf ) == 0x6 ) ) ) // bit operations on indexed memory
        {
            uint8_t index = op3;
            int32_t index32 = (int32_t) (int8_t) index;
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
        strcpy( ac, z80_instructions[ op ] );
        if ( '*' == ac[ 0 ] )
        {
            z80_render( ac, op, address );
            renderData = false;
        }
    }
    else
        strcpy( ac, i8080_instructions[ op ] );

    if ( renderData )
    {
        char * p;
        if ( ( p = strstr( ac, "d16" ) ) )
            replace_with_num( p, "d16", mword( address + 1 ), 16 );
        else if ( ( p = strstr( ac, "a16" ) ) )
            replace_with_num( p, "a16", mword( address + 1 ), 16 );
        else if ( ( p = strstr( ac, "d8" ) ) )
            replace_with_num( p, "d8", memory[ address + 1 ], 8 );
    }

    return ac;
} //x80_render_operation

not_inlined void x80_trace_state()
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
    const acycles_t & acycles = reg.fZ80Mode ? z80_cycles : i8080_cycles; // ms C++ will opportunistically read *both* in the loop below otherwise

    while ( cycles < maxcycles )        // 2% of runtime checking if we're done
    {
        if ( g_traceInstructions )      // 2.5% of runtime is checking this flag and branching
            x80_trace_state();

        uint8_t op = memory[ reg.pc ];  // 1% of runtime
        reg.pc++;                       // 4% of runtime including npad to make _restart_op aligned

_restart_op:
        cycles += acycles[ op ];

        switch ( op )                   // 47% of runtime is completing cycle addition & setting up for the jump table jump; 10 instructions
        {
            case 0x00: break; // nop
            case 0x01: case 0x11: case 0x21: case 0x31: // lxi rp, d16
            {
                * reg.rpAddress( op >> 4 ) = pcword();
                break;
            }
            case 0x02: memory[ reg.B() ] = reg.a; break; // stax b
            case 0x03: case 0x13: case 0x23: case 0x33: // inx. no status flag updates
            {
                uint16_t * pdst = reg.rpAddressFromOp( op );
                *pdst = 1 + *pdst;
                break;
            }
            case 0x04: case 0x14: case 0x24: case 0x34: case 0x0c: case 0x1c: case 0x2c: case 0x3c: // inr r. does not set carry
            {
                uint8_t * pdst = dst_address( op );
                *pdst = op_inc( *pdst );
                break;
            }
            case 0x05: case 0x15: case 0x25: case 0x35: case 0x0d: case 0x1d: case 0x2d: case 0x3d: // dcr r. does not set carry
            {
                uint8_t * pdst = dst_address( op );
                *pdst = op_dec( * pdst );          // 5% of runtime
                break;
            }
            case 0x06: case 0x16: case 0x26: case 0x36: case 0x0e: case 0x1e: case 0x2e: case 0x3e: // mvi r, d8
            {
                * dst_address( op ) = pcbyte();
                break;
            }
            case 0x07: // rlc
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
                    reg.z80_assignYX( reg.a );
                }
                break;
            }
            case 0x09: case 0x19: case 0x29: case 0x39: // dad
            {
                op_dad( * reg.rpAddressFromOp( op ) );
                break;
            }
            case 0x0a: reg.a = memory[ reg.B() ]; break; // ldax b
            case 0x0b: case 0x1b: case 0x2b: case 0x3b: // dcx. no status flag updates
            {
                uint16_t * pdst = reg.rpAddressFromOp( op );
                *pdst = *pdst - 1;
                break;
            }
            case 0x0f: // rrc
            {
                reg.fCarry = ( 0 != ( reg.a & 1 ) );
                reg.a >>= 1;
                if ( reg.fCarry )
                    reg.a |= 0x80;
                if ( reg.fZ80Mode )
                {
                    reg.clearHN();
                    reg.z80_assignYX( reg.a );
                }
                break;
            }
            case 0x12: memory[ reg.D() ] = reg.a; break; // stax d
            case 0x17: // ral
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
                    reg.z80_assignYX( reg.a );
                }
                break;
            }
            case 0x1a: reg.a = memory[ reg.D() ]; break; // ldax d
            case 0x1f: // rar
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
                    reg.z80_assignYX( reg.a );
                }
                break;
            }
            case 0x22: { uint16_t offset = pcword(); setmword( offset, reg.H() ); break; } // shld
            case 0x27: { op_daa(); break; } // daa
            case 0x2a: { reg.SetH( mword( pcword() ) ); break; } // lhld
            case 0x2f: // cma
            {
                reg.a = ~reg.a;
                if ( reg.fZ80Mode )
                {
                    reg.fAuxCarry = true;
                    reg.fWasSubtract = true;
                    reg.z80_assignYX( reg.a );
                }
                break;
            }
            case 0x32: memory[ pcword() ] = reg.a; break; // sta a16
            case 0x37: // stc
            {
                reg.fCarry = 1;
                if ( reg.fZ80Mode )
                {
                    reg.clearHN();
                    reg.z80_assignYX( reg.a );
                }
                break;
            }
            case 0x3a: { reg.a = memory[ pcword() ]; break; } // lda a16
            case 0x3f: // cmc
            {
                reg.fCarry = !reg.fCarry;
                if ( reg.fZ80Mode )
                {
                    reg.fWasSubtract = false;
                    reg.fAuxCarry = !reg.fCarry;
                    reg.z80_assignYX( reg.a );
                }
                break;
            }
            case 0x40: case 0x41: case 0x42: case 0x43: case 0x44: case 0x45: case 0x46: case 0x47: // mov r, r
            case 0x48: case 0x49: case 0x4a: case 0x4b: case 0x4c: case 0x4d: case 0x4e: case 0x4f:
            case 0x50: case 0x51: case 0x52: case 0x53: case 0x54: case 0x55: case 0x56: case 0x57:
            case 0x58: case 0x59: case 0x5a: case 0x5b: case 0x5c: case 0x5d: case 0x5e: case 0x5f:
            case 0x60: case 0x61: case 0x62: case 0x63:            case 0x65: case 0x66: case 0x67: // 0x64 is hook
            case 0x68: case 0x69: case 0x6a: case 0x6b: case 0x6c: case 0x6d: case 0x6e: case 0x6f:
            case 0x70: case 0x71: case 0x72: case 0x73: case 0x74: case 0x75:            case 0x77: // 0x76 is hlt
            case 0x78: case 0x79: case 0x7a: case 0x7b: case 0x7c: case 0x7d: case 0x7e: case 0x7f:
            {
                *dst_address( op ) = src_value( op ); // 5% of runtime
                break;
            }
            case 0x64: { op = x80_invoke_hook(); goto _restart_op; } // hook
            case 0x76: { x80_invoke_halt(); goto _all_done; } // hlt
            case 0x80: case 0x81: case 0x82: case 0x83: case 0x84: case 0x85: case 0x86: case 0x87: // math
            case 0x88: case 0x89: case 0x8a: case 0x8b: case 0x8c: case 0x8d: case 0x8e: case 0x8f:
            case 0x90: case 0x91: case 0x92: case 0x93: case 0x94: case 0x95: case 0x96: case 0x97:
            case 0x98: case 0x99: case 0x9a: case 0x9b: case 0x9c: case 0x9d: case 0x9e: case 0x9f:
            case 0xa0: case 0xa1: case 0xa2: case 0xa3: case 0xa4: case 0xa5: case 0xa6: case 0xa7:
            case 0xa8: case 0xa9: case 0xaa: case 0xab: case 0xac: case 0xad: case 0xae: case 0xaf:
            case 0xb0: case 0xb1: case 0xb2: case 0xb3: case 0xb4: case 0xb5: case 0xb6: case 0xb7:
            case 0xb8: case 0xb9: case 0xba: case 0xbb: case 0xbc: case 0xbd: case 0xbe: case 0xbf:
            {
                op_math( op, src_value( op ) );  // 4% of runtime
                break;
            }
            case 0xc0: case 0xd0: case 0xe0: case 0xf0: case 0xc8: case 0xd8: case 0xe8: case 0xf8: // conditional return
            {
                if ( check_conditional( op ) )
                    reg.pc = popword();
                else
                    cycles -= cyclesnt;
                break;
            }
            case 0xc1: case 0xd1: case 0xe1: case 0xf1: // pop
            {
                if ( 0xf1 == op )
                    reg.SetPSW( popword() );
                else
                    * reg.rpAddressFromOp( op ) = popword();
                break;
            }
            case 0xc2: case 0xd2: case 0xe2: case 0xf2: case 0xca: case 0xda: case 0xea: case 0xfa: // conditional jmp
            {
                uint16_t address = pcword(); // must be consumed regardless of whether jump is taken
                if ( check_conditional( op ) )
                    reg.pc = address;
                else
                    cycles -= cyclesnt;
                break;
            }
            case 0xc3: { reg.pc = pcword(); break; } // jmp a16
            case 0xc4: case 0xd4: case 0xe4: case 0xf4: case 0xcc: case 0xdc: case 0xec: case 0xfc: // conditional call
            {
                uint16_t address = pcword(); // must be consumed regardless of whether call is taken
                if ( check_conditional( op ) )
                {
                    pushword( reg.pc );
                    reg.pc = address;
                }
                else
                    cycles -= cyclesnt;
                break;
            }
            case 0xc5: case 0xd5: case 0xe5: case 0xf5: // push
            {
                pushword( ( 0xf5 == op ) ? reg.PSW() : * reg.rpAddressFromOp( op ) );
                break;
            }
            case 0xc6: case 0xd6: case 0xe6: case 0xf6: case 0xce: case 0xde: case 0xee: case 0xfe: // adi aci sui sbi ani xri ori cpi
            {
                op_math( op, pcbyte() );         // 4% of runtime
                break;
            }
            case 0xc7: case 0xd7: case 0xe7: case 0xf7: case 0xcf: case 0xdf: case 0xef: case 0xff: // rst
            {
                // bits 5..3 are exp, which form an address 0000000000exp000 that is called.
                // rst is generally invoked by hardware interrupts, which supply one instruction rst.
            
                pushword( reg.pc );
                reg.pc = 0x38 & (uint16_t) op;
                break;
            }
            case 0xc9: { reg.pc = popword(); break; } // ret
            case 0xcd: { uint16_t v = pcword(); pushword( reg.pc ); reg.pc = v; break; } // call a16
            case 0xd3: { x80_invoke_out( pcbyte() ); break; } // out d8
            case 0xdb: { x80_invoke_in( pcbyte() ); break; } // in d8
            case 0xe3: { uint16_t t = reg.H(); reg.SetH( mword( reg.sp ) ); setmword( reg.sp, t ); break; } // xthl
            case 0xe9: { reg.pc = reg.H(); break; } // pchl
            case 0xeb: { uint16_t t = reg.H(); reg.SetH( reg.D() ); reg.SetD( t ); break; } // xchg
            case 0xf3: { reg.fINTE = false; break; } // di
            case 0xf9: { reg.sp = reg.H(); break; } // sphl
            case 0xfb: { reg.fINTE = true; break; } // ei
            default:
            {
                if ( reg.fZ80Mode )
                    cycles += z80_emulate( op );
                else
                    x80_hard_exit( "Error: 8080 undocumented instruction: %#x, next byte %#x\n", op, memory[ reg.pc + 1 ] );
            } //default
        } //switch

        reg.r++; // increment R even for 8080 emulation to avoid 'if'. mask high bit on read. 4.61% of runtime; ouch.
    } //while
_all_done:
    return cycles;
} //x80_emulate
