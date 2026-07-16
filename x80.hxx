#pragma once

// i8080 and Z80 emulator

#define OPCODE_HOOK 0x64  // mov h, h. When executed, x80_invoke_hook is called
#define OPCODE_HLT  0x76  // for ending execution. When executed, x80_invoke_halt is called
#define OPCODE_NOP  0x00
#define OPCODE_JMP  0xC3  // jmp nn
#define OPCODE_RET  0xC9

#ifdef TARGET_BIG_ENDIAN
const size_t reg_offsets[8] = { 0, 1, 2, 3, 4, 5, 9, 8 }; //bcdehlfa
#else
const size_t reg_offsets[8] = { 1, 0, 3, 2, 5, 4, 8, 9 }; //bcdehlfa
#endif

struct registers
{
    // the memory layout of these registers is assumed by the code below

#ifdef TARGET_BIG_ENDIAN
    uint8_t b, c;
    uint8_t d, e;
    uint8_t h, l;
    uint16_t sp;
    uint8_t a, f;
#else
    uint8_t c, b;
    uint8_t e, d;
    uint8_t l, h;
    uint16_t sp;
    uint8_t f, a;
#endif
    uint16_t pc;

    // Z80-specific. p == prime registers swapped via exchange instructions

    uint8_t cp, bp;
    uint8_t ep, dp;
    uint8_t lp, hp;
    uint8_t fp, ap;

    uint16_t ix;
    uint16_t iy;
    uint8_t r;    // refresh
    uint8_t i;    // interrupt page

    // these first four bool flags must remain in this order for getFlag() to work

    bool fZero;                // Z zero
    bool fCarry;               // C carry
    bool fParityEven_Overflow; // P/V. (Overflow) on the Z80.
    bool fSign;                // S negative; < 0
    bool fAuxCarry;            // A/H. aka half-carry for the Z80
    bool fWasSubtract;         // N. Z80-specific. Flag for BCD math (DAA). true for subtract, false for add.
    bool fY;                   // defined to be 0, but physical Z80s and 8085s sometimes set to 1
    bool fX;                   // defined to be 0, but physical Z80s and 8085s sometimes set to 1

    bool fZ80Mode;             // Not a cpu flag. true if emulating Z80, false if emulating i8080
    bool fINTE;                // Not a cpu flag. true if hardware interrupts are enabled. set/reset with ei and di

    template <bool Z80Mode> uint8_t materializeFlags()
    {
        // flags bits 7..0 (where they differ, 8080/Z80):
        // sign, zero, mustbe0, auxcarry/halfcarry, mustbe0, parityeven/overflow, mustbe1/subtract, carry

        // branchless: every fXxx member is a bool (0 or 1), so shifting it into
        // place and OR-ing together avoids a chain of conditional branches here.
        // Each is cast to int first since msvc warns ('unsafe mix of type "int"
        // and type "bool"') about shifting/OR-ing a bool directly.

        f = ( Z80Mode ? ( (int) fWasSubtract << 1 ) : 2 ) |
            (int) fCarry | ( (int) fParityEven_Overflow << 2 ) | ( (int) fAuxCarry << 4 ) |
            ( (int) fZero << 6 ) | ( (int) fSign << 7 );

        if ( Z80Mode )    // these flags are undocumented but must work to run emulation tests
            f |= ( (int) fY << 5 ) | ( (int) fX << 3 );

        return f;
    } //materializeFlags

    template <bool Z80Mode> uint16_t PSW()
    {
        materializeFlags<Z80Mode>();
#ifdef TARGET_BIG_ENDIAN
        return * ( (uint16_t *) & a );
#else
        return * ( (uint16_t *) & f );
#endif
    } //PSW

    template <bool Z80Mode> void SetPSW( uint16_t x )
    {
#ifdef TARGET_BIG_ENDIAN
        * ( (uint16_t *) & a ) = x;
#else
        * ( (uint16_t *) & f ) = x;
#endif
        unmaterializeFlags<Z80Mode>();
    } //SetPSW

#ifdef TARGET_BIG_ENDIAN
    uint16_t B() const { return * ( (uint16_t *) & b ); }
    uint16_t D() const { return * ( (uint16_t *) & d ); }
    uint16_t H() const { return * ( (uint16_t *) & h ); }

    void SetB( uint16_t x ) { * ( (uint16_t *) & b ) = x; }
    void SetD( uint16_t x ) { * ( (uint16_t *) & d ) = x; }
    void SetH( uint16_t x ) { * ( (uint16_t *) & h ) = x; }
#else
    uint16_t B() const { return * ( (uint16_t *) & c ); }
    uint16_t D() const { return * ( (uint16_t *) & e ); }
    uint16_t H() const { return * ( (uint16_t *) & l ); }

    void SetB( uint16_t x ) { * ( (uint16_t *) & c ) = x; }
    void SetD( uint16_t x ) { * ( (uint16_t *) & e ) = x; }
    void SetH( uint16_t x ) { * ( (uint16_t *) & l ) = x; }
#endif

    void z80_increment_r() { /* reg.r++; */ } // 4.6% of runtime when the increment is enabled

    template <bool Z80Mode> void unmaterializeFlags()
    {
        if ( Z80Mode )
            fWasSubtract = ( 0 != ( f & 2 ) );
        else
        {
            f &= 0xd7; // set 0 bits
            f |= 0x2; // always 1 on 8080
        }

        fCarry = ( 0 != ( f & 1 ) );
        fParityEven_Overflow = ( 0 != ( f & 4 ) );
        fAuxCarry = ( 0 != ( f & 0x10 ) );
        fZero = ( 0 != ( f & 0x40 ) );
        fSign = ( 0 != ( f & 0x80 ) );

        if ( Z80Mode )
        {
            fY = ( 0 != ( f & 0x20 ) );
            fX = ( 0 != ( f & 8 ) );
        }
    } //unmaterializeFlags

    inline uint16_t * rpAddress( uint8_t rp )
    {
        // rp == register pair: 0 B, 1 D, 2 H, 3 SP
        assert( rp <= 3 );
        return ( ( (uint16_t *) this ) + rp );
    } //rpAddress

    inline uint16_t * rpAddressFromOp( uint8_t op )
    {
        return (uint16_t *) ( ( (uint8_t *) this ) + ( ( op >> 3 ) & 6 ) );
    } //rpAddressFromOp

    inline uint16_t * rpAddressFromLowOp( uint8_t op )
    {
        assert( ! ( op & 0x88 ) ); // save masking with &6 if the high bits on each nibble are 0
        return (uint16_t *) ( ( (uint8_t *) this ) + ( op >> 3 ) );
    } //rpAddressFromLowOp

    inline uint8_t * regOffset( uint8_t rm )
    {
        assert( 6 != rm ); // that's a memory access, not a register. f can't be used directly.
        return ( ( ( uint8_t *) this ) + reg_offsets[ rm ] );
    } //regOffset

    inline void clearHN()
    {
        fAuxCarry = false;
        fWasSubtract = false;
    } //clearHN

    inline bool getFlag( uint8_t x )
    {
        assert( x <= 3 );
        return * ( ( & fZero ) + x );
    } //getFlag

    template <bool Z80Mode> const char * renderFlags()
    {
        static char ac[10] = {0};
        size_t next = 0;
        ac[ next++ ] = fSign ? 'S' : 's';
        ac[ next++ ] = fZero ? 'Z' : 'z';
        if ( Z80Mode )
            ac[ next++ ] = fY ? 'Y' : 'y';

        if ( Z80Mode )
            ac[ next++ ] = fAuxCarry ? 'H' : 'h'; // half-carry; almost same meaning as aux-carry
        else
            ac[ next++ ] = fAuxCarry ? 'A' : 'a';

        if ( Z80Mode )
            ac[ next++ ] = fX ? 'X' : 'x';

        if ( Z80Mode )
            ac[ next++ ] = fParityEven_Overflow ? 'V' : 'v';
        else
            ac[ next++ ] = fParityEven_Overflow ? 'P' : 'p';

        if ( Z80Mode )
            ac[ next++ ] = fWasSubtract ? 'N' : 'n';

        ac[ next++ ] = fCarry ? 'C' : 'c';
        ac[ next ] = 0;

        return ac;
    } //renderFlags

    // 0xdd (ix) and 0xfd (iy) differ only in bit 5 (0 for dd, 1 for fd). ix/iy
    // are declared adjacent (see the layout comment at the top of this struct),
    // so indexing off &ix picks the right one with no branch, unlike the
    // 0xdd/0xfd if/else this replaces.
    inline uint16_t & z80_indexRef( uint8_t op )
    {
        assert( 0xdd == op || 0xfd == op );
        return ( & ix )[ ( op >> 5 ) & 1 ];
    } //z80_indexRef

    inline void z80_setIndex( uint8_t op, uint16_t val )
    {
        z80_indexRef( op ) = val;
    } //z80_setIndex

    inline uint16_t z80_getIndex( uint8_t op )
    {
        return z80_indexRef( op );
    } //z80_getIndex

    inline uint8_t z80_getIndexByte( uint8_t op, uint8_t hl )
    {
        assert( 0 == hl || 1 == hl );

        uint16_t result16 = z80_getIndex( op );

        if ( 0 == hl )
            return ( ( result16 >> 8 ) & 0xff );
        return ( result16 & 0xff );
    } //z80_getIndexByte

    uint8_t * z80_getIndexByteAddress( uint8_t op, uint8_t hl )
    {
        assert( 0 == hl || 1 == hl );

        uint8_t * pval = (uint8_t *) & z80_indexRef( op );

#ifdef TARGET_BIG_ENDIAN
        if ( 1 == hl )
#else
        if ( 0 == hl )
#endif
            pval++;

        return pval;
    } //z80_getIndexByteAddress

    inline void z80_assignYX( uint8_t val )
    {
        fY = ( 0 != ( val & 0x20 ) );
        fX = ( 0 != ( val & 8 ) );
    } //z80_assignYX

    void powerOn()
    {
        pc = 0;
        r = 1;
        i = 0;
        a = f = b = c = d = e = h = l = 0xff;
        sp = ix = iy = 0xffff;
        ap = fp = bp = cp = dp = ep = hp = lp = 0xff;
        fZero = fCarry = fParityEven_Overflow = fSign = fAuxCarry = fWasSubtract = fY = fX = 0;
        fINTE = false;
    } //powerOn
};

extern uint8_t memory[ 65536 ];
extern registers reg;

// callbacks when instructions are executed

extern void x80_invoke_out( uint8_t x ); // called for the out instruction
extern void x80_invoke_in( uint8_t x );  // called for the in instruction
extern void x80_invoke_halt( void );     // called when the 8080 hlt (on Z80 halt) instruction is executed
extern uint8_t x80_invoke_hook( void );  // called with the OPCODE_HOOK instruction is executed
extern void x80_hard_exit( const char * pcerror, uint8_t arg1, uint8_t arg2 ); // called on failures to exit the app

// emulator API

extern uint32_t x80_emulate( uint16_t maxcycles );             // execute up to about maxcycles
extern void x80_trace_instructions( bool trace );              // enable/disable tracing each instruction
extern void x80_end_emulation();                               // stop the emulation
extern void x80_trace_state( void );                           // trace the registers
extern void x80_profile_enable( bool enable );                 // count executions per PC address
extern const uint64_t * x80_profile_counts( void );            // 65536 per-PC execution counts, or NULL if disabled
extern const char * x80_render_operation( uint16_t address );  // return a string with the disassembled instruction at address
extern uint8_t x80_instruction_length( uint16_t address );      // return the decoded instruction length in bytes

// The gdb/MI debugger backend isn't built for Watcom's real-mode DOS
// target (see x80.cxx) - excluded here too so a stray call from a future
// change fails to compile immediately instead of failing to link.
#ifndef WATCOMDOS

enum x80_debug_stop_reason
{
    x80_debug_stop_none,
    x80_debug_stop_entry,
    x80_debug_stop_breakpoint,
    x80_debug_stop_step,
    x80_debug_stop_pause
};

extern void x80_debug_enable( bool enable );
extern void x80_debug_continue();
extern void x80_debug_step();
extern void x80_debug_pause();
extern bool x80_debug_stopped();
extern x80_debug_stop_reason x80_debug_reason();
extern void x80_debug_set_breakpoint( uint16_t address, bool enable );
extern void x80_debug_clear_breakpoints();

#endif //!WATCOMDOS
