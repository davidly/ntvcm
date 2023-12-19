#pragma once

#include <assert.h>

// i8080 and Z80 emulator

#define OPCODE_HOOK 0x64  // mov h, h. When executed, x80_invoke_hook is called
#define OPCODE_HLT  0x76  // for ending execution. When executed, x80_invoke_halt is called
#define OPCODE_NOP  0x00
#define OPCODE_JMP  0xC3  // jmp nn
#define OPCODE_RET  0xC9


const size_t reg_offsets[8] = { 1, 0, 3, 2, 5, 4, 8, 9 }; //bcdehlfa

struct registers
{
    // the memory layout of these registers is assumed by the code below

    uint8_t c, b;
    uint8_t e, d;
    uint8_t l, h;
    uint16_t sp;
    uint8_t f, a;
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

    uint8_t materializeFlags()
    {
        // flags bits 7..0 (where they differ, 8080/Z80):
        // sign, zero, mustbe0, auxcarry/halfcarry, mustbe0, parityeven/overflow, mustbe1/subtract, carry

        f = fZ80Mode ? ( fWasSubtract ? 2 : 0 ) : 2;
        if ( fCarry ) f |= 1;
        if ( fParityEven_Overflow ) f |= 4;
        if ( fAuxCarry ) f |= 0x10;
        if ( fZero ) f |= 0x40;
        if ( fSign ) f |= 0x80;

        if ( fZ80Mode )    // these flags are undocumented but must work to run emulation tests
        {
            if ( fY ) f |= 0x20;
            if ( fX ) f |= 8;
        }

        return f;
    } //materializeFlags

    uint16_t PSW()
    {
        materializeFlags();
        return * ( (uint16_t *) & f );
    } //PSW

    uint16_t B() const { return * ( (uint16_t *) & c ); }
    uint16_t D() const { return * ( (uint16_t *) & e ); }
    uint16_t H() const { return * ( (uint16_t *) & l ); }

    void z80_increment_r() { /* reg.r++; */ } // 4.6% of runtime when the increment is enabled

    void unmaterializeFlags()
    {
        if ( fZ80Mode )
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

        if ( fZ80Mode )
        {
            fY = ( 0 != ( f & 0x20 ) );
            fX = ( 0 != ( f & 8 ) );
        }
    } //unmaterializeFlags

    void SetPSW( uint16_t x )
    {
        * ( (uint16_t *) & f ) = x;
        unmaterializeFlags();
    } //SetPSW

    void SetB( uint16_t x ) { * ( (uint16_t *) & c ) = x; }
    void SetD( uint16_t x ) { * ( (uint16_t *) & e ) = x; }
    void SetH( uint16_t x ) { * ( (uint16_t *) & l ) = x; }

    uint16_t * rpAddress( uint8_t rp )
    {
        // rp == register pair: 0 B, 1 D, 2 H, 3 SP
        assert( rp <= 3 );
        return ( ( (uint16_t *) this ) + rp );
    } //rpAddress

    uint16_t * rpAddressFromOp( uint8_t op )
    {
        return (uint16_t *) ( ( (uint8_t *) this ) + ( ( op >> 3 ) & 6 ) );
    } //rpAddressFromOp

    uint16_t * rpAddressFromLowOp( uint8_t op )
    {
        assert( ! ( op & 0x8080 ) ); // save masking if the high bits on each nibble are 0
        return (uint16_t *) ( ( (uint8_t *) this ) + ( ( op >> 3 ) ) );
    } //rpAddressFromLowOp

    uint8_t * regOffset( uint8_t rm )
    {
        assert( 6 != rm ); // that's a memory access, not a register. f can't be used directly.
        return ( ( ( uint8_t *) this ) + reg_offsets[ rm ] );
    } //regOffset

    void clearHN()
    {
        fAuxCarry = false;
        fWasSubtract = false;
    } //clearHN

    bool getFlag( uint8_t x )
    {
        assert( x <= 3 );
        return * ( ( & fZero ) + x );
    } //getFlag

    const char * renderFlags()
    {
        static char ac[10] = {0};
        size_t next = 0;
        ac[ next++ ] = fSign ? 'S' : 's';
        ac[ next++ ] = fZero ? 'Z' : 'z';
        if ( fZ80Mode )
            ac[ next++ ] = fY ? 'Y' : 'y';

        if ( fZ80Mode )
            ac[ next++ ] = fAuxCarry ? 'H' : 'h'; // half-carry; almost same meaning as aux-carry
        else
            ac[ next++ ] = fAuxCarry ? 'A' : 'a';

        if ( fZ80Mode )
            ac[ next++ ] = fX ? 'X' : 'x';

        if ( fZ80Mode )
            ac[ next++ ] = fParityEven_Overflow ? 'V' : 'v';
        else
            ac[ next++ ] = fParityEven_Overflow ? 'P' : 'p';

        if ( fZ80Mode )
            ac[ next++ ] = fWasSubtract ? 'N' : 'n';

        ac[ next++ ] = fCarry ? 'C' : 'c';
        ac[ next ] = 0;
    
        return ac;
    } //renderFlags

    uint16_t * z80_indexAddress( uint8_t op )
    {
        assert( 0xdd == op || 0xfd == op );

        if ( 0xdd == op )
            return & ix;
        return & iy;
    } //z80_indexAddress

    void z80_setIndex( uint8_t op, uint16_t val )
    {
        * z80_indexAddress( op ) = val;
    } //z80_setIndex

    uint16_t z80_getIndex( uint8_t op )
    {
        assert( 0xdd == op || 0xfd == op );

        if ( 0xdd == op )
            return ix;
        return iy;
    } //z80_getIndex

    uint8_t z80_getIndexByte( uint8_t op, uint8_t hl )
    {
        assert( 0xdd == op || 0xfd == op );
        assert( 0 == hl || 1 == hl );

        uint16_t result16 = z80_getIndex( op );

        if ( 0 == hl )
            return ( ( result16 >> 8 ) & 0xff );
        return ( result16 & 0xff );
    } //z80_getIndexByte

    uint8_t * z80_getIndexByteAddress( uint8_t op, uint8_t hl )
    {
        assert( 0xdd == op || 0xfd == op );
        assert( 0 == hl || 1 == hl );

        uint8_t * pval;
        if ( 0xdd == op )
            pval = (uint8_t *) & ix;
        else
            pval = (uint8_t *) & iy;

        if ( 0 == hl )
            pval++;

        return pval;
    } //z80_getIndexByteAddress

    void z80_assignYX( uint8_t val )
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

// emulator API

extern uint64_t x80_emulate( uint64_t maxcycles );             // execute up to about maxcycles
extern void x80_trace_instructions( bool trace );              // enable/disable tracing each instruction
extern void x80_trace_state( void );                           // trace the registers
extern const char * x80_render_operation( uint16_t address );  // return a string with the disassembled instruction at address
extern void x80_hard_exit( const char * pcerror, uint8_t arg1, uint8_t arg2 ); // called on failures to exit the app
