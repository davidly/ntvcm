#pragma once

#include <assert.h>

// i8080 and Z80 emulator

#define OPCODE_HOOK 0x64  // mov h, h. When executed, x80_invoke_hook is called
#define OPCODE_HLT 0x76   // for ending execution. When executed, x80_invoke_halt is called

const uint8_t reg_offsets[8] = { 3, 2, 5, 4, 7, 6, 0, 1 }; //bcdehlfa

struct registers
{
    uint8_t f, a;
    uint8_t c, b;
    uint8_t e, d;
    uint8_t l, h;
    uint16_t sp;
    uint16_t pc;

    // Z80-specific. p == prime registers swapped via exchange instructions

    uint8_t fp, ap;
    uint8_t cp, bp;
    uint8_t ep, dp;
    uint8_t lp, hp;

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
    bool fY;                   // defined to be 0, but physical Z80s and 8085s sometimes set them to 1
    bool fX;                   // defined to be 0, but physical Z80s and 8085s sometimes set them to 1
    bool fZ80Mode;             // true if emulating Z80, false if emulating i8080

    uint8_t materializeFlags()
    {
        // flags bits 7..0 (where they differ, 8080/Z80):
        // sign, zero, mustbe0, auxcarry/halfcarry, mustbe0, parityeven/overflow, mustbe1/subtract, carry

        f = fZ80Mode ? ( fWasSubtract ? 0x02 : 0 ) : 0x02;
        if ( fCarry ) f |= 0x01;
        if ( fParityEven_Overflow ) f |= 0x04;
        if ( fAuxCarry ) f |= 0x10;
        if ( fZero ) f |= 0x40;
        if ( fSign ) f |= 0x80;

        if ( fZ80Mode )    // these flags are undocumented but must work to run emulation tests
        {
            if ( fY ) f |= 0x20;
            if ( fX ) f |= 0x08;
        }

        return f;
    }

    uint16_t PSW()
    {
        materializeFlags();
        return * ( (uint16_t *) & (this->f) );
    }

    uint16_t B() const { return * ( (uint16_t *) & ( this->c ) ); }
    uint16_t D() const { return * ( (uint16_t *) & ( this->e ) ); }
    uint16_t H() const { return * ( (uint16_t *) & ( this->l ) ); }

    void unmaterializeFlags()
    {
        if ( fZ80Mode )
            fWasSubtract = ( 0 != ( f & 0x02 ) );
        else
        {
            this->f &= 0xd7; // set 0 bits
            this->f |= 0x2; // always 1 on 8080
        }

        fCarry = ( 0 != ( f & 0x01 ) );
        fParityEven_Overflow = ( 0 != ( f & 0x04 ) );
        fAuxCarry = ( 0 != ( f & 0x10 ) );
        fZero = ( 0 != ( f & 0x40 ) );
        fSign = ( 0 != ( f & 0x80 ) );

        if ( fZ80Mode )
        {
            fY = ( 0 != ( f & 0x20 ) );
            fX = ( 0 != ( f & 0x08 ) );
        }
    } //unmaterializeFlags

    void SetPSW( uint16_t x )
    {
        * ( (uint16_t *) & ( this->f ) ) = x;
        unmaterializeFlags();
    } //SetPSW

    void SetB( uint16_t x ) { * ( (uint16_t *) & ( this->c ) ) = x; }
    void SetD( uint16_t x ) { * ( (uint16_t *) & ( this->e ) ) = x; }
    void SetH( uint16_t x ) { * ( (uint16_t *) & ( this->l ) ) = x; }

    uint16_t * rpAddress( uint8_t rp )
    {
        // 0 B, 1 D, 2 H, 3 SP

        assert( rp <= 3 );
        return (uint16_t *) ( ( (uint8_t *) this ) + 2 + 2 * rp ); 
    } //rpAddress

    uint16_t * rpAddressFromOp( uint8_t op )
    {
        uint8_t rp = ( op >> 4 ) & 0x3;
        return rpAddress( rp );
    } //rpAddressFromOp

    uint16_t * indexAddress( uint8_t op )
    {
        assert( 0xdd == op || 0xfd == op );

        if ( 0xdd == op )
            return & ( this->ix );
        return & ( this->iy );
    } //indexAddress

    void setIndex( uint8_t op, uint16_t val )
    {
        * indexAddress( op ) = val;
    } //setIndex

    uint16_t getIndex( uint8_t op )
    {
        assert( 0xdd == op || 0xfd == op );

        if ( 0xdd == op )
            return this->ix;
        return this->iy;
    } //getIndex

    uint8_t getIndexByte( uint8_t op, uint8_t hl )
    {
        assert( 0xdd == op || 0xfd == op );
        assert( 0 == hl || 1 == hl );

        uint16_t result16 = getIndex( op );

        if ( 0 == hl )
            return ( ( result16 >> 8 ) & 0xff );
        return ( result16 & 0xff );
    } //getIndexByte

    uint8_t * getIndexByteAddress( uint8_t op, uint8_t hl )
    {
        assert( 0xdd == op || 0xfd == op );
        assert( 0 == hl || 1 == hl );

        uint8_t * pval;
        if ( 0xdd == op )
            pval = (uint8_t *) & ( this->ix );
        else
            pval = (uint8_t *) & ( this->iy );

        if ( 0 == hl )
            pval++;

        return pval;
    } //getIndexByte

    uint8_t * regOffset( uint8_t m )
    {
        assert( 6 != m ); // that's a memory access, not a register. f can't be used directly.
        return ( ( ( uint8_t *) this ) + reg_offsets[ m ] );
    } //regOffset

    void clearHN()
    {
        fAuxCarry = false;
        fWasSubtract = false;
    } //clearHN

    void assignYX( uint8_t val )
    {
        fY = ( 0 != ( val & 0x20 ) );
        fX = ( 0 != ( val & 0x08 ) );
    } //assignYX

    bool getFlag( uint8_t x )
    {
        assert( x <= 3 );
        return * ( ( & fZero ) + x );
    } //getFlag

    void incR() { r++; r &= 0x7f; }
    void decR() { r--; r &= 0x7f; }

    const char * renderFlags()
    {
        static char ac[10] = {0};
        size_t next = 0;
        ac[ next++ ] = fSign ? 'S' : 's';
        ac[ next++ ] = fZero ? 'Z' : 'z';
        if ( fZ80Mode )
            ac[ next++ ] = fY ? 'Y' : 'y';

        if ( fZ80Mode )
            ac[ next++ ] = fAuxCarry ? 'H' : 'h'; // half-carry; same meaning as aux-carry
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

    void powerOn()
    {
        pc = 0;
        r = 1;
        i = 0;
        a = f = b = c = d = e = h = l = 0xff;
        sp = ix = iy = 0xffff;
        ap = fp = bp = cp = dp = ep = hp = lp = 0xff;
        fZero = fCarry = fParityEven_Overflow = fSign = fAuxCarry = fWasSubtract = fY = fX = 0;
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
