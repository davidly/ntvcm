# ntvcm
NT Virtual CP/M Machine. Emulates CP/M 2.2 and the 8080/Z80 on Windows to run .com files. 

This app emulates the subset of CP/M 2.2 required to run asm.com, load.com, and Turbo Pascal
versions 1.00 and 3.01A. The latter two use bdos APIs very differently.

Console input/output work for both bios bdos API. Disk input/output work via the bdos API.
There is no attempt at emulating physical disks; apps that use bdos will just work.

The x80 emulator passes at 100% for:
    
    8080: 8080ex1.com, 8080pre.com, Microcosm v1.0, and cputest.com V1.2 in 8080 mode.
    
    Z80: zexall.com, zexdoc.com, and cputest.com V1.2 in Z80 mode.
    
I implemented this to automate testing of apps generated by the BA BASIC compiler in my TTT repo,
so I wouldn't have to copy files to my TRS-80 Model 100 or Z80 CP/M 2.2 machines. It was just
as painful copying them to other emulators because those (that I found) can't be invoked with 
other Windows apps in a test script.

The app supports tracing to the ntvcm.log file. Instruction tracing includes a disassembler
that shows instructions either as 8080 or Z80 depending on the mode.

Performance of the CPU emulator is in the ballpark of other emulators I looked at. 
I wrote the code to be more readable than other emulators, whose use of lookup tables and macros
I found to be nearly inscrutable (though I'm sure that's just me). I also wanted both 8080 and Z80
modes, which hurts the performance of each. An interesting fact is that on modern CPUs, code for 
computing parity of a byte is faster than a 256-element lookup table. zexall.com runs 5.748 billion
instructions shared with the 8080, and just 16 million instructions unique to the Z80; optimizing
for the 8080 will have the most impact.

Cycle counts are pretty close, but not precise. I used documented numbers, which are sometimes
incorrect. And I made guesses for cycle counts for undocumented Z80 instructions.

Support for Z80 undocumented instructions and the Y and X flags passes the tests specified
above, but I can't vouch for more than that. Undocumented 8080 instructions are not implemented.

The two versions of Turbo Pascal and the apps they generate use a tiny fraction of Z80 instructions.
It took a day to implement the instructions for Turbo Pascal, and three more days to implement the 
full instruction set. Getting the undocumented featuers to work took an extra couple days. The 
invaluable resources required to do that are listed in x80.cxx. No single resource was 100% correct
or complete. It takes a village :)

    NT Virtual CP/M 2.2 Machine: emulates a CP/M 2.2 i8080/Z80 runtime environment

    usage: ntvcm [-c] [-p] [-s:X] [-t] <cp/m 2.2 .com file> [filearg1] [filearg2]

    notes:    filearg1 and filearg2 optionally specify filename arguments for the command
              -c     don't auto-detect ESC characters and change to to 80x24 mode
              -i     trace 8080/Z80 instructions when tracing
              -p     show performance information
              -s:X   speed in Hz. Default is 0, which is as fast as possible.
                     for 4Mhz, use -s:4000000
              -t     enable debug tracing to ntvcm.log
              -8     emulate 8080, not Z80
              e.g. to assemble, load, and run test.asm:
                   ntvcm asm.com test
                   ntvcm load.com test
                   ntvcm test.com

Example usage:

    C:\>ntvcm -p z80test\zexall.com
    Z80all instruction exerciser
    <adc,sbc> hl,<bc,de,hl,sp>....  OK
    add hl,<bc,de,hl,sp>..........  OK
    add ix,<bc,de,ix,sp>..........  OK
    add iy,<bc,de,iy,sp>..........  OK
    aluop a,nn....................  OK
    aluop a,<b,c,d,e,h,l,(hl),a>..  OK
    aluop a,<ixh,ixl,iyh,iyl>.....  OK
    aluop a,(<ix,iy>+1)...........  OK
    bit n,(<ix,iy>+1).............  OK
    bit n,<b,c,d,e,h,l,(hl),a>....  OK
    cpd<r>........................  OK
    cpi<r>........................  OK
    <daa,cpl,scf,ccf>.............  OK
    <inc,dec> a...................  OK
    <inc,dec> b...................  OK
    <inc,dec> bc..................  OK
    <inc,dec> c...................  OK
    <inc,dec> d...................  OK
    <inc,dec> de..................  OK
    <inc,dec> e...................  OK
    <inc,dec> h...................  OK
    <inc,dec> hl..................  OK
    <inc,dec> ix..................  OK
    <inc,dec> iy..................  OK
    <inc,dec> l...................  OK
    <inc,dec> (hl)................  OK
    <inc,dec> sp..................  OK
    <inc,dec> (<ix,iy>+1).........  OK
    <inc,dec> ixh.................  OK
    <inc,dec> ixl.................  OK
    <inc,dec> iyh.................  OK
    <inc,dec> iyl.................  OK
    ld <bc,de>,(nnnn).............  OK
    ld hl,(nnnn)..................  OK
    ld sp,(nnnn)..................  OK
    ld <ix,iy>,(nnnn).............  OK
    ld (nnnn),<bc,de>.............  OK
    ld (nnnn),hl..................  OK
    ld (nnnn),sp..................  OK
    ld (nnnn),<ix,iy>.............  OK
    ld <bc,de,hl,sp>,nnnn.........  OK
    ld <ix,iy>,nnnn...............  OK
    ld a,<(bc),(de)>..............  OK
    ld <b,c,d,e,h,l,(hl),a>,nn....  OK
    ld (<ix,iy>+1),nn.............  OK
    ld <b,c,d,e>,(<ix,iy>+1)......  OK
    ld <h,l>,(<ix,iy>+1)..........  OK
    ld a,(<ix,iy>+1)..............  OK
    ld <ixh,ixl,iyh,iyl>,nn.......  OK
    ld <bcdehla>,<bcdehla>........  OK
    ld <bcdexya>,<bcdexya>........  OK
    ld a,(nnnn) / ld (nnnn),a.....  OK
    ldd<r> (1)....................  OK
    ldd<r> (2)....................  OK
    ldi<r> (1)....................  OK
    ldi<r> (2)....................  OK
    neg...........................  OK
    <rrd,rld>.....................  OK
    <rlca,rrca,rla,rra>...........  OK
    shf/rot (<ix,iy>+1)...........  OK
    shf/rot <b,c,d,e,h,l,(hl),a>..  OK
    <set,res> n,<bcdehl(hl)a>.....  OK
    <set,res> n,(<ix,iy>+1).......  OK
    ld (<ix,iy>+1),<b,c,d,e>......  OK
    ld (<ix,iy>+1),<h,l>..........  OK
    ld (<ix,iy>+1),a..............  OK
    ld (<bc,de>),a................  OK
    Tests complete
    Z80  cycles:      46,716,028,182
    clock rate:            unbounded
    approx ms at 4Mhz:    11,679,007
    kernel CPU ms:                31
    user CPU ms:              25,890
    total CPU ms:             25,921
    elapsed ms:               26,574
    

