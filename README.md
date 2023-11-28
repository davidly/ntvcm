# ntvcm
NT Virtual CP/M Machine. Emulates CP/M 2.2 and the 8080/Z80 on Linux, MacOS, Windows, and real-mode DOS to run .com files. 

Tested with:

    asm.com
    load.com
    Turbo Pascal v1.00
    Turbo Pascal v3.01A
    WordStar Release 4
    mbasic.com BASIC-80 rev. 5.21 (startrek and other apps)
    Aztec C v1.06 (compiler, assembler, linker, and generated apps)
    CalcStar v1.0
    Microsoft fortran-80 v3.4
    Microsoft Multiplan (C) 1981
    CamelForth beta test version
    strtrk.com built for Kaypro II
    BASCOM 5.30a
    Microsoft MS-COBOL Version 4.65
    Digital Research CBASIC Compiler CB-80 Version 2.0
    
Console input/output work for both bios bdos API. Disk input/output work via the bdos API. There is no attempt at emulating physical disks. Apps that use bdos for disk I/O will work but apps that use the BIOS or assume things about disk layout will not.

The x80 emulator passes at 100% for:
    
    8080: 8080ex1.com, 8080pre.com, Microcosm v1.0, and cputest.com V1.2 in 8080 mode.
    
    Z80: zexall.com, zexdoc.com, and cputest.com V1.2 in Z80 mode.
    
I implemented this to automate testing of apps generated by the BA BASIC compiler in my TTT repo, so I wouldn't have to copy files to my TRS-80 Model 100 or Z80 CP/M 2.2 machines. It was just as painful copying them to other emulators because those (that I found) can't be invoked with other Windows apps in a test script.

The app supports tracing to the ntvcm.log file. Instruction tracing includes a disassembler that shows instructions either as 8080 or Z80 depending on the mode.

I've done some testing on Linux and MacOS with the same set of test apps as Windows. It all seems to work, most importantly the CPU tests. CP/M apps expect cr/lf in text files, so you may need to use a tool like unix2dos to convert .asm and .bas files so they work properly in the emulator in those environments. The -l flag in ntvcm is useful if you want to use lowercase filenames instead of CP/M's default of uppercase.

Performance of the CPU emulator is in the ballpark of other emulators I looked at. I wrote the code to be more readable than other emulators, whose use of lookup tables and macros I found to be nearly inscrutable (though I'm sure that's just me). I also wanted both 8080 and Z80
modes, which hurts the performance of each. An interesting fact is that on modern CPUs, code for computing parity of a byte is faster than a 256-element lookup table. 

zexall.com runs 5.748 billion instructions shared with the 8080, and just 16 million instructions unique to the Z80; optimizing the emulator for the 8080 will have the most impact. On my machine, zexall.com runs in about 18.4 seconds using the Microsoft compiler and 16.7 seconds using the Gnu compiler. On a real 4Mhz Z80 it'd take about 3 hours and 14 minutes.

Cycle counts are pretty close, but not precise. I used documented numbers, which are sometimes incorrect. And I made guesses for cycle counts for undocumented Z80 instructions.

Support for Z80 undocumented instructions and the Y and X flags passes the tests specified above, but I can't vouch for more than that. Undocumented 8080 instructions are not implemented.

Non-standard BDOS calls are added for sleeping for a specified number of milliseconds and to fetch and iterate through RSS feeds. On Linux and MacOS, this creates dependencies on httplib.h from  https://github.com/yhirose/cpp-httplib and openssl. You must have these dependencies copied
to your machine for NTVCM to build. Or, remove the RSS feature by not defining NTVCM_RSS_SUPPORT in your build script. Each build script includes lines for building with and without. I've been unable to build on MacOS with RSS enabled. rssrdr.c in the Aztec folder is a sample CP/M RSS reader app that can be built with that compiler using m.bat in that folder.

NTVCM can be built to target RISC-V using g++ then run in the [RVOS RISC-V / Linux emulator](https://github.com/davidly/rvos). RVOS can run that RISC-V image on Windows/MacOS/Linux on AMD64, x86, ARM32, or ARM64.

NTVCM can be built to target real-mode DOS using the Watcom compiler. Details in mwatcom.bat. 

The two versions of Turbo Pascal and the apps they generate use a tiny fraction of Z80 instructions. It took a day to implement the instructions for Turbo Pascal, and three more days to implement the full instruction set. Getting the undocumented featuers to work took an extra couple days. The invaluable resources required to do that are listed in x80.cxx. No single resource was 100% correct or complete. It takes a village :)

    NT Virtual CP/M 2.2 Machine: emulates a CP/M 2.2 i8080/Z80 runtime environment

    usage: ntvcm [-c] [-p] [-s:X] [-t] <cp/m 2.2 .com file> [filearg1] [filearg2]

    notes:    filearg1 and filearg2 optionally specify filename arguments for the command
            -b     translate bios console input backspace (BS / 0x08) to delete (DEL / 0x7f).
            -c     never auto-detect ESC characters and change to to 80x24 mode
            -C     always switch to 80x24 mode
            -d     don't clear the display on app exit when in 80x24 mode
            -i     trace 8080/Z80 instructions when tracing with -t
            -k     translate Kaypro II extended characters to Windows code page 437 or Linux ascii art
            -l     force CP/M filenames to be lowercase (can be useful on Linux)
            -p     show performance information at app exit
            -s:X   speed in Hz. Default is 0, which is as fast as possible.
                   for 4Mhz, use -s:4000000
            -t     enable debug tracing to ntvcm.log
            -v:X   translate escape sequences to VT-100 where X can be one of:
                     5:  translate vt-52 escape sequences. for CalcStar and other apps
                     k:  translate Kaypro II / Lear-Siegler ADM-3A escape sequences. for strtrk.com
            -z:X   applies X as a hex mask to SetProcessAffinityMask, e.g.:
                     /z:11    2 performance cores on an i7-1280P
                     /z:3000  2 efficiency cores on an i7-1280P
                     /z:11    2 random good cores on a 5950x
            -8     emulate the i8080, not Z80
            e.g. to assemble, load, and run test.asm:
                 ntvcm asm.com test
                 ntvcm load.com test
                 ntvcm test.com
            e.g. to run Star Trek in mbasic in 80x24 mode using i8080 emulation:
                 ntvcm -8 -C mbasic startrek.bas

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
    
    elapsed milliseconds:           18,366
    Z80  cycles:            46,716,093,718
    clock rate:                  unbounded
    approx ms at 4Mhz:          11,679,023 == 0 days, 3 hours, 14 minutes, 39 seconds, 23 milliseconds
    

