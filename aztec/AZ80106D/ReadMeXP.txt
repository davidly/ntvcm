BEFORE USING IT BE A GOOD IDEA FOR YOU TO REVIEW BUGS.TXT FOR KNOWN ISSUES
WITH WINDOWS XP AND TROUBLESHOOTING THESE ISSUES.

ReadMe for C:\AZ80106D
April, 2008

Manx Aztec C80
Version 1.06d
Developer System
for CP/M-80
Target: Z80 and 8080 Native Code

CP/M-80 development environment for Z80 and compatible computers
Preconfigured for cross-development under MS-DOS or Windows XP.

With samples.

Disclaimer and Conditions of Use

This compiler is both the MS-DOS cross-compiler and the native mode CP/M
80 Aztec CZ80 Version 1.06d (C) Copyright Manx Software Systems, Inc. and
also includes the earlier Aztec CZ80 Version 1.05 for native mode CP/M 80.
I cannot provide you with a legally licenced copy.

I herewith grant you a non-exclusive conditional licence to use any and
all of my work included with this compiler for whatever use you deem fit,
provided you do not take credit for my work, and that you leave my
copyright notices intact in all of it.

I believe everything I have written to be correct. Regardless, I, Bill
Buckels, do not guarantee, or warranty, in whole or in part, any of this,
and further do not offer support for any of this. All of this is provided
as-is.

Bill Buckels
bbuckels@mts.net

Introduction

This is a complete Aztec C build environment for Windows XP (MS-DOS) which
will enable you to produce Z80 programs for use under CP/M-80.

The flavour of C that is used is not ANSI compliant. I am unable to
provide a manual for this compiler. Review the samples, library source
code and other documents that I have provided for details on its use. Also
review the Aztec-C and CP/M Message Boards at the the link below for
additional details about setting-up this compiler and CP/M 80:

http://landover.no-ip.com/forums/

This environment includes 3 compilers as noted above. Amazingly the cross-
compiler and the newest native mode compiler are exactly the same version,
yet I got them from 2 entirely different sources,the cross-compiler coming
from a gentleman who emailed me a copy that he purchased directly from
Manx as I did my own cross-compilers that I resurrected last year. 

Generally speaking directories like BIN80 and SRC80 are from the native
mode compiler version and directories like BIN and SRC are from the cross-
compiler. For using this compiler in XP or MS-DOS you will only need the
cross-compiler but if you are ambitious and wish to build yourself some
native mode disks you will have the CP/M 80 native mode version.

Also included are  several tools. The emulator disk images are targeted at
either the x128 Vice emulator for the C128 or the MyZ80 emulator from
Simeon Cran. You must download these emulators yourself.

Vice http://www.viceteam.org/

MyZ80 - http://primepuzzle.com/mouse/maxz80.zip
Tutorial - http://primepuzzle.com/mouse/hints0.htm 

I have included a third emulator called ZRUN which doesn't need a
diskimage and is OK for testing console programs but I haven't been able
to use it for anything else in Windows XP and haven't tried it in the
DOSBox emulator. Obviously what works for some will not work for others.
In the interest of moving-on with my life I have left this in place
without taking it further. 

There is much more that can be done with all of this. 

You can review the MAKEFILE's in the SAMPLES directories to see what it is
I am doing and review the contents of each directory and their
subdirectories for more information. Always save a copy of your original
directory if modifying or building anything.

If you don't know what CP/M-80 was you didn't miss much because of the way
everything turned-out but should probably read the Wikipedia article for a
synopsis:

http://en.wikipedia.org/wiki/CP/M-80

OTOH without the assisted suicide of CP/M Microsoft arguably may not have
been the power that it is today... a point for discussion that makes little
difference now.

Another link especially if you think you have missed something is the 
Unofficial CP/M Web Site:

http://www.cpm.z80.de/

Getting Started

Unzip the ZIP file(s) with directories intact onto the root of your C:
Drive. The Windows XP shortcut has been pre-configured to use this directory
structure.

Open the C:\AZ80106D folder in MyComputer and click-on the shortcut. A cmd
window will open, and you will be in the C:\AZ80106D directory which is the
parent directory for this environment. The required environment variables to
build Aztec C programs are set for you, and you can build these anywhere on
your computer from this cmd window.

You must be somewhat familiar with DOS commands to use this build
environment. You must CD (Chdir) to each programs build directory.

You can test the environment by building the programs in the SAMPLES
subdirectories. Each has its own MAKEFILE which will build the program simply
by typing "make" and pressing [ENTER] while in each's project directory under
SAMPLES.

You can even copy the XP shortcut that I have provided to your working
directory and set that as your startup directory to avoid much typing.

To recap, in any of the program build directories just type "Make" and press
enter to make any of the programs provided as samples. Provided you have
used the XP shortcut noted above, you should have no problems. Otherwise use
a DOS emulator like DOSBox in which case you will need to set-up your own
environment by modifying C:\AZ80106D\AZTEC.BAT.

What You Will Need

All you need is the ability to write and compile programs in the C
programming language.

Review the various ReadMe files and source code in this environment for
additional notes. Or better yet, just build the sample programs and
experiment.

Have Fun!

Bill Buckels
bbuckels@mts.net
April 2008

(C) Copyright Bill Buckels 2008
All Rights Reserved.

End of Document

