@echo off
setlocal
SET WATCOM=..\WATCOM
SET PATH=%WATCOM%\BINNT64;%WATCOM%\BINNT;%PATH%
SET EDPATH=%WATCOM%\EDDAT
SET INCLUDE=%WATCOM%\H;%WATCOM%\H\NT;..\djl

rem wcl -d0 -q -zp=1 -ml -obmr -oh -ei -oi -s -0 -xs -j -oe=160 -ol+ -ot ntvcm.cxx x80.cxx -bcl=DOS -k8192 -fe=ntvcmdos.exe -DWATCOMDOS -DNDEBUG -lr -I.
rem ntvcm.cxx and x80.cxx are compiled separately so they can use different inline limits.
rem Each file's code must fit in one 64k segment; ntvcm.cxx outgrew that at -oe=100.
rem x80.cxx (the CPU emulator) keeps -oe=100 for speed. ntvcm.cxx (mostly BDOS calls) uses -oe=80.
set OPTS=-d0 -q -zp=1 -ml -obmr -oh -ei -oi -s -0 -xs -j -ol+ -ot -bcl=DOS -DWATCOMDOS -DNDEBUG -I.
wcl %OPTS% -oe=80 -c ntvcm.cxx
if errorlevel 1 exit /b 1
wcl %OPTS% -oe=100 -c x80.cxx
if errorlevel 1 exit /b 1
wcl %OPTS% ntvcm.obj x80.obj -k8192 -fe=ntvcmdos.exe -lr

