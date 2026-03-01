@echo off
setlocal

SET WATCOM=..\OW
SET PATH=%WATCOM%\BINNT64;%WATCOM%\BINNT;%PATH%
SET EDPATH=%WATCOM%\EDDAT
SET INCLUDE=%WATCOM%\LH;%WATCOM%\H;.;..\djl

rem -oe=160 generates faulty code including i64 -3 * -14 = 0
rem use of x87 fp emulation with -fpc produces incorrect results for several tests

wcl386 -q -oh -oi -ol+ -ei -ot -ombr -fp3 -3r -cc++ -xs -xr ntvcm.cxx x80.cxx -bcl=LINUX -k8192 -fe=ntvcml -DWATCOM -DWATCOMLINUX -DNDEBUG


