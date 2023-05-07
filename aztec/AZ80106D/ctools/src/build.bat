@ECHO=OFF
REM
REM $Id: build.bat,v 1.1 1997/02/20 23:21:23 jochen Rel $
REM
SET CC=sc
SET CXX=sc -cpp
SET CXXFLAGS=
SET CPPFLAGS=-Igetopt -DVERSION="V0.4"
SET AR=lib
SET LOADLIBS=local.lib
ECHO @@@
ECHO @@@ built local.lib
ECHO @@@
%CC% -Igetopt -c -ogetopt\getopt getopt\getopt.c
IF ERRORLEVEL 1 GOTO END
%CC% -Igetopt -c -ogetopt\getopt1 getopt\getopt1.c
IF ERRORLEVEL 1 GOTO END
%AR% local.lib /c /noi +getopt\getopt.obj+getopt\getopt1.obj,, 
IF ERRORLEVEL 2 GOTO END
ECHO @@@
ECHO @@@ built d64dump.exe
ECHO @@@
%CXX% %CXXFLAGS% %CPPFLAGS% -c dump.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c err.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c misc.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c d64.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c d64dump.cc
IF ERRORLEVEL 1 GOTO END
%CXX% -od64dump d64dump.obj dump.obj misc.obj err.obj d64.obj %LOADLIBS%
IF ERRORLEVEL 1 GOTO END
ECHO @@@
ECHO @@@ built biosdump.exe
ECHO @@@
%CXX% %CXXFLAGS% %CPPFLAGS% -c bios.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c biosdump.cc
IF ERRORLEVEL 1 GOTO END
%CXX% -obiosdump biosdump.obj dump.obj misc.obj err.obj d64.obj bios.obj %LOADLIBS%
IF ERRORLEVEL 1 GOTO END
ECHO @@@
ECHO @@@ built cformat.exe
ECHO @@@
%CXX% %CXXFLAGS% %CPPFLAGS% -c cformat.cc
IF ERRORLEVEL 1 GOTO END
%CXX% -ocformat cformat.obj err.obj misc.obj d64.obj bios.obj %LOADLIBS%
IF ERRORLEVEL 1 GOTO END
ECHO @@@
ECHO @@@ built ctools.exe
ECHO @@@
%CXX% %CXXFLAGS% %CPPFLAGS% -c bitstr.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c fcb.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c dir.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c file.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c tools.cc
IF ERRORLEVEL 1 GOTO END
%CXX% %CXXFLAGS% %CPPFLAGS% -c ctools.cc
IF ERRORLEVEL 1 GOTO END
%CXX% -octools ctools.obj dump.obj err.obj misc.obj d64.obj bios.obj bitstr.obj fcb.obj dir.obj file.obj tools.obj %LOADLIBS%
IF ERRORLEVEL 1 GOTO END
ECHO @@@
ECHO @@@ copy *.exe to ../bin
ECHO @@@
mkdir ..\bin
copy /b *.exe ..\bin /v
ECHO @@@
ECHO @@@ all done
ECHO @@@
:END
SET CC=
SET CXX=
SET CXXFLAGS=
SET CPPFLAGS=
SET AR=
SET LOADLIBS=
