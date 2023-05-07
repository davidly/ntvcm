@echo off
if "%CLIB%" == "" goto START
goto END

   This is a modified version of the AZTEC Batch which should
   run on either a floppy or a hard drive...

:START
set CROOT=\az80106d\
set CLIB=%CROOT%LIB\;
set INCLUDE=%CROOT%INCLUDE\;
set PATH=%CROOT%BIN;%CROOT%UTL;%PATH%;
:END
