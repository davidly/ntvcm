@echo off
if not exist c:\maxz80\*.* goto END
if "%CROOT%" == "" goto END
if "%1" == "Z80" goto BEGIN
cls
REM MAKEMY.BAT (C) Copyright Bill Buckels 2008
REM this batchfile assumes that maxz80 is installed in c:\maxz80
REM it further assumes that it is being run from within the
REM C:\AZ8106d\MyZ80 subdirectory from within the
REM Aztec C Z80 Build Environment under XP

REM create the main CPM submit file

echo ERA MAKEMY.SUB>MYMAIN.SUB
echo IMPORT MAKEMY.SUB>>MYMAIN.SUB
echo SUBMIT MAKEMY.SUB>>MYMAIN.SUB

REM finish the secondary CPM submit file
REM add the rest of the files
REM call this batch recursiveley to do so.

for %%f in (*.com) do call makemy.bat Z80 %%f
for %%f in (*.ovr) do call makemy.bat Z80 %%f

REM finish-off the submit file
REM so it automatically exits CPM when done
echo EXIT>>MAKEMY.SUB

REM we are done the prep-work now.
REM copy the baggage to the cpm emulator directory
copy *.com c:\maxz80\. >NUL
copy *.ovr c:\maxz80\. >NUL
copy MYMAIN.SUB c:\maxz80\. >NUL
copy MAKEMY.SUB c:\maxz80\. >NUL

REM start the emulator
REM this is the manual part.
REM the user needs to switch to drive C:
REM then IMPORT and SUBMIT MYMAIN.SUB
REM the rest is automatic.

SET C.DSK=AZSAMPLE.DSK
cd c:\maxz80\
echo This is the non-automated part of this process.
echo 1. After reading this, press a key. MYZ80 will start.
echo 2. You will then need to change to Drive C in MYZ80.
echo    Do this by typing C: then pressing the Enter Key.
echo 3. You will now need to build your disk.
echo    Do this by typing IMPORT MYMAIN.SUB them
echo    SUBMIT MYMAIN.SUB and press the Enter Key.
echo 4. The rest is automated beyond that point.
pause
cls
call MYZ80
SET C.DSK=


REM copy the finished disk back
copy AZSAMPLE.DSK C:\AZ80106d\MyZ80\. >NUL
cd C:\AZ80106d\MyZ80
cls
goto END

:BEGIN

REM this is the recursively called area

echo IMPORT %2>>MAKEMY.SUB

:END

