@echo off
if "%CLIB%" == "" goto ERR
if exist az80105.d64 del az80105.d64
if exist maked64.log del maked64.log
echo ========================>> maked64.log
echo Log File for Maked64.bat>> maked64.log
echo %DATE% - %TIME% >> maked64.log
echo Contents of az80105.d64>> maked64.log 
echo ========================>> maked64.log
call cformat -1 az80105.d64 
call ctools az80105.d64 p AS.COM 
call ctools az80105.d64 p CC.SUB 
call ctools az80105.d64 p CII.COM 
call ctools az80105.d64 p EXMPL.C 
call ctools az80105.d64 p LIBC.H 
call ctools az80105.d64 p LIBC.LIB 
call ctools az80105.d64 p LN.COM 
call ctools az80105.d64 p XDIR.COM 
call ctools az80105.d64 d >> maked64.log
echo ========================>> maked64.log
echo Done!
goto END
:ERR
echo Run in the Aztec CZ80 environment.
:END			  