@echo off
setlocal

rem this small C compiler requires the input filename to be in upper case
set str=%1
call :TOUPPERCASE str
goto :compile

:TOUPPERCASE
if not defined %~1 exit /b
for %%a in ("a=A" "b=B" "c=C" "d=D" "e=E" "f=F" "g=G" "h=H" "i=I" "j=J" "k=K" "l=L" "m=M" "n=N" "o=O" "p=P" "q=Q" "r=R" "s=S" "t=T" "u=U" "v=V" "w=W" "x=X" "y=Y" "z=Z" "Ñ=é" "î=ô" "Å=ö") do (
call set %~1=%%%~1:%%~a%%
)

exit /b 0

:compile
ntvcm cc %str%
ntvcm zmac %str%=%str%

rem if you need floating point...
rem NTVCM ZLINK %str%,%str%=%str%,IOLIB,FLOAT,PRINTF2

rem if you don't
NTVCM ZLINK %str%,%str%=%str%,IOLIB,PRINTF1



