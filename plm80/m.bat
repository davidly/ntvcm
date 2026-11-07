@echo off
rem Build the native PDP-10 runner; compile.py falls back to Python without it.
setlocal
pushd "%~dp0" || exit /b 1
call "..\msvcenv.bat"
if errorlevel 1 goto failed
cl /nologo /std:c++14 /EHsc /O2 /GL /W4 /D_CRT_SECURE_NO_WARNINGS pdp10.cxx /Fe:pdp10.exe /link /LTCG
if errorlevel 1 goto failed
popd
exit /b 0
:failed
set "result=%errorlevel%"
popd
exit /b %result%
