@echo off
setlocal

call "%~dp0msvcenv.bat"
if errorlevel 1 exit /b 1

rem With RSS
cl /nologo ntvcm.cxx x80.cxx /DNTVCM_RSS_SUPPORT /openmp /I. /GS- /GL /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi %MSVC_ARCH_OPTIONS% /link user32.lib /OPT:REF

rem Without RSS
rem cl /nologo ntvcm.cxx x80.cxx /I. /GS- /GL /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi %MSVC_ARCH_OPTIONS% /link user32.lib ntdll.lib /OPT:REF

