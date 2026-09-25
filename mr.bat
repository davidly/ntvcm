@echo off
setlocal

call "%~dp0msvcenv.bat"
if errorlevel 1 exit /b 1

rem with RSS
cl /W4 /wd4706 /wd4996 /nologo ntvcm.cxx x80.cxx /DNDEBUG /DNTVCM_RSS_SUPPORT /openmp /I. /GS- /GL /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi %MSVC_ARCH_OPTIONS% /link user32.lib /OPT:REF

rem without RSS
rem cl /W4 /wd4706 /wd4996 /nologo ntvcm.cxx x80.cxx /DNDEBUG /I. /GS- /GL /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi %MSVC_ARCH_OPTIONS% /link user32.lib ntdll.lib /OPT:REF

