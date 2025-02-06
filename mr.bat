
@ECHO OFF
FOR /F "tokens=*" %%s IN ('git log -1 HEAD --format^=%%h ') DO (SET COMMIT=%%s)
rem ECHO %COMMIT%

rem with RSS
cl /W4 /wd4706 /wd4996 /nologo ntvcm.cxx x80.cxx /DNDEBUG /DCOMMIT_ID=%COMMIT% /DNTVCM_RSS_SUPPORT /openmp /I. /GS- /GL /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi /jumptablerdata /D_AMD64_ /link user32.lib /OPT:REF

rem without RSS
rem cl /W4 /wd4706 /wd4996 /nologo ntvcm.cxx x80.cxx /DNDEBUG /DCOMMIT_ID=%COMMIT% /I. /GS- /GL /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi /jumptablerdata /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF

