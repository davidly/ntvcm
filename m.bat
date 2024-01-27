setlocal

rem With RSS
cl /nologo ntvcm.cxx x80.cxx /DNTVCM_RSS_SUPPORT /openmp /I. /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi /jumptablerdata /D_AMD64_ /link user32.lib /OPT:REF

rem Without RSS
rem cl /nologo ntvcm.cxx x80.cxx /I. /Oti2 /Ob3 /Qpar /Fa /FAsc /EHac /Zi /jumptablerdata /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF

