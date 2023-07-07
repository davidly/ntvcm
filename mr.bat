rem with RSS
cl /nologo ntvcm.cxx x80.cxx /DNTVCM_RSS_SUPPORT /openmp /I. /Oti2 /Ob2 /Qpar /Fa /FAsc /EHac /Zi /DNDEBUG /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF

rem without RSS
rem cl /nologo ntvcm.cxx x80.cxx /I. /Oti2 /Ob2 /Qpar /Fa /FAsc /EHac /Zi /DNDEBUG /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF

