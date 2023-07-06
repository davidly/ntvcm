setlocal

rem set include=%include%;c:\program files (x86)\microsoft visual studio\2019\community\common7\ide\extensions\microsoft\python\miniconda\miniconda3-x64\library\include\
rem set lib=%lib%;c:\program files (x86)\microsoft visual studio\2019\community\common7\ide\extensions\microsoft\python\miniconda\miniconda3-x64\library\lib\

rem With RSS
cl /nologo ntvcm.cxx x80.cxx /DNTVCM_RSS_SUPPORT /openmp /I. /Oti2 /Ob2 /Qpar /Fa /FAsc /EHac /Zi /DDEBUG /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF

rem Without RSS
rem cl /nologo ntvcm.cxx x80.cxx /I. /Oti2 /Ob2 /Qpar /Fa /FAsc /EHac /Zi /DDEBUG /D_AMD64_ /link user32.lib ntdll.lib /OPT:REF

