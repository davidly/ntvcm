@echo off
del %1.rel
del %1.com

rem compile
ntvcm -t cobol %1,%1=%1
copy ntvcm.log compile.log 1>nul 2>nul

rem link
ntvcm -t l80 %1,%1/N/E
copy ntvcm.log link.log 1>nul 2>nul


