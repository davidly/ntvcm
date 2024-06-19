@echo off
del %1.rel 2>nul
del %1.com 2>nul

rem compile
ntvcm cobol %1,%1=%1

rem link
ntvcm l80 %1,%1/N/E


