@echo off
del %1.s    2>nul
del %1.o    2>nul
del %1.tm1  2>nul
del %1.tm2  2>nul

ntvcm cpp -x -o %1.tm1 %1.c
ntvcm cp1 -b0 -n8 -o %1.tm2 %1.tm1
ntvcm cp2 -o %1.s %1.tm2
ntvcm a80 -o %1.o %1.s
ntvcm lnk -eb__memory -tb0x100 -htr -o %1.com crtx.o %1.o libc.80
