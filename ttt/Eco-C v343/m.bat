@echo off
setlocal

ntvcm cp %1.c
ntvcm m80 =%1
ntvcm l80 %1,bdos,qsort,%1/N/E



