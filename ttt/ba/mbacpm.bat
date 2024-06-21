@echo off
ba /a:8 /x %1 >nul
ntvcm asm.com %1 >nul
ntvcm load.com %1 >nul

