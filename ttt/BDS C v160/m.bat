@echo off
setlocal

ntvcm cc %1.c -o
echo:
ntvcm clink %1 -sw
echo:

