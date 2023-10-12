@echo off
rem this build script targets 16-bit 8086 DOS.
rem I used:  Open Watcom C/C++ x86 16-bit Compile and Link Utility
rem          Version 2.0 beta Oct  9 2023 02:19:55 (64-bit)
rem from:    https://github.com/open-watcom/open-watcom-v2/releases/tag/Current-build

@echo on
wcl -q -zp=1 -ml -obmr -oh -ei -oi -s -0 -xs -j -oe=128 -ol+ -ot ntvcm.cxx x80.cxx -bcl=DOS -k8192 /I. /DWATCOM /DDEBUG

