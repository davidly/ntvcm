@echo off
setlocal

path=d:\mingw64\bin;%path%

g++ -Ofast -ggdb -D _MSC_VER -D _GNU_CPP ntvcm.cxx x80.cxx -I ../djl -D DEBUG -o ntvcm.exe


