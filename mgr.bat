@echo off
setlocal

path=d:\mingw64\bin;%path%

g++ -Ofast -ggdb -fopenmp -D _MSC_VER -D _GNU_CPP ntvcm.cxx x80.cxx -I ../djl -D NDEBUG -o ntvcm.exe -static -lwininet


