@echo off
setlocal

rem With RSS
g++ -Ofast -ggdb -fopenmp -D NTVCM_RSS_SUPPORT -D _MSC_VER -D _GNU_CPP ntvcm.cxx x80.cxx -I ../djl -D NDEBUG -o ntvcmg.exe -static -lwininet

rem Without RSS
rem g++ -Ofast -ggdb -D _MSC_VER -D _GNU_CPP ntvcm.cxx x80.cxx -I ../djl -D NDEBUG -o ntvcmg.exe -static



