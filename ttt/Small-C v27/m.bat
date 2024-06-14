ntvcm cc -p %1.c >%1.out
rem delete the first line of the output file
more +1 %1.out >%1.mac
ntvcm m80 %1=%1
ntvcm l80 %1,clib/s,%1/N/E
