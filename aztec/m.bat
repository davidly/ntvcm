if "%1" == "" goto :fail

ntvcm cc -T -F -DCPMTIME %1.c
ntvcm as -L %1.asm
ntvcm ln -T %1.o c.lib
goto :done

:fail
echo no input specified

:done

