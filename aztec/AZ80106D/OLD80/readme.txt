ReadMe for OLD80

This directory contains Aztec CZ80 version 1.05 which amazingly fits on a
single sided C128 CPM 80 disk.

The down-side is that the C link library is pretty tiny:

**DIR**     supp8080    fprintf     fscanf      scan        fltlib      
fopen       fread       **DIR**     fseek       fgets       fputs       
getc        putc        ungetc      getbuff     setbuf      alloc       
format      callcpm     croot       open        read        write       
lseek       posit       **DIR**     blkio       rename      unlink      
atof        atol        atoi        block       atol        atoi        
block       string      toupper     closall     lsubs       fstub       

Two disk images are provided. AZ80105.DSK is for MyZ80 and AZ80105.D64 is
for the x128 Vice emulator.

Generally speaking you would not want to bother with a native mode cpm
compiler especially on the x128, however the MyZ80 emulator works pretty
well with this.

I am assuming you will have a SUBMIT command to use.

On the x128 emulator use the d81 diskimage also bundled with this lot to
boot cpm. Then put this d64 into drive B: Then to compile anything like
the exempl.c program that is on the d64 just type "A:SUBMIT CC EXEPL" and
press [Return]. Do yourself a favour and put the thing into WarpMode
directly after typing. However your keystrokes will repeat like crazy if
you try and run in warpmode during normal program execution.

On the MyZ80 emulator set the environment C.DSK=AZ80105.DSK and then start
MyZ80. Do this in a cmd window or in MS-DOS. Once inside MyZ80 type C: to
get on the C: drive that you have just assigned to AZ80105.DSK. Then type
"SUBMIT CC EXMPL" and press [Enter].

One more thing: This is pretty tight on disk space in x128 and you have a
copy of xdir on the image to determine just how tight. Use PIP to copy
part of your compiler to A: if you wish or the whole thing if you wish and
then copy it back to a d81 that you create on your own. To copy CII.COM
from B: to A: just type "PIP A:CII.COM=B:CII.COM" [Return] and when you
are done if you want to erase CII.COM from B: just "ERA CII.COM" [return].

Not wishing to write a book on the subject, please review all the
documentation I have provided on all of this and review the samples etc.
for further details.

Have Fun!

Bill Buckels April 2008


