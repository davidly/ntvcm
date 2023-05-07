/*
 * FIND is a program was writen to help maintain files on my hard disc.
 * This program should run on any CP/M 2.2 or higher revision machine.
 * It was developed using CP/M 3.0 and the AZTEC C compiler vers. 1.06B.
 *
 * To compile:
 *            cc -dTINY find.c
 *            as find.asm
 *            ln find.o t.lib c.lib
 *
 * As this program was a quick ditch effort, I welcome any and all
 * modifications that anyone makes to it.  For example, anbiguous(* , ?)
 * filenames are not extracted from the DMA address after the bdos
 * call to "search filename".  The program also does not allow the
 * user to search specific user areas, it assumes a scan of user 0
 * thru user 15.
 *
 * Have fun with it, but this program is not for resale in any form.
 *
 * Please send any ideas/changes/reactions to me in mail.  Do not post.
 *
 * Jeff Gibson                     UUCP: {cepu,ihnp4,noao,uiucdcs}!bradley!jmg
 * Bradley University              ARPA: cepu!bradley!jmg@UCLA-LOCUS
 * Peoria, IL 61625                PH: (309) 692-9069
 *
 */
#include "stdio.h"

/* Modifications (C) Copyright Bill Buckels 2008. All Rights Reserved. */
/* BB - true and false were not defined */
#define FALSE 0
#define TRUE 1


int  drive_range[17],
     DRIVES;

main(argc, argv)
int argc;
char *argv[];
{
  char  fn[36],
        drive_name;

  int   i,
        old_user,
        new_user,
        found,
        drive;


  if (--argc < 1)
   {
     printf("\nusage:  find -drives abcdefghijklmnop filename\n");
     bios(0);
   }

  DRIVES = found = FALSE;

  *++argv;
  if  (*argv[0] == '-')
   {
     check(*argv);
     com_line(*++argv);
   }
  else
    {
      *--argv;
      drive_range[0] = 0;
      drive_range[1] = -1;
    }
  fcbinit(*++argv, &fn);
  old_user = getusr();
  i = 0;
  putchar('\n');
  while (drive_range[i] != -1)
   {
      fn[0] = drive_range[i];
      for (new_user=0; new_user < 16; new_user++)
       {
          if  (bdos(11) == 1)                 /* check for key strike */
             if  (getchar() == 0x03)          /* abort on ^C */
                bios(0);
          setusr(new_user);
          if  (bdos(17, fn) != 0xff)
           {
             found = TRUE;
             if  (drive_range[i] == 0)
                drive_name = (char)(drive_range[i] + 65);
             else
                drive_name = (char)(drive_range[i] + 64);
             printf("%s: USER %d  DRIVE %c\n", *argv, new_user, drive_name);
           }
       }
      i++;
   }
 if  (!found)
    printf("%s: was not found\n", *argv);
 setusr(old_user);
 bios(0);
}

check(argv)
char argv[];
{
    if  (tolower(argv[1]) == 'd')
       DRIVES = TRUE;
    else
      {
        printf ("\nERROR in argument\n");
        bios(0);
      }
}


com_line(line)
char line[];
{
   int i, j;

   if  (DRIVES)
    {
      i = 0;
      j = 0;
      while  (tolower(line[i]) >= 'a' && tolower(line[i]) <= 'p')
       {
           drive_range[j] = (int)(tolower(line[i++]) - 0x60);
           j++;
       }
      drive_range[j] = -1;
    }
   else
     {
        printf("ERROR --- illegal drive specification.\n");
        bios(0);
     }
}

