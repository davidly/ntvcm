/* textfile.c (C) Copyright 2007 Bill Buckels */

/* Textfile Program for the Commodore 64 */
/* Written in Aztec C */

/* Modified for CPM/80 */

/* simple program illustrates a strategy
for textfile I/O.

I could not open a textfile for
read/write or append on the C64.

One Aztec C manual for the Apple II
does say that seeking is not reliable
on some implementations of Aztec C
and perhaps this may extend to appending
to a file.

My workaround is to save the old file
to a BAK file, then truncate the
orginal file and write the contents of
the BAK file to the orginal file
followed by the new line.

At any rate if you can't get append
to work then here's another way to
achieve the same results, albeit
without the same efficiency of an
append.

*/

#include <stdio.h>

char *welcome1="Welcome to the Wonderfully";
char *welcome2="Ancient World of Aztec C!";
char *welcome3="--------------------------";
char name[128], buffer[128];

FILE *fp=NULL;
FILE *fp2=NULL;

#define TRUE 1
#define FALSE 0

int status = FALSE;

nocr(ptr)
char *ptr;
{
	int idx;

	for (idx=0;ptr[idx] != 0; idx++) {
	    if (ptr[idx] == '\n') {
		   ptr[idx] = 0;
		   break;
		}

	}

}

main()
{

	puts(welcome3);
	name[0] = 0;
	while (name[0] == 0) {
	  printf("Enter your name: ");
	  gets(name);
    }
    printf("Hello %s.\n",name);

	fp = fopen("TEXTFILE.TXT","r");
	if (fp == NULL) {
		status = FALSE;
		fp = fopen("TEXTFILE.TXT","w");
		fprintf(fp,"%s\n",name);
		fclose(fp);

	}
	else {
	  fp2 = fopen("TEXTFILE.BAK","w");
	  while (fgets(buffer,128,fp) != NULL) {
          nocr(buffer);
          fprintf(fp2,"%s\n",buffer);
          if (strcmp(name,buffer)==0){
			  status = TRUE;
		  }
	  }
	  fclose(fp);
	  fclose(fp2);

	  if (status == FALSE) {
		  fp = fopen("TEXTFILE.TXT","w");
		  fp2 = fopen("TEXTFILE.BAK","r");
		  while (fgets(buffer,128,fp2) != NULL) {
			  nocr(buffer);
			  fprintf(fp,"%s\n",buffer);

		  }
		  fprintf(fp,"%s\n",name);
		  fclose(fp);
		  fclose(fp2);
	  }

	}

	if (status == TRUE) {;
	   puts("We have met before.");
	}
	else {
	   puts("I have added you to my friends.");
	}

	puts(welcome1);
	puts(welcome2);
	puts(welcome3);

	exit(0);
}


