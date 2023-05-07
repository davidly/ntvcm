/* Copyright (C) 1981,1982,1983,1984 by Manx Software Systems */
#include "stdio.h"
#include "fcntl.h"
#include "errno.h"

extern int errno;

static struct modes {
	char fmode[3];
	int omode;
} modes[] = {
	"r",	O_RDONLY,
	"r+",	O_RDWR,
	"w",	(O_WRONLY|O_CREAT|O_TRUNC),
	"w+",	(O_RDWR|O_CREAT|O_TRUNC),
	"a",	(O_WRONLY|O_CREAT|O_APPEND),
	"a+",	(O_RDWR|O_CREAT|O_APPEND),
	"x",	(O_WRONLY|O_CREAT|O_EXCL),
	"x+",	(O_RDWR|O_CREAT|O_EXCL),
	"",		0,
};

FILE *
fopen(name,mode)
char *name,*mode;
{
	register FILE *fp;
	FILE *newstream(), *freopen();

	if ((fp = newstream()) == NULL)
		return NULL;
	return freopen(name, mode, fp);
}

FILE *
freopen(name, mode, fp)
char *name,*mode; FILE *fp;
{
	register struct modes *mp;
	register int fd;

	fclose(fp);

	for (mp = modes ; ; ++mp) {
		if (mp->fmode == 0) {
			errno = EINVAL;
			return NULL;
		}
		if (strcmp(mp->fmode, mode) == 0)
			break;
	}

/*
	Don't try to optimize the next 3 lines.  Since _unit is a char,
	assigning to it in the if statement will cause the -1 test to fail
	on unsigned char machines.
*/
	if ((fd = open(name, mp->omode)) == -1)
		return (NULL);
	fp->_unit = fd;
	fp->_flags = _BUSY;
	return fp;
}
 
