/* Copyright (C) 1984 by Manx Software Systems */
#include "stdio.h"

FILE *
fdopen(fd,mode)
char *mode;
{
	register FILE *fp;
	FILE *newstream();

	if ((fp = newstream()) == NULL)
		return NULL;
	fp->_unit = fd;
	fp->_flags = _BUSY;
	return fp;
}
 
