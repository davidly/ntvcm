/* Copyright (C) 1982 by Manx Software Systems */
#include "stdio.h"

getw(stream)
FILE *stream;
{
	register int x1,x2;

	if ((x1 = getc(stream)) == EOF || (x2 = getc(stream)) == EOF)
		return EOF;
	return (x2<<8) | x1;
}
