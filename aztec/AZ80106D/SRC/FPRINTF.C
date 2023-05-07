/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

static FILE *Stream;

fprintf(stream,fmt,args)
FILE *stream; char *fmt; unsigned args;
{
	int fpsub();

	Stream = stream;
	return format(fpsub,fmt,&args);
}

static
fpsub(c)
{
	return aputc(c,Stream);
}
