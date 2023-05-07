/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

putw(w,stream)
register unsigned w;
FILE *stream;
{
	if ( putc(w,stream) < 0 ) 
		return EOF;
	else if ( putc((w>>8),stream) < 0 )
		return EOF;
	return w;
}
