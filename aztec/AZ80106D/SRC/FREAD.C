/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

fread(buffer,size,number,stream)
register char *buffer; unsigned size; int number;
FILE *stream;
{
	int total;
	register int c,i;

	for ( total = 0 ; total < number ; ++total ) {
		for ( i = size ; i ; --i ) {
			if ( (c = getc(stream)) == EOF )
				return total;
			*buffer++ = c;
		}
	}
	return total;
}
