/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

fputs(s,fp)
register char *s;
FILE *fp;
{
	while ( *s )
		if (aputc(*s++,fp) == EOF)
			return(EOF);
	return 0;
}
