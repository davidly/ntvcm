/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

agetc(ptr)
register FILE *ptr;
{
	register int c;

top:
	if ((c = getc(ptr)) != EOF) {
		switch (c &= 127) {
		case 0x1a:
			ptr->_flags |= _EOF;
			return EOF;
		case '\r':
		case 0:
			goto top;
		}
	}
	return c;
}

