/* Copyright (c) 1981, 1982 by Manx Software Systems */
#include "stdio.h"

ungetc(c,ptr)
int c; register FILE *ptr;
{
	if (c == EOF || ptr->_bp <= ptr->_buff)
		return EOF;
	*--ptr->_bp = c;
	return c;
}

