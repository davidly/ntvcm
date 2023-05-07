/* Copyright (C) 1982 by Manx Software Systems */
#include "stdio.h"

getc(ptr)
register FILE *ptr;
{
	register int len;

	if (ptr->_bp >= ptr->_bend) {
		if (ptr->_flags&(_EOF|_IOERR))
			return EOF;
		ptr->_flags &= ~_DIRTY;
		if (ptr->_buff == NULL)
			getbuff(ptr);
		if ((len = read(ptr->_unit,ptr->_buff,ptr->_buflen)) <= 0) {
			ptr->_flags |= len==0 ? _EOF : _IOERR;
			return EOF;
		}
		ptr->_bend = (ptr->_bp = ptr->_buff) + len;
	}
	return *ptr->_bp++ & 255;
}
