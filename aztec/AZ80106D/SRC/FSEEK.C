/* Copyright (c) 1981, 1982 by Manx Software Systems */
#include "stdio.h"

fseek(fp,pos,mode)
register FILE *fp;
long pos;
{
	register int i;
	long curpos, lseek();

	fp->_flags &= ~_EOF;
	if (fp->_flags & _DIRTY) {
		if (flsh_(fp,-1))
			return EOF;
	} else if (mode == 1 && fp->_bp)
		pos -= fp->_bend - fp->_bp;
	fp->_bp = fp->_bend = NULL;
	if (lseek(fp->_unit, pos, mode) < 0)
		return EOF;
	return 0;
}

long ftell(fp)
register FILE *fp;
{
	long pos, lseek();

	pos = lseek(fp->_unit, 0L, 1);	/* find out where we are */
	if (fp->_flags & _DIRTY)
		pos += fp->_bp - fp->_buff;
	else if (fp->_bp)
		pos -= fp->_bend - fp->_bp;
	return pos;
}
