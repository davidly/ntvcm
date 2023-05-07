/* Copyright (C) 1983, 1984 by Manx Software Systems */
#include "io.h"
#include <errno.h>
#include <fcntl.h>

int bdf_(), filerd(), tty_rd(), bdosrd();

int (*Rd_tab[])() = {
	bdf_, filerd, tty_rd, bdosrd,
};
extern int errno;

read(fd, buff, len)
char *buff;
{
	register struct channel *chp;

	chp = &chantab[fd];
	return (*Rd_tab[chp->c_read])(chp->c_arg, buff, len);
}

static
filerd(afp,buffer,len)
struct fcbtab *afp;
char *buffer; unsigned len;
{
	register unsigned l = 0;
	register struct fcbtab *fp;
	unsigned k,j;

	fp = afp;
	if (fp->offset) 
	{
		if ((l = 128 - fp->offset) > len)
			l = len;
		if (getsect(fp, buffer, l))
			return 0;
	}
	if (k = (len-l)/128)
		if ((j = blkrd(&fp->fcb, buffer+l, k)) != 0) 
			return (k-j)*128 + l;
	l += k*128;
	if (l < len)
		if (getsect(fp, buffer+l, len-l))
			return l;
	return len;
}

static
getsect(fp, buf, len)
register struct fcbtab *fp; char *buf; unsigned len;
{
	if (_find(fp))
		return -1;
	movmem(Wrkbuf+fp->offset, buf, len);
	if ((fp->offset = (fp->offset + len) & 127) == 0)
		++fp->fcb.f_record;
	return 0;
}

char _Eol = '\n';

tty_rd(x,buff,len)
char *buff;
{
	static char buffer[258];
	static int used;
	register int l;

	if (buffer[1] == 0) {
		buffer[0] = 255;
		buffer[1] = buffer[2] = 0;
		bdos(10,buffer);
		bdos(2,'\n');
		if (buffer[2] == 0x1a) {
			buffer[1] = 0;
			return 0;
		}
		buffer[++buffer[1] + 1] = _Eol;
		used = 2;
	}
	if ((l = buffer[1]) > len)
		l = len;
	movmem(buffer+used, buff, l);
	used += l;
	buffer[1] -= l;
	return l;
}

static
bdosrd(kind, buff, len)
register char *buff;
{
	register int count;

	for (count = 0 ; count < len ; ++count) {
		if ((*buff++ = bdos(kind)) == 0x1a)
			break;
	}
	return count;
}
