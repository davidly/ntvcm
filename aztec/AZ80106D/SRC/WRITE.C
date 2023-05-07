/* Copyright (C) 1983, 1984 by Manx Software Systems */
#include "io.h"
#include "errno.h"

int tty_wr(), bdoswr(), filewr(), bdf_();

int (*Wrt_tab[])() = {
	bdf_, filewr, bdoswr, bdoswr
};

write(fd, buff, len)
char *buff;
{
	register struct channel *chp;

	chp = &chantab[fd];
	return (*Wrt_tab[chp->c_write])(chp->c_arg, buff, len);
}

static
filewr(afp,buffer,len)
struct fcbtab *afp;
char *buffer; unsigned len;
{
	register unsigned l = 0;
	register struct fcbtab *fp;
	unsigned k,j;

	fp = afp;
	if (fp->offset) {
		if ((l = 128 - fp->offset) > len)
			l = len;
		if (putsect(fp, buffer, l)) {
			return -1;
		}
	}
	if (k = (len-l)/128)
		if ((j = blkwr(&fp->fcb, buffer+l, k)) != 0) {
			if ((l += (k-j)*128) == 0)
				return -1;
			else
				return l;
		}
	l += k*128;
	if (l < len)
		if (putsect(fp, buffer+l, len-l)) {
			return l;
		}
	return len;
}

static
putsect(fp, buf, len)
register struct fcbtab *fp; char *buf; unsigned len;
{
	if (_find(fp) < 0)
		return -1;
	movmem(buf, Wrkbuf+fp->offset, len);
	if ((errno = bdos(WRITRN, &fp->fcb)) != 0)
		return -1;
	if ((fp->offset = (fp->offset + len) & 127) == 0)
		++fp->fcb.f_record;
	return 0;
}

tty_wr(kind, buff, len)
register char *buff;
{
	register int count;

	for (count = len ; count-- ; ) {
		if (*buff == '\n')
			bdos(2,'\r');
		bdos(2,*buff++);
	}
	return len;
}

static
bdoswr(kind, buff, len)
register char *buff;
{
	register int count;

	for (count = len ; count-- ; )
		bdos(kind,*buff++);
	return len;
}