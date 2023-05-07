/* Copyright (C) 1982 by Manx Software Systems */

#include <errno.h>
#include "io.h"

close(fd)
{
	register struct channel *chp;
	extern int bdf_();

	if (fd < 0 || fd > MAXCHAN) {
		errno = EBADF;
		return -1;
	}
	chp = &chantab[fd];
	fd = (*chp->c_close)(chp->c_arg);
	chp->c_read = chp->c_write = chp->c_ioctl = chp->c_seek = 0;
	chp->c_close = bdf_;
	return fd;
}