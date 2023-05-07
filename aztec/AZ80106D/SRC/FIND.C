/* Copyright (C) 1984 by Manx Software Systems */
#include "io.h"

static struct fcbtab *Wfp;
static unsigned Wsct;

_zap()			/* invalidate work buffer */
{
	Wfp = 0;
}

_find(fp)
register struct fcbtab *fp;
{
	extern int errno;

	bdos(SETDMA, Wrkbuf);
	if (Wfp != fp || fp->fcb.f_record != Wsct) {
		if ((errno = bdos(READRN, &fp->fcb)) == 1 || errno == 4) {
			errno = 0;
			setmem(Wrkbuf, 128, 0x1a);
			Wfp = 0;
			return 1;
		} else if (errno)
			return -1;
		Wfp = fp;
		Wsct = fp->fcb.f_record;
	}
	return 0;
}

