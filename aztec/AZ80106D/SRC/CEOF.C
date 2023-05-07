/* Copyright (C) 1984 by Manx Software Systems */
#include "io.h"
#include "errno.h"

_Ceof(fp)
register struct fcbtab *fp;
{
	register char *cp;

	bdos(FILSIZ, &fp->fcb);
	if (fp->fcb.f_record == 0) {
		fp->offset = 0;
		return 0;
	}
	--fp->fcb.f_record;			/* backup to last record */
	if (_find(fp))
		return -1;

	for (cp = Wrkbuf+128 ; cp > Wrkbuf ; )
		if (*--cp != 0x1a) {
			++cp;
			break;
		}
	if ((fp->offset = cp-Wrkbuf) == 128) {
		++fp->fcb.f_record;
		fp->offset = 0;
	}
	return 0;
}
