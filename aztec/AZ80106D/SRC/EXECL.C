/* Copyright (C) 1983, 1984 by Manx Software Systems */
#include "io.h"
#include <errno.h>

execl(path)
char *path;
{
	register char *cp, *xp;
	int user, ouser;
	auto struct fcb fcb;
	auto char loader[70];
	extern char ldr_[];

	if ((user = fcbinit(path, &fcb)) == -1) {
		errno = EINVAL;
		return -1;
	}
	ouser = bdos(GETUSR, 255);
	bdos(GETUSR, user);
	if (bdos(OPNFIL, &fcb) == 255) {
		errno = ENOENT;
		return -1;
	}
	fcb.f_cr = 0;

	fcbinit(0, 0x5c);
	fcbinit(0, 0x6c);
	cp = (char *)0x81;
	*(char *)0x80 = cp - (char *)0x81;
	movmem(ldr_, loader, sizeof loader);
	(*(int (*)())loader)(&fcb, ouser);
}