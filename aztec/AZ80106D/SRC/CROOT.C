/* Copyright (C) 1981,1982,1984 by Manx Software Systems */

#include <errno.h>
#include <fcntl.h>
#include "io.h"

int bdf_(), ret_();

/*
 * channel table: relates fd's to devices
 */
struct channel chantab[] = {
	{ 2, 0, 1, 0, ret_, 2 },
	{ 0, 2, 1, 0, ret_, 2 },
	{ 0, 2, 1, 0, ret_, 2 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
	{ 0, 0, 0, 0, bdf_, 0 },
};

int (*cls_)() = ret_;

Croot()
{
	main();
	exit(0);
}

exit(code)
{
	register int fd;

	(*cls_)();
	for (fd = MAXCHAN; fd--;)
		close(fd);
	if (code && !(bdos(24)&1))
		unlink("A:$$$.SUB");
	_exit();
}

bdf_()
{
	errno = EBADF;
	return -1;
}

ret_()
{
	return 0;
}