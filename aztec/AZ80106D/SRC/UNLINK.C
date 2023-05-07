/* Copyright (C) 1983, 1984 by Manx Software Systems */
#include <errno.h>

unlink(name)
char *name;
{
	auto char delfcb[40];

	fcbinit(name,delfcb);
	if (bdos(19,delfcb) == 0xff) 
	{
		errno = ENOENT;
		return -1;
	}
	return 0;
}

