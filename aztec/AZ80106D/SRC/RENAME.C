/* Copyright (C) 1983, 1984 by Manx Software Systems */
#include <errno.h>

rename(old, new)
char *old, *new;
{
	auto char buff[60];
	register int user;

	fcbinit(old,buff);
	fcbinit(new,buff+16);
	user = 0;
	if (bdos(15,buff+16) != 0xff) 
	{
		bdos(16,buff+16);
		errno = EEXIST;
		user = -1;
	} 
	else if (bdos(23,buff) == 0xff) {
		errno = ENOENT;
		user = -1;
	}
	return user;
}
