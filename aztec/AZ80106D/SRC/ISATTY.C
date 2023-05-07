/* Copyright (C) 1983 by Manx Software Systems */
#include "io.h"
#include "errno.h"

isatty(fd)
{
	return chantab[fd].c_ioctl;
}

