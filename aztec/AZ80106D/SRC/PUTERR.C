/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

puterr(c)
{
	return aputc(c, stderr);
}
