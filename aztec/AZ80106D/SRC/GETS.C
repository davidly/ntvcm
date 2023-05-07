/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

#undef getchar

char *gets(line)
char *line;
{
	register char *cp;
	register int i;

	cp = line;
	while ((i = getchar()) != EOF && i != '\n')
		*cp++ = i;
	*cp = 0;
	if (i == EOF && cp == line)
		return NULL;
	return line;
}
