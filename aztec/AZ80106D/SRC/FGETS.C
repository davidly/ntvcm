/* Copyright (C) 1981,1982 by Manx Software Systems */
#include "stdio.h"

char *fgets(s, n, fp)
char *s; FILE *fp;
{
	register c;
	register char *cp;

	cp = s;
	while (--n > 0 && (c = agetc(fp)) != EOF) {
		*cp++ = c;
		if (c == '\n')
			break;
	}
	*cp = 0;
	if (c == EOF && cp == s)
		return NULL;
	return(s);
}
