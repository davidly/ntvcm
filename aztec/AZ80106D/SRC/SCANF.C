/* Copyright (C) 1982 by Manx Software Systems */
#include "stdio.h"

static int scnlast;

scanf(fmt, args)
char *fmt; int *args;
{
	int gchar();

	scnlast = 0;
	return scanfmt(gchar, fmt, &args);
}

static gchar(what)
{
	if (what == 0) {
		if (feof(stdin))
			scnlast = EOF;
		else
			scnlast = agetc(stdin);
	} else
		scnlast = ungetc(scnlast, stdin);
	return scnlast;
}

