/* Copyright (C) 1982 by Manx Software Systems */
#include "stdio.h"

static int scnlast;
static FILE *scnfp;

fscanf(fp, fmt, args)
FILE *fp; char *fmt; int *args;
{
	int gchar();

	scnfp = fp;
	scnlast = 0;
	return scanfmt(gchar, fmt, &args);
}

static gchar(what)
{
	if (what == 0) {
		if (feof(scnfp))
			scnlast = EOF;
		else
			scnlast = agetc(scnfp);
	} else
		scnlast = ungetc(scnlast, scnfp);
	return scnlast;
}

