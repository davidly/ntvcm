/* Copyright (C) 1983 by Manx Software Systems */
static char *scnstr;
static char quit;

sscanf(string, fmt, arg)
char *string, *fmt; int *arg;
{
	int sgetc();

	scnstr = string;
	quit = 0;
	return scanfmt(sgetc, fmt, &arg);
}

static
sgetc(what)
{
	if (what == 0) {
		if (*scnstr)
			return *scnstr++ & 255;
		quit = 1;
	} else {
		if (!quit)
			return *--scnstr & 255;
	}
	return -1;
}
