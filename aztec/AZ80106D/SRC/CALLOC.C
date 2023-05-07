/* Copyright (C) 1984 by Manx Software Systems */

char *calloc(nelem, size)
unsigned nelem, size;
{
	register unsigned i = nelem*size;
	register char *cp, *malloc();

	if ((cp = malloc(i)) != (char *)0)
		setmem(cp, i, 0);
	return cp;
}
