/* Copyright (C) 1982 by Manx Software Systems */
#include <ctype.h>

long
atol(cp)
register char *cp;
{
	long n;
	register sign;

	while (*cp == ' ' || *cp == '\t')
		++cp;
	sign = 0;
	if ( *cp == '-' ) {
		sign = 1;
		++cp;
	} else if ( *cp == '+' )
		++cp;

	for ( n = 0 ; isdigit(*cp) ; )
		n = n*10 + *cp++ - '0';
	return sign ? -n : n;
}
