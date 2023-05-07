/* Copyright (C) 1981,1982 by Manx Software Systems */

puts(str)
register char *str;
{
	while (*str)
		if (putchar(*str++) == -1)
			return -1;
	return putchar('\n');
}
