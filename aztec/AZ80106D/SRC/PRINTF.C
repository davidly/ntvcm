/* Copyright (C) 1981,1982 by Manx Software Systems */

printf(fmt,args)
char *fmt; unsigned args;
{
	extern int putchar();

	format(putchar,fmt,&args);
}
