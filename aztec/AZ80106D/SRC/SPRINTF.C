/* Copyright (C) 1982 by Manx Software Systems */
static char *buff;

sprintf(str,fmt,args)
char *str, *fmt; unsigned args;
{
	int spsub();
	register int i;

	buff = str;
	i = format(spsub,fmt,&args);
	*buff = 0;
	return i;
}

static
spsub(c)
{
	return (*buff++ = c)&0xff;
}

