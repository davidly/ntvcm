putchar(c)
register char c;
{
	bios(4,c);
}
