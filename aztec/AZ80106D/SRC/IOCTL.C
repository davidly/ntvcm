/* Copyright (C) 1984 by Manx Software Systems */
#include "io.h"
#include "errno.h"
#include "sgtty.h"

#define TIME	10		/* number of iterations of raw_rd loop */
#define MIN		1		/* minimum number of chars returned from read */

extern int (*Rd_tab[])();
extern int (*Wrt_tab[])();

struct sgttyb Tty_ctl;
extern char _Eol;
extern int tty_rd();
static int raw_rd(), raw_wr();
static int rd_func, wrt_func;

ioctl(fd, cmd, arg)
struct sgttyb *arg;
{
	register struct channel *chp;

	chp = &chantab[fd];
	if (chp->c_ioctl == 0) {
		errno = ENOTTY;
		return -1;
	}
	switch (cmd) {
	case TIOCGETP:
		*arg = Tty_ctl;
		break;
	case TIOCSETP:
		Tty_ctl = *arg;
		Wrt_tab[2] = raw_wr;
		Rd_tab[2] = raw_rd;
		if (Tty_ctl.sg_flags&RAW) {
			rd_func =
			wrt_func = 6;
			_Eol = '\r';
			break;
		} else if (Tty_ctl.sg_flags&CBREAK) {
			rd_func = (Tty_ctl.sg_flags&ECHO) ? 1 : 6;
			wrt_func = 2;
		} else {
			Rd_tab[2] = tty_rd;
			wrt_func = 2;
		}
		if (Tty_ctl.sg_flags&CRMOD)
			_Eol = '\n';
		else
			_Eol = '\r';
	}
	return 0;
}

raw_rd(x, buff, len)
register char *buff;
{
	int c, i;
	register int count;

	for (count = 0 ; count < len ; ) {
		for (i = TIME ; i-- ; )
			if ((c = bdos(rd_func,0xff)) != 0)
				goto have_char;
		if (count < MIN)
			continue;
		break;
have_char:
		if (c == '\r')
			c = _Eol;
		*buff++ = c;
		++count;
	}
	return count;
}

raw_wr(kind, buff, len)
register char *buff;
{
	register int count;

	for (count = len ; count-- ; ) {
		if (*buff == '\n' && (Tty_ctl.sg_flags&CRMOD))
			bdos(wrt_func,'\r');
		bdos(wrt_func,*buff++);
	}
	return len;
}
