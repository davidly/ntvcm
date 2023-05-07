//
// $Id: err.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// handle errors
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
// $Log: err.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: err.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */

#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <new.h>
#include "err.h"

// test of environment
#ifdef unix
#ifndef linux
#error unknown unix (known linux)
#endif
// unix ...
#elif !defined(MSDOS)
#error unknown operation system (known "unix" and "MSDOS")
#endif

static void THROW(int status);

const char *short_usage = NULL;
const char *source      = NULL;

static void err_new_handler()
{
	warnx("out of memory");
	exit(EXIT_FAILURE);
}
	
void
err_init()
{
	(void)set_new_handler(&err_new_handler);
}

static void
vwarn(const char *fmt, va_list ap)
{
	int save_errno = errno;

	if (source != NULL)
		fprintf(stderr, "%s: ", source);
	if (fmt != NULL) {
		vfprintf(stderr, fmt, ap);
		 fprintf(stderr, ": ");
	}
	fprintf(stderr, "%s\n", strerror(save_errno));
}

static void
vwarnx(const char *fmt, va_list ap)
{
	if (source != NULL)
		fprintf(stderr, "%s: ", source);
	if (fmt != NULL) {
		vfprintf(stderr, fmt, ap);
	}
	fprintf(stderr, "\n");
}

void
warn(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vwarn(fmt, ap);
	va_end(ap);
}

void
warnx(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vwarnx(fmt, ap);
	va_end(ap);
}

void
err(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vwarn(fmt, ap);
	va_end(ap);
	THROW(EXIT_FAILURE);
}

void
errx(const char *fmt, ...)
{
	va_list ap;

	if (fmt != NULL) {
		va_start(ap, fmt);
		vwarnx(fmt, ap);
		va_end(ap);
	}
	THROW(EXIT_FAILURE);
}


#define NR_BUFFER 1
static jmp_buf buffer[NR_BUFFER];
static int buf_pos = 0;

jmp_buf *
__get_jmp_buf()
{
	if (buf_pos >= NR_BUFFER) {
		buf_pos = 0;
		errx("too few error TRY buffer");
	}
	return &buffer[buf_pos++];
}

void
__unget_jmp_buf()
{
	if (--buf_pos < 0) {
		buf_pos = 0;
		errx("too less error TRY buffer");
	}
}

void
THROW(int status)
{
	if (buf_pos == 0) {
		if (short_usage != NULL) 
			fprintf(stderr, "usage: %s\n", short_usage);
		exit(status);
	}
	longjmp(buffer[--buf_pos], 1);
}
