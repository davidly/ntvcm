//
// $Id: err.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
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

#ifndef __ERR_H__
#define __ERR_H__ 1

#include <stdlib.h>
#include <setjmp.h>

extern const char *short_usage;
	// short description of the program usage
extern const char *source;
	// active part of the program

extern void err_init();
	// setup error handling of short memory
extern void warn(const char *fmt = NULL, ...);
	// warning with strerror()
extern void warnx(const char *fmt, ...);
	// warning w/o strerror()
extern void err(const char *fmt = NULL, ...);
	// error with strerror() -> throw()
extern void errx(const char *fmt = NULL, ...);
	// error w/o strerror() -> throw()
#define TODO() errx("TODO in %s at line %u", __FILE__, __LINE__)

#define TRY     if (setjmp(*__get_jmp_buf()) == 0){
#define CATCH() __unget_jmp_buf();} else
	// exception handling
extern jmp_buf *__get_jmp_buf();
extern void __unget_jmp_buf();
	// used by TRY/CATCH

#endif /* __ERR_H__ */
