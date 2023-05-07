//
// $Id: misc.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// misc routines
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

#ifndef __MISC_H__
#define __MISC_H__ 1

#include <stdio.h>
#include "global.h"

extern bool exists_file(const char *filename);
	// exist a file with name FILENAME
extern bool regular_file(const char *filename);
	// test for regular file
extern long file_size(FILE *fp);
	// return length of file FP or -1

#endif /* __MISC_H__ */
