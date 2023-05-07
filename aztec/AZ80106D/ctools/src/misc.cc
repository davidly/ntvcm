//
// $Id: misc.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
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
// $Log: misc.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: misc.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif


#include <sys/stat.h>
#include "misc.h"


bool
exists_file(const char *filename)
{
        struct stat buf;

	return stat(filename, &buf) == 0;
}

bool
regular_file(const char *filename)
{
        struct stat buf;

	return stat(filename, &buf)==0 && S_ISREG(buf.st_mode);
}

long
file_size(FILE *fp)
{
        long pos, len;

        if ((pos = ftell(fp)) < 0) return -1;
        if (fseek(fp, 0, SEEK_END) < 0) return -1;
        if ((len = ftell(fp)) < 0) return -1;
        if (fseek(fp, pos, SEEK_SET) < 0) return -1;
        return len;
}
