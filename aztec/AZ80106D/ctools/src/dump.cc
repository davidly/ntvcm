//
// $Id: dump.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// output memory dumps
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
// $Log: dump.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: dump.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */

#include <ctype.h>
#include <stdio.h>
#include "dump.h"

void
dump(void *mem, long count, long offset, FILE *fp)
{
	unsigned char *m = (unsigned char *)mem;
	char line[0x10];
	int  i;

	while (count >= 0x10) {
		fprintf(fp, "%04lx - ", offset);
		for (i=0; i<0x10; i++) {
			fprintf(fp, "%02x ", m[i]);
			line[i] = isprint(m[i]) ? m[i] : '.';
			if (i == 0x07) fputs(". ", fp);
		}
		fprintf(fp, " %16.16s\n", line);
		m      += 0x10;
		offset += 0x10;
		count  -= 0x10;
	}
	if (count > 0) {
		fprintf(fp, "%04lx - ", offset);
		for (i=0; i<0x10; i++) {
			if (i < count) {
				fprintf(fp, "%02x ", m[i]);
				line[i] = isprint(m[i]) ? m[i] : '.';
			} else {
				fputs("   ", fp);
				line[i] = ' ';
			}
			if (i == 0x07) fputs(". ", fp);
		}
		fprintf(fp, " %16.16s\n", line);
	}
}
