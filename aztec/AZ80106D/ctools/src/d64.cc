//
// $Id: d64.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// access (c1541 and c1571) disk images
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
// $Log: d64.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: d64.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include "misc.h"
#include "err.h"
#include "d64.h"


#define	DISK_SIZE	683L	// blocks on a single sided disk

void d64::open(const char filename[])
{
	block bam;
	long fsize;

	image = fopen(filename, "rb+");
	_read_only = (image == NULL);
	if (_read_only) {
		image = fopen(filename, "rb");
		if (image == NULL)
			err("%s", filename);
	}
	fsize = file_size(image);
	if (fsize < 0)
		err("%s", filename);
	_double_sided = (fsize == 2*DISK_SIZE*sizeof(block));
	if (!_double_sided && fsize != DISK_SIZE*sizeof(block)) {
		fclose(image);
		errx("neither c1541 nor c1571 image [size]");
	}
	read(18, 0, bam);
	if (bam[2] != 0x41 || bam[165] != 0x32 || bam[166] != 0x41) {
		fclose(image);
		errx("neither c1541 nor c1571 image [BAM]");
	}
}

void d64::close()
{
	int status;

	status = fclose(image);
	image = NULL;
	if (status < 0)
		err();
}

void d64::read(unsigned tr, unsigned sec, block sector)
{
	seek(tr,sec);
	if (fread(sector, sizeof(block), 1, image) < 1)
		err();
}

void d64::write(unsigned tr, unsigned sec, const block sector)
{
	seek(tr,sec);
	if (fwrite(sector, sizeof(block), 1, image) < 1)
		err();
}

static long
position(unsigned tr, unsigned sec, bool double_sided)
{
	static const unsigned block_per_track[] = {
		 0, 21, 21, 21, 21, 21, 21, 21, 21, 21, // 00 -- 09
		21, 21, 21, 21, 21, 21, 21, 21, 19, 19, // 10 -- 19
		19, 19, 19, 19, 19, 18, 18, 18, 18, 18, // 20 -- 29
		18, 17, 17, 17, 17, 17,  0,             // 30 -- 35
	};
	long tr_offs = 0;
	unsigned i;

	if (tr > 35) {
		if (!double_sided || tr > 70) return -1;
		tr_offs = DISK_SIZE;
		tr -= 35;
	}
	if (sec >= block_per_track[tr])
		return -1;
	for (i=1; i<tr; i++)
		tr_offs += block_per_track[i];
	return tr_offs + sec;
}

void
d64::seek(unsigned tr, unsigned sec)
{
	long pos;

	if ((pos = position(tr, sec, _double_sided)) < 0)
		errx("illegal track and sector %i %i", tr, sec);
	if ((fseek(image, pos*sizeof(block), SEEK_SET)) < 0)
		err();
}
