//
// $Id: bios.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// bios interface (like CP/M BIOS)
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
// $Log: bios.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: bios.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include "err.h"
#include "bios.h"

static void
translate_null(unsigned nr, unsigned &tr, unsigned &sec)
{
	tr = 0;
	sec = nr;
}

static void
translate_c64(unsigned nr, unsigned &tr, unsigned &sec)
{
	tr = 0;
	sec = 0;
	if (nr >= 544) return;
	tr = nr/17 + 3;
	if (tr >= 18) tr++;
	sec = nr%17;
}

static void
translate_c128_ss(unsigned nr, unsigned &tr, unsigned &sec)
{
	static const struct {
		unsigned fst;	// 1st track of zone
		unsigned  bl;	// # of sectors in a tr
		unsigned  sz;	// # of sectors in zone
		unsigned sys;	// reserve sectors in zone
	} zone[4] = {
		{ 1, 21, 357, 2},
		{18, 19, 133, 1},
		{25, 18, 108, 0},
		{31, 17,  85, 0},
	};
	int i;

	tr = 0;
	sec = 0;
	if (nr >= 680) return;
	for (i=0; i<4; i++) {
		nr += zone[i].sys; 
		if (nr < zone[i].sz) {
			tr = zone[i].fst + nr/zone[i].bl;
			sec = (5*nr)%zone[i].bl;
			return;
		}
		nr -= zone[i].sz;
	}
}

static void
translate_c128_ds(unsigned nr, unsigned &tr, unsigned &sec)
{
	if (nr < 680) {
		translate_c128_ss(nr, tr, sec);
	} else {
		translate_c128_ss(nr-680, tr, sec);
		if (tr > 0) tr += 35;
	}
}

const bios_disk_types bios_disk_type[] = {
	{CPM_DISK_NULL,   {0, 0, 0,   0,  0, 0}, translate_null},
	{CPM_DISK_C64,    {4, 8, 1, 136, 64, 2}, translate_c64},
	{CPM_DISK_C128_SS,{4, 8, 1, 170, 64, 2}, translate_c128_ss},
	{CPM_DISK_C128_DS,{8,16, 2, 170,128, 2}, translate_c128_ds},
};


void
bios_disk::open(const char filename[])
{
	block boot;

	d64.open(filename);
	d64.read(1, 0, boot);
	if (boot[0] != 'C' || boot[1] != 'B' || boot[2] != 'M') {
		type = &bios_disk_type[CPM_DISK_C64];
	} else if (boot[255] != 0xFF) {
		type = &bios_disk_type[CPM_DISK_C128_SS];
	} else if (d64.double_sided()) {
		type = &bios_disk_type[CPM_DISK_C128_DS];
	} else {
		type = &bios_disk_type[CPM_DISK_NULL];
		errx("c128 double sided CP/M disk on a single sided disk");
	}
}

void
bios_disk::close()
{
	d64.close();
	type = &bios_disk_type[CPM_DISK_NULL];
}

void
bios_disk::read(unsigned nr, byte buffer[])
{
	unsigned unit, i;
        unsigned tr, sec;
	block *b = (block *)buffer;

	nr *= (unit = type->dpb.bpp);
	for(i=0; i<unit; i++) {
		(type->translate)(nr++, tr, sec);
		if (tr == 0)
			errx("illegal physical sector #%ui", nr-1);
		d64.read(tr, sec, *(b++));
	}
}

void
bios_disk::write(unsigned nr, const byte buffer[])
{
	unsigned unit, i;
        unsigned tr, sec;
	const block *b = (const block *)buffer;

	nr *= (unit = type->dpb.bpp);
	for(i=0; i<unit; i++) {
		(type->translate)(nr++, tr, sec);
		if (tr == 0)
			errx("illegal physical sector #%ui", nr-1);
		d64.write(tr, sec, *(b++));
	}
}
