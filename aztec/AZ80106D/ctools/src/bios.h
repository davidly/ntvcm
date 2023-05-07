//
// $Id: bios.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
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

#ifndef __BIOS_H__
#define __BIOS_H__ 1

#include "global.h"
#include "d64.h"

#define CPM_RECORD_SIZE		(128)	// size of a cpm record

struct dpb {
	unsigned bpp;  // physical sectors in an logical sector
	unsigned  bl;  // logical  sector size in records
	unsigned  ex;  // extents per directory entry
	unsigned  ds;  // number of logical sectors
	unsigned dir;  // dir entry number
	unsigned  ph;  // physical sector size in records
};

#define CPM_DISK_NULL		(0)	// no disk
#define CPM_DISK_C64		(1)	// c64 cpm disk
#define CPM_DISK_C128_SS	(2)	// c128 single sided disk
#define CPM_DISK_C128_DS	(3)	// c128 double sided disk

struct bios_disk_types {
        int magic;
                // CPM_DISK_...
        struct dpb dpb;
                // disk parameter block
        void (*translate)(unsigned, unsigned&, unsigned&);
                // translate physical sector number in d64 location
};

class bios_disk {
	private:
		class d64 d64;
			// at the moment only D64 images
	protected:
		const struct bios_disk_types *type;
			// format type
	public:
		void open(const char filename[]);
			// select device
		void close();
			// unselect device
		void read(unsigned nr, byte buffer[]);
			// read a logical sector
		void write(unsigned nr, const byte buffer[]);
			// write a logical sector
		const struct dpb &dpb() const {return type->dpb;}
			// get reference of the dpb of this disk
		bool read_only() const {return d64.read_only();}
                        // test for read only disk
};

#endif /* __BIOS_H__ */
