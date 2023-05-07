//
// $Id: file.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// file usage
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

#ifndef __FILE_H__
#define __FILE_H__ 1

#include "bios.h"
#include "dir.h"

typedef byte cpm_rec[CPM_RECORD_SIZE];

class cpm_file {
	private:
		enum {F_CLOSED, F_READ, F_WRITE} state;
			// state of file
		bios_disk *dsk;
			// working on this disk
		const struct dpb *dpb;
			// its geometry
		cpm_dir *dr;
			// in this directory
		dir_entry d_ent;
			// on this entry
		unsigned pos;
			// at this position
		cpm_fcb fcb;
			// active fcb
		unsigned rec_nr, rec_lim;
			// record position and limit in fcb
		unsigned sector;
			// work in this sector
		byte *buffer;
			// buffer for sector
	public:
		void create(cpm_dir &dir, const dir_name &dn);
			// create a file
		void open(cpm_dir &dir, const dir_entry &entry);
			// open a file
		bool eof();
			// test end of file
		void read(byte *rec);
			// read a CP/M record
		void write(const byte *rec);
			// write a CP/M record
		void close();
			// close file
};

#endif /* __FILE_H__ */
