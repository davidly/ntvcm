//
// $Id: dir.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// the directory
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

#ifndef __DIR_H__
#define __DIR_H__ 1

#include "global.h"
#include "bitstr.h"
#include "bios.h"
#include "fcb.h"

class cpm_dir;

struct dir_entry {
	dir_name name;
	unsigned pos0, posn;
	unsigned sectors;
	unsigned records;
};

class dir_rider {
	private:
		cpm_dir *dir;
			// rider of this directory
		unsigned pos;
			// search position
		dir_name pat;
			// the search pattern
	public:
		dir_rider(cpm_dir *d, const dir_name &pattern);
			// constructor for cpm_dir::open()
		~dir_rider();
			// destructor for cpm_dir::close()
		const dir_entry *read(bool removed = FALSE);
			// get next directory entry matching pat
};

class cpm_dir {
	private:
		bios_disk *disk;
			// dir of this disk 
		cpm_fcb *buffer;
			// directory buffer
		bitstr al;
			// allocation vector
	protected:
		void set_al();
			// setup AL
		void check(unsigned pos);
			// check directory entry `pos'
		void check();
			// check directory
		unsigned collect(const dir_name &dn);
			// collect directory for add()
		void initdir();
			// remove SFCB, XFCB and directory labels
	public:
		cpm_dir();
			// constructor 
		~cpm_dir();
			// destructor
		void load(bios_disk *disk, bool init=FALSE);
			// load buffer from dsk, using initdir()
		void save();
			// save buffer
		bios_disk &get_disk() {return *disk;}
			// get disk
		unsigned sector_size() {return disk->dpb().bl;}
			// size of a sector in records
		unsigned size() {return disk->dpb().dir;}
			// size of directory
		unsigned free();
			// get number of free records
		dir_rider *open(const dir_name &name);
			// open a dir_rider for this name
		void close(dir_rider *dr);
			// close a dir_rider
		const cpm_fcb &operator[](unsigned i) {return buffer[i];}
			// get i-th fcb in directory (for dir_rider)
		bool exists(const dir_name &pattern);
			// exists file
		unsigned allocate_sector();
			// return the number of an allocated sector
		void erase(const dir_entry &entry);
		bool erase(const dir_name &pattern);
			// erase files
		bool unerase();
		bool unerase(const dir_entry &entry);
			// try to un-erase files
		bool rename(const dir_name &old_p, const dir_name &new_p);
                        // rename files
		dir_entry &add(const dir_name &dn);
			// add dir entry
		void append(dir_entry &entry, const cpm_fcb &fcb);
			// append FCB to ENTRY
};

#endif /* __DIR_H__ */
