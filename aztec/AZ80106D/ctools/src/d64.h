//
// $Id: d64.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
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

#ifndef __D64_H__
#define __D64_H__ 1

#include "global.h"

#include <stdio.h>

typedef byte block[256];
	// sector on disk image

class d64 {
	private:
		FILE *image;
		bool _double_sided;
		bool _read_only;
		void seek(unsigned tr, unsigned sec);
	public:
		void open(const char filename[]);
			// open image file `filename'
		void close();
			// close image file
		void read(unsigned tr, unsigned sec, block sector);
			// read `sector' at `tr'/`sec'
		void write(unsigned tr, unsigned sec, const block sector);
			// write `sector' to `tr'/`sec'
		bool double_sided() const {return _double_sided;}
			// is image double sided?
		bool read_only() const {return _read_only;}
			// is image disk read only?
};

#endif /* __D64_H__ */
