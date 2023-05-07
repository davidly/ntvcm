//
// $Id: bitstr.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// dynamic array of bits
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

#ifndef __BITMAP_H__
#define __BITMAP_H__ 1

class bitstr {
	private:
		unsigned len;		// number of chars in s
		unsigned sz;		// allocated slots
		char *s;		// bits string
	public:
		bitstr(unsigned size = 0);
			// create array of bits with SIZE bits
		~bitstr();
			// destructor */
		void resize(unsigned size);
			// re-create array of bits with SIZE bits
		int get(unsigned pos);
			// get bit value at POS
		void put(unsigned pos, int value);
			// put VALUE at POS
		unsigned sum(void);
			// get sum of all bits
};

#endif /* __BITMAP_H__ */
