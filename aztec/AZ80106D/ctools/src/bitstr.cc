//
// $Id: bitstr.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
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
// $Log: bitstr.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: bitstr.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include <limits.h>
#include <string.h>
#include "bitstr.h"


bitstr::bitstr(unsigned size)
{
	sz = size;
	len = (size+CHAR_BIT-1)/CHAR_BIT;
	if (len > 0) {
		s = new char[len];
		memset(s, 0, len);	
	} else {
		s = NULL;
	}
}

bitstr::~bitstr()
{
	if (len > 0) delete[] s;
	sz = len = 0;
}

void bitstr::resize(unsigned size)
{
	if (size == sz) {
		memset(s, 0, len);
		return;
	}
	if (len > 0) delete[] s;
	sz = size;
	len = (size+CHAR_BIT-1)/CHAR_BIT;
	if (len > 0) {
		s = new char[len];
		memset(s, 0, len);	
	} else {
		s = NULL;
	}
}

int bitstr::get(unsigned pos)
{
	if (pos >= sz) 
		return -1;
	return (s[pos/CHAR_BIT] & 1<<(pos%CHAR_BIT)) != 0;
}

void bitstr::put(unsigned pos, int value)
{
	if (pos >= sz) 
		return;
	if (value == 0) 
		s[pos/CHAR_BIT] &= ~(1<<(pos%CHAR_BIT));
	else
		s[pos/CHAR_BIT] |= 1<<(pos%CHAR_BIT);
}

unsigned bitstr::sum(void)
{
	unsigned i, u;
	
	u = 0;
	for(i=0; i<sz; i++)
		u += ((s[i/CHAR_BIT] & 1<<(i%CHAR_BIT)) != 0);
	return u;
}
