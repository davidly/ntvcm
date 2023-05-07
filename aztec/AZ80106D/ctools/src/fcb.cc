//
// $Id: fcb.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (c) 1996-1997  Jochen Metzinger
//
// handle file control blocks 
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
// $Log: fcb.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: fcb.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include <ctype.h>
#include <string.h>
#include "err.h"
#include "fcb.h"


byte default_user_array = 0;

//// class dir_name ///////////////////////////////////////////////////////

void
dir_name::parse(const char filename[], bool expand)
{
	const char *colon, *dot;
	int i;

	// default setting of dir_name
	dr = expand ? '?' : (default_user_array & 0x0F);
	memset(&f, ' ', 8);
	memset(&t, ' ', 3);
	// user array (<letter>:)
	colon = strchr(filename, ':');
	if (colon != NULL) {
		char user;
		if (colon != filename+1)
			errx("bad file name syntax");
		user = toupper(filename[0]);
		if (user == '?' || user == '*')
			dr = '?';
		else if ('A' <= user && user <= 'P')
			dr = user - 'A';
		else
			errx("bad file name syntax");
		filename = colon + 1;
	}
	if (expand && *filename == '\0') {
		memset(&f, '?', 8);
		memset(&t, '?', 3);
		return;
	}
	// name part
	dot = strrchr(filename, '.');
	if (dot == NULL)
		dot = strrchr(filename, '\0');
	for (i=0; i<8 && filename<dot; i++)
		if (*filename == '*') {
			while (i < 8) f[i++] = '?';
			break;
		} else {
			f[i] = toupper(*filename) & ~0x80;
			filename++;
		}
	// extension part
	if (*dot == '\0') {
		if (expand) memset(&t, '?', 3);
		return;
	}
	filename = dot + 1;
	for (i=0; i<3 && *filename != '\0'; i++) 
		if (*filename == '*') {
			while (i < 3) t[i++] = '?';
			return;
		} else {
			t[i] = toupper(*filename) & ~0x80;
			filename++;
		}
}

char *
dir_name::str() const
{
	static char result[CPM_MAX_FNAME];
	int i;
	
	// user array (<letter>:)
	if (dr < 16)
		result[0] = 'A' + dr;
	else if (dr == '?')
		result[0] = '?';
	else if (dr == 0xE5)
		result[0] = 'X';
	else
		result[0] = '&';
	result[1] = ':';
	// name part
	for (i=0; i<8; i++)
		result[2+i] = f[i] & ~0x80;
	result[10] = '.';
	// extension part
	for (i=0; i<3; i++)
		result[11+i] = t[i] & ~0x80;
	result[14] = '\0';
	return result;
}

char *
dir_name::filename() const
{
	static char result[CPM_MAX_FNAME];
	int i, j;
	char c;

        j = 0;
        for (i=0; i<8; i++) {
                c = f[i] & ~0x80;
                if (c == ' ')
                        break;
                result[j++] = tolower(c);
        }
        result[j++] = '.';
        for (i=0; i<3; i++) {
                c = t[i] & ~0x80;
                if (c == ' ')
                        break;
                result[j++] = tolower(c);
        }
        if (result[j-1] == '.')
                j--;
        result[j] = '\0';
        return result;
}

unsigned
dir_name::flags() const
{
	unsigned result;
	int i;

	result = 0;
	for (i=0; i<8; i++) {
		result <<= 1;
		if ((f[i] & 0x80) != 0) result |= 1;
	}
	for (i=0; i<3; i++) {
		result <<= 1;
		if ((t[i] & 0x80) != 0) result |= 1;
	}
	return result;
}

bool
dir_name::operator==(const dir_name &dn) const
{
	int i;

	if (dr != dn.dr)
		return FALSE;
	for (i=0; i<8; i++) 
		if (((f[i] ^ dn.f[i]) & ~0x80) != 0)
			return FALSE;
	for (i=0; i<3; i++) 
		if (((t[i] ^ dn.t[i]) & ~0x80) != 0)
			return FALSE;
	return TRUE;
}

bool
dir_name::match(const dir_name &pattern) const
{
	int i;

	if (dr != pattern.dr && pattern.dr != '?')
		return FALSE;
	for (i=0; i<8; i++) 
		if (((f[i] ^ pattern.f[i]) & ~0x80) != 0 && pattern.f[i] != '?')
			return FALSE;
	for (i=0; i<3; i++) 
		if (((t[i] ^ pattern.t[i]) & ~0x80) != 0 && pattern.t[i] != '?')
			return FALSE;
	return TRUE;
}

void
dir_name::rename(const dir_name &new_pattern)
{
	int i;

	if (new_pattern.dr != '?')
		dr = new_pattern.dr & 0x0F;
	for (i=0; i<8; i++)
		if (new_pattern.f[i] != '?')
			f[i] = new_pattern.f[i] & ~0x80;
	for (i=0; i<3; i++)
		if (new_pattern.t[i] != '?')
			t[i] = new_pattern.t[i] & ~0x80;
}

//// class cpm_fcb ////////////////////////////////////////////////////////

int
cpm_fcb::sectors()
{
	int i, sum;

	sum = 0;
	for (i=0; i<16; i++)
		if (d[i] != 0)
			sum++;
	return sum;
}

int
cpm_fcb::records()
{
	if (rc >= 0x80) 
		return 0x80*(ex+1);
	else
		return 0x80*ex + rc;
}
