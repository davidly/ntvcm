//
// $Id: file.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
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
// $Log: file.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: file.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include <string.h>
#include "err.h"
#include "file.h"


void
cpm_file::open(cpm_dir &dir, const dir_entry &entry)
{
	dsk = &dir.get_disk();
	dpb = &dsk->dpb();
	dr = &dir;
	d_ent = entry;
	pos = d_ent.pos0;
	fcb = (*dr)[pos];
	rec_nr  = 0;
	rec_lim = fcb.rc >= 0x80
			? 0x80*dpb->ex
			: 0x80*(fcb.ex % dpb->ex) + fcb.rc;
	sector = 0;
	buffer = new byte[CPM_RECORD_SIZE*dpb->bl];
	state = F_READ;
}

void
cpm_file::create(cpm_dir &dir, const dir_name &dn)
{
	dsk = &dir.get_disk();
	if (dsk->read_only())
		errx("read only disk");
	dpb = &dsk->dpb();
	dr = &dir;
	d_ent = dr->add(dn);
	pos = d_ent.pos0;
	fcb = (*dr)[pos];
	rec_nr = 0;
	rec_lim = 0x80*dpb->ex;
	sector = 0;
	buffer = new byte[CPM_RECORD_SIZE*dpb->bl];
	state = F_WRITE;
}

void
cpm_file::close()
{
	switch (state) {
		case F_READ:
			break;
		case F_WRITE:
			// save sector buffer
			if (rec_nr % dpb->bl != 0)
				dsk->write(sector, buffer);
			// save used fcb
			if (rec_nr > 0) {
				if (rec_nr < rec_lim) {
					fcb.rc  = rec_nr % 0x80;
					fcb.ex += rec_nr / 0x80;
				} else {
					fcb.rc = 0x80;
					fcb.ex += dpb->ex - 1;
				}
				dr->append(d_ent, fcb);
			}
			// save changed directory
			dr->save();
			break;
		default:
			errx("INTERN [cpm_file::close: no opened file]");
	}
	delete[] buffer;
	state = F_CLOSED;
}

bool
cpm_file::eof()
{
	switch (state) { 
		case F_READ:
			return rec_nr >= rec_lim;
		case F_WRITE:
			return TRUE;
		default:
			errx("INTERN [cpm_file::eof: no opened file]");
	}
	return FALSE;
}

void
cpm_file::read(byte *rec)
{
	// get buffer
	if (rec_nr % dpb->bl == 0 ) {
		sector = fcb.d[rec_nr / dpb->bl];
		if (sector == 0)
			memset(buffer, 'Z'-'@', CPM_RECORD_SIZE*dpb->bl);
		else
			dsk->read(sector, buffer);
	}
	// transfer 
	memcpy(rec, &buffer[(rec_nr++)%(dpb->bl)*CPM_RECORD_SIZE],
		CPM_RECORD_SIZE);
	// get fcb
	if (rec_nr >= rec_lim && fcb.rc >= 0x80 ) {
		if (pos >= d_ent.posn)
			return;
		fcb = (*dr)[++pos];
		rec_nr  = 0;
		rec_lim = fcb.rc >= 0x80
				? 0x80*dpb->ex
				: 0x80*(fcb.ex % dpb->ex) + fcb.rc;
	}
}

void
cpm_file::write(const byte *rec)
{
	// reserve sector
	if (rec_nr % dpb->bl == 0) {
		sector = dr->allocate_sector();
		if (sector == 0)
			errx("disk full");
		memset(buffer, 'Z'-'@', CPM_RECORD_SIZE*dpb->bl);
		fcb.d[rec_nr / dpb->bl] = sector;
	}
	// transfer
	memcpy(&buffer[(rec_nr++)%(dpb->bl)*CPM_RECORD_SIZE], rec,
		CPM_RECORD_SIZE);
	// put_buffer
	if (rec_nr % dpb->bl == 0) 
		dsk->write(sector, buffer);
	// put fcb
	if (rec_nr == rec_lim) {
		fcb.rc = 0x80;
		fcb.ex = fcb.ex + dpb->ex - 1;
		dr->append(d_ent, fcb);
		fcb.rc = 0;
		fcb.ex++;
		rec_nr = 0;
		memset(fcb.d, 0, sizeof(fcb.d));
	}
}
