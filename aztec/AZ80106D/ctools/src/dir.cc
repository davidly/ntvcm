//
// $Id: dir.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
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
// $Log: dir.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: dir.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include <string.h>
#include "err.h"
#include "dir.h"


//// class dir_rider //////////////////////////////////////////////////////

dir_rider::dir_rider(cpm_dir *d, const dir_name &pattern)
{
	dir = d;
	pos = 0;
	pat = pattern;
}

dir_rider::~dir_rider()
{
}

const dir_entry *
dir_rider::read(bool removed)
{
	static dir_entry result;
	cpm_fcb fcb; 

	while (pos < dir->size()) {
		fcb = (*dir)[pos++];
		if ((fcb.dr != 0xE5 || removed) && fcb.match(pat)) {
			result.name = fcb;
			result.pos0 = pos-1;
			result.sectors = 0;
			do {
				result.sectors += fcb.sectors();
				result.records = fcb.records();
				fcb = (*dir)[pos++];
			} while (fcb.match(result.name) && pos <= dir->size());
			pos--;
			result.posn = pos-1;
			return &result;
		}
	}
	return NULL;
}

//// class cpm_dir ////////////////////////////////////////////////////////

cpm_dir::cpm_dir()
{
	disk = NULL;
	buffer = NULL;
}

cpm_dir::~cpm_dir()
{
	disk = NULL;
	if (buffer != NULL) delete[] buffer;
	buffer = NULL;
}

void
cpm_dir::set_al()
{
	unsigned pos, i, s;
	unsigned ds = disk->dpb().ds;
	unsigned sdir = size()/disk->dpb().bl/(CPM_RECORD_SIZE/sizeof(cpm_fcb));

	// new bitstr
	al.resize(ds);
	// reserve directory
	for (s=0; s<sdir; s++) {
		al.put(s, TRUE);
	}
	// reserve not deleted files
	for (pos=0; pos<size(); pos++) {
		if (buffer[pos].dr > 0x0F)
			continue;
		for (i=0; i<16; i++) {
			s= buffer[pos].d[i];
			if (s == 0)
				continue;
			if (s >= ds)
				errx("directory corrupted " \
					"[illegal sector at #%i]", pos);
			if (al.get(s))
				warnx("duplicate use of sector #%i", s);
			al.put(s, TRUE);
		}
	}
}

void 
cpm_dir::check(unsigned pos)
{
	int i, diff_ex;
	unsigned dpb_ex = disk->dpb().ex;
	unsigned dpb_ds = disk->dpb().ds;
	
	if (buffer[pos].dr == 0xE5)
		return;
	if (buffer[pos].dr > 0x22)
		errx("directory corrupted [no user array at #%i]",pos);
	if (buffer[pos].dr == 0x21) {
		if (pos%4 == 3) errx("directory corrupted [SFCB]");
		errx("directory corrupted [no user array at #%i]",pos);
	}
	if (buffer[pos].dr == 0x20)
		errx("directory corrupted [directory label]");
	if (buffer[pos].dr > 0x0F)
		errx("directory corrupted [FXCB]");
	if (buffer[pos].rc >= 0x80 && (buffer[pos].ex+1)%(dpb_ex) != 0)
		errx("directory corrupted [rc=128 and bad ex at #%i]",pos);
	if (buffer[pos].ex >= dpb_ex) {
		if (buffer[pos-1] != buffer[pos])
			errx("directory corrupted [ex>0 at #%i]",pos);
		if (buffer[pos-1].rc < 0x80)
			errx("directory corrupted [rc<128 at #%i]",pos-1);
		diff_ex = buffer[pos].ex - (buffer[pos-1].ex + 1);
		if (diff_ex < 0 || dpb_ex <= (unsigned)diff_ex)
			errx("directory corrupted [ex at #%i]",pos);
	}
	for (i=0; i<16; i++)
		if (buffer[pos].d[i] >= dpb_ds)
			errx("directory corrupted [illegal sector at #%u]",pos);
}

void
cpm_dir::check()
{
	unsigned pos;

	for (pos=0; pos < size(); pos++) 
		check(pos);
}

void
cpm_dir::initdir()
{
	unsigned src, dst, pos;

	// move used entries
	for(src=dst=0; src<size(); src++) {
		if (buffer[src].dr == 0xE5)
			continue;	// unused entries
		if (0x10 <= buffer[src].dr && buffer[src].dr <= 0x21)
			continue;	// XFCB and dir labels and SFCB
		buffer[dst++] = buffer[src];
	}
	// clean bottom array
	for (pos=dst; pos<size(); pos++) 
		memset(&buffer[pos], 0xE5, sizeof(cpm_fcb));
	return;
}

void
cpm_dir::load(bios_disk *disk, bool init)
{
	unsigned ndir, sdir, i, pos;

	// setup vars
	if (buffer != NULL) delete[] buffer;
	this->disk = disk;
	// get geometry
	ndir = disk->dpb().dir;
	sdir = (CPM_RECORD_SIZE/sizeof(cpm_fcb))*(disk->dpb().bl);
	// get data in a new buffer
	this->buffer = new cpm_fcb[ndir];
	for (i=pos=0; pos<ndir; pos+=sdir, i++)
		disk->read(i, (byte *)&buffer[pos]);
	// check all
	if (init)
		initdir();
	check();
	set_al();
}

void
cpm_dir::save()
{
	unsigned ndir, sdir, i, pos;

	if (disk->read_only())
		errx("read only disk");
	check();
	// get geometry
	ndir = disk->dpb().dir;
	sdir = (CPM_RECORD_SIZE/sizeof(cpm_fcb))*(disk->dpb().bl);
	// put data to disk
	for (i=pos=0; pos<ndir; pos+=sdir, i++)
		disk->write(i, (byte *)&buffer[pos]);
}

unsigned
cpm_dir::free()
{
        return (disk->dpb().ds - al.sum()) * disk->dpb().bl;
}

dir_rider *
cpm_dir::open(const dir_name &name)
{
	return new dir_rider(this, name);
}

void
cpm_dir::close(dir_rider *dr)
{
	delete dr;
	dr = NULL;
}

bool
cpm_dir::exists(const dir_name &pattern)
{
	unsigned pos;

	for (pos=0; pos<size(); pos++)
		if (buffer[pos].match(pattern))
			return TRUE;
	return FALSE;
}

unsigned
cpm_dir::allocate_sector()
{
	unsigned i;
	unsigned ds = disk->dpb().ds;

	for (i=0; i<ds; i++)
		if (al.get(i) == 0) {
			al.put(i, TRUE);
			return i;
		}
	return 0;
}

void
cpm_dir::erase(const dir_entry &entry)
{
	unsigned pos;

	for (pos=entry.pos0; pos<=entry.posn; pos++)
		buffer[pos].dr = 0xE5;
	warnx("%s deleted", entry.name.str());
}
	
bool
cpm_dir::erase(const dir_name &pattern)
{
	bool found;
	dir_rider *r;
	const dir_entry *entry;

	if (disk->read_only())
		errx("read only disk");
	found = FALSE;
	r = open(pattern);
	while ((entry = r->read()) != NULL) {
		erase(*entry);
		found = TRUE;
	}
	if (found) {
		save();
		set_al();
	}
	return found;
}

bool
cpm_dir::unerase(const dir_entry &entry)
{
	dir_name dn;
	int dr;
	unsigned pos, i, sec, sum;
	unsigned dpb_ex = disk->dpb().ex;
	unsigned dpb_ds = disk->dpb().ds;

	// find user array
	dn = entry.name;
	for (dr=0x0F; dr>=0; dr--) {
		dn.dr=dr;
		if (!exists(dn)) break;
	}
	if (dr < 0) {
		warnx("%s cannot find user array", entry.name.str());
		return FALSE;
	}
	// test for whole file 
	for (pos=entry.pos0; pos<entry.posn; pos++) {
		if (buffer[pos].rc < 0x80) {
			warnx("%s fragmented", entry.name.str());
			return FALSE;
		}
		if (buffer[pos].ex != (pos-entry.pos0)*dpb_ex) {
			warnx("%s fragmented", entry.name.str());
			return FALSE;
		}
	}
	if (buffer[pos].ex/dpb_ex != pos-entry.pos0) {
		warnx("%s fragmented", entry.name.str());
		return FALSE;
	}
	// removed used sectors
	sum = 0;
	for (pos=entry.pos0; pos<=entry.posn; pos++)
		for (i=0; i<16; i++) {
			sec = buffer[pos].d[i];
			if (sec == 0 || sec >= dpb_ds) {
				sec = 0;
 			} else if (al.get(sec)) {
				sec = 0;
			} else {
				al.put(sec, TRUE);
				sum++;
			}
			buffer[pos].d[i] = sec;
		}
	if (sum == 0) {
		warnx("%s empty", entry.name.str());
		return FALSE;
	}
	// un-erase file
	for (pos=entry.pos0; pos<=entry.posn; pos++)
		buffer[pos].dr = dr;
	warnx("%s undeleted", dn.str());
	return TRUE;
}

bool
cpm_dir::unerase()
{
	bool found;
	dir_rider *r;
	const dir_entry *entry;

        if (disk->read_only())
                errx("read only disk");
        found = FALSE;
	r = open(dir_name("*.*", TRUE));
	while ((entry = r->read(TRUE)) != NULL) {
		if (entry->name.dr != 0xE5)
			continue;
		if (entry->name.f[1] == 0xE5)
			continue;
		if (unerase(*entry))
			found = TRUE;
	}
	if (found) {
		save();
		set_al();
	}
	return found;
}

bool
cpm_dir::rename(const dir_name &old_p, const dir_name &new_p)
{
	bool found;
	dir_rider *r;
	const dir_entry *entry;
	dir_name dn;
	char old_name[CPM_MAX_FNAME];
	unsigned pos;

        if (disk->read_only())
                errx("read only disk");
        found = FALSE;
	r = open(old_p);
	while ((entry = r->read()) != NULL) {
		dn = entry->name;
		strcpy(old_name, dn.str()); 
		dn.rename(new_p);
		if (exists(dn)) {
			warnx("%s exists", dn.str());
			continue;
		}
		for (pos=entry->pos0; pos<=entry->posn; pos++)
			buffer[pos].rename(new_p);
		warnx("renamed %s to %s", old_name, dn.str());
		found = TRUE;
	}
	if (found) {
		save();
		set_al();
	}
	return found;
}

unsigned
cpm_dir::collect(const dir_name &dn)
{
	unsigned src, dst, pos;

	// move used entries
	for(src=dst=0; src<size(); src++) {
		if (buffer[src].dr == 0xE5)
			continue;	// unused entries
		if (buffer[src] == dn)
			continue;	// file with same name
		buffer[dst++] = buffer[src];
	}
	// clean bottom array
	for (pos=dst; pos<size(); pos++) 
		memset(&buffer[pos], 0xE5, sizeof(cpm_fcb));
	return dst;
}

dir_entry &
cpm_dir::add(const dir_name &dn)
{
	static dir_entry result;
	unsigned pos;

	pos = collect(dn);
	if (pos >= size())
		errx("directory full");
	// setup dir entry
	memset(&buffer[pos], 0, sizeof(cpm_fcb));
	memcpy(&buffer[pos], &dn, sizeof(dir_name));
	// save new directory
	save();
	set_al();
	// setup result
	result.name = dn;
        result.pos0 = result.posn = pos;
        result.sectors = 0;
        result.records = 0;
	return result;
}

void
cpm_dir::append(dir_entry &entry, const cpm_fcb &fcb)
{
	unsigned pos;

	// allocate new dir entry 
	pos = entry.posn;
	if (buffer[pos].rc == 0x80) {
		if (++pos >= size())
			errx("directory full");
		entry.posn = pos;
	}
	// init it
	buffer[pos] = fcb;
	return;
}
