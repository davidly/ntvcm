//
// $Id: tools.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// user tools
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
// $Log: tools.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: tools.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif


#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef MSDOS
#include <dos.h>
#endif
#include "err.h"
#include "misc.h"
#include "dump.h"
#include "dir.h"
#include "file.h"
#include "tools.h"


static bios_disk disk;
static cpm_dir   dir;


void
init(const char name[], bool initdir)
{
	disk.open(name);
	dir.load(&disk, initdir);
}

void
done()
{
	disk.close();
}

void
print_dir(const char filename[])
{
	int found;
	dir_rider *r;
	const dir_entry *entry;
	unsigned flags;

	found = 0;
	r = dir.open(dir_name(filename, TRUE));
	while ((entry = r->read()) != NULL) {
		if (found++ == 0) {
			puts("  Name         Bytes  Recs    Attributes");
			puts("-------------- ----- ------- ------------");
		};
		printf("%s %5uk%7u ", (entry->name).str(),
			(entry->sectors)*(dir.sector_size()/8),
			entry->records);
		flags = (entry->name).flags();
		printf("%3s %2s %c%c%c%c%c\n",
			flags & FLAG_SYS ? "Sys" : "Dir",
			flags & FLAG_RO  ? "RO"  : "RW",
			flags & FLAG_A ? 'A' : ' ',
			flags & FLAG_1 ? '1' : ' ',
			flags & FLAG_2 ? '2' : ' ',
			flags & FLAG_3 ? '3' : ' ',
			flags & FLAG_4 ? '4' : ' ');
	}
	dir.close(r);
	if (found > 0) {
		puts("-------------- ----- ------- ------------");
		printf("%4i file(s)   %5uk free\n", found, dir.free()/8);
	} else {
		warnx("no file found");
	}
}

void
erase_files(const char filename[])
{
	if (dir.erase(dir_name(filename))) return;
	warnx("no file found");
}

void
unerase_files()
{
	if (dir.unerase()) return;
	warnx("no file found");
}

void
rename_files(const char old_name[], const char new_name[])
{
	if (dir.rename(dir_name(old_name), dir_name(new_name, TRUE)))
		return;
	warnx("no file found");
}

void
print_files(const char filename[])
{
	bool found;
	dir_rider *r;
	const dir_entry *entry;
	cpm_file f;
	cpm_rec data;
	int i;

	found = FALSE;
	r = dir.open(dir_name(filename, TRUE));
	while ((entry = r->read()) != NULL) {
		found = TRUE;
		puts("::::::::::::::");
		puts(entry->name.str());
		puts("::::::::::::::");
		f.open(dir, *entry);
		while (!f.eof()) {
			f.read(data);
			for (i=0; i<CPM_RECORD_SIZE; i++) {
				if (data[i] == 'Z'-'@')
					goto done;
#ifdef unix
				if (data[i] == '\r')
					continue; 
#endif
				putchar(data[i]);
			}
		}
	done:	putchar('\n');
		f.close();
	}
	if (! found)
		warnx("no file found");
}

void
dump_files(const char filename[])
{
	bool found;
	dir_rider *r;
	const dir_entry *entry;
	cpm_file f;
	long position;
	cpm_rec data;

	found = FALSE;
	r = dir.open(dir_name(filename, TRUE));
	while ((entry = r->read()) != NULL) {
		found = TRUE;
		puts("::::::::::::::");
		puts(entry->name.str());
		puts("::::::::::::::");
		f.open(dir, *entry);
		position = 0;
		while (!f.eof()) {
			f.read(data);
			dump(data, CPM_RECORD_SIZE, position);
			position += CPM_RECORD_SIZE;
		}
		putchar('\n');
		f.close();
	}
	if (! found)
		warnx("no file found");
}

#ifdef MSDOS
static char *
target_filename(const dir_name &dn)
{
	static const char MSDOS_FILE_CHARS[] =
		"!#$%&'()-0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`{}~";
	char *s, *t, *dot;

	s = dn.filename();
	// X-out strange characters
	dot = strrchr(s, '.');
	if (dot == NULL) dot = strchr(s, '\0');
	for (t=s; *t!='\0'; t++) {
		*t = toupper(*t);
		if (t == dot)
			continue;
		if (strchr(MSDOS_FILE_CHARS, *t) == NULL)
			*t = 'X';
	}
	return s;
}
#else /* not MSDOS */
#define target_filename(dn) ((dn).filename())
#endif

static char *
target_check_file(const char *fname)
{
	static char filename[256];
	char line[256], *p;
	int c, answer;

	strncpy(filename, fname, sizeof(filename)-1);
	filename[sizeof(filename)-1] = '\0';
	while (exists_file(filename)) {
		fprintf(stderr, "%s: Warning! %s already exists. " \
			"Overwrite (y/N/r)? ", source, filename);
		fflush(stderr);
		c = fgetc(stdin);
		answer = tolower(c);
		while (c != '\n' && c != EOF)
			c = fgetc(stdin);
		switch (answer) {
			case '\n': case 'n':
				return NULL;
			case 'y':
				return filename;
			case 'r':
				fprintf(stderr, "New name? ");
				fflush(stderr);
				// get a line w/o '\n'
				fgets(line, sizeof(line), stdin);
				p = strchr(line, '\n');
				if (p != NULL) *p = '\0';
				if (line[0] != '\0')
					strcpy(filename, line);
				break;
			default:
				break;	
		}
	}
	return filename;
}

void
get_files(const char filename[])
{
	bool found;
	dir_rider *r;
	const dir_entry *entry;
	cpm_file f;
	FILE *fp;
	char *fname;
	cpm_rec data;

	found = FALSE;
	r = dir.open(dir_name(filename, TRUE));
	while ((entry = r->read()) != NULL) {
		found = TRUE;
		warnx("get %s",	entry->name.str());
		fname = target_check_file(target_filename(entry->name));
		if (fname == NULL)
			continue;
		f.open(dir, *entry);
		if ((fp = fopen(fname, "wb")) == NULL)
			err();
		while (!f.eof()) {
			f.read(data);
			if (fwrite(data, CPM_RECORD_SIZE, 1, fp) < 1)
				err();
		}
		f.close();
		if (fclose(fp) < 0)
			err();
	}
	if (! found)
		warnx("no file found");
}

static dir_name&
cpm_filename(const char filename[])
{
	static const char CPM_FILE_CHARS[] =
		"\"#%'+-0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`~";
	static dir_name dn;
	char *s, *t, *dot;

	// take basename
#ifdef MSDOS
	t = strchr(filename, ':');
	if (t != NULL)
		filename = t + 1;
	t = strrchr(filename, '\\');
#else /* not MSDOS */
	t = strrchr(filename, '/');
#endif
	if (t != NULL)
		filename = t + 1;
	s = strdup(filename);
	// X-out strange characters
	dot = strrchr(s, '.');
	if (dot == NULL) dot = strchr(s, '\0');
	for (t=s; *t!='\0'; t++) {
		*t = toupper(*t);
		if (t == dot)
			continue;
		if (strchr(CPM_FILE_CHARS, *t) == NULL)
			*t = 'X';
	}
	// parse name
	dn.parse(s);
	free(s);
	// ... and return it
	return dn;
}

static bool
cpm_check_file(dir_name &dn)
{
	dir_name new_name;
	char line[256], *p;
	int c, answer;

	while (dir.exists(dn)) {
		fprintf(stderr, "%s: Warning! %s already exists. " \
			"Overwrite (y/N/r)? ", source, dn.str());
		fflush(stderr);
		c = fgetc(stdin);
		answer = tolower(c);
		while (c != '\n' && c != EOF)
			c = fgetc(stdin);
		switch (answer) {
			case '\n': case 'n':
				return FALSE;
			case 'y':
				return TRUE;
			case 'r':
				fprintf(stderr, "New name? ");
				fflush(stderr);
				// get a line w/o '\n'
				fgets(line, sizeof(line), stdin);
				p = strchr(line, '\n');
				if (p != NULL) *p = '\0';
				if (line[0] != '\0') {
					TRY {
						new_name.parse(line, TRUE);
						dn.rename(new_name);
					} CATCH() {}
				}
				break;
			default:
				break;	
		}
	}
	return TRUE;
}

void
put_file(const char filename[])
{
	dir_name dn;
	FILE *fp;
	cpm_file f;
	cpm_rec data;
	size_t sz;

	if (! regular_file(filename)) {
		warnx("%s: not a regular file", filename);
		return;
	}
	warnx("put %s", filename);
	dn = cpm_filename(filename);
	if (! cpm_check_file(dn))
		return;
	if ((fp = fopen(filename, "rb")) == NULL)
		err("%s", filename);
	f.create(dir, dn);
	while ((sz = fread(data, 1, CPM_RECORD_SIZE, fp)) == CPM_RECORD_SIZE)
		f.write(data);
	if (ferror(fp))
		err();
	if (sz > 0) {
		while (sz < CPM_RECORD_SIZE)
			data[sz++] = 'Z'-'@';
		f.write(data);
	}
	if (fclose(fp) < 0)
		err();
	f.close();
}

#ifdef MSDOS

void
put_files(char filename[])
{
	struct find_t ffblk;
	char *name, *base, *t;

	// can file name expanded?
	if (_dos_findfirst(filename, 0xff, &ffblk) != 0) {
		put_file(filename);
		return;
	}
	// file prefix from name to base
	base = name = malloc(strlen(filename)+13);
	strcpy(name, filename);
	t = strchr(base, ':');
	if (t != NULL)
		base = t + 1;
	t = strrchr(base, '\\');
	if (t != NULL)
		base = t + 1;
	// work on all found names
	do {
		strcpy(base, ffblk.name);
		put_file(name);
	} while (_dos_findnext(&ffblk) == 0);
	free(name);
}

#endif /* MSDOS */

void
flush_dir()
{
	dir.save();
}
