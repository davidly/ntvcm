//
// $Id: biosdump.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// simple CP/M disk image viewer
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
// $Log: biosdump.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: biosdump.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include "err.h"
#include "bios.h"
#include "dump.h"


static void
usage()
{
	puts("biosdump " VERSION "  view c64/c128 CP/M disk images");
	puts("Copyright (c) 1995-1997 Jochen Metzinger\n");
	printf("Usage: %s\n\n", short_usage);
	puts("image          disk image\n");
	puts("-h --help      print this help, then exit");
	exit(EXIT_SUCCESS);
}

class test_disk: public bios_disk {
	public:
		void dump(unsigned nr);
			// dump a sector
};

void test_disk::dump(unsigned nr)
{
	byte *sector;
	unsigned ph_nr;
	unsigned i;
	unsigned tr, sk;

	TRY {
		if (nr >= dpb().ds)
			errx("illegal sector #%i", nr);
		putchar('*');
		for (i=0, ph_nr = dpb().bpp*nr; i<dpb().bpp; i++) {
			type->translate(ph_nr++, tr, sk);
			printf(" %02i/%02i ", tr, sk);
		}
		putchar('\n');
		sector = new byte[128*dpb().bl];
		read(nr, sector);
		::dump(sector, 128*dpb().bl, 0);
		delete[] sector;
	} CATCH() {
	}
}


int
main(int argc, char *argv[])
{
	test_disk disk;
	int nr, count;
	char line[256];

#ifdef MSDOS
	source = "biosdump";
#else
	source = argv[0];
#endif
	short_usage = "biosdump [--help] image";
	err_init();
	while (1) {
		static int c, index;
		static struct option long_options[] = {
			{"help", no_argument, NULL, 'h'},
			{0, 0, 0, 0},
		};

		c = getopt_long (argc, argv, "h", long_options, &index);
		if (c == EOF)
			break;
		switch (c) {
			case 'h':
				usage();
				break;
			case ':':
			case '?':
				errx();
				break;
			default:
				errx("unknown option %c", c);
		}
	}
	if (optind == argc)
		errx();
	if (optind < argc - 1)
		errx("too many arguments");
	disk.open(argv[optind]);
	printf("* ph=%i bl=%i bl/ph=%i sz=%i dir=%i exm=%i %s\n",
	       disk.dpb().bl<<7, disk.dpb().ph<<7, disk.dpb().bpp,
	       disk.dpb().ds, disk.dpb().dir, disk.dpb().ex-1,
	       disk.read_only()?"RO":"RW");
	short_usage = NULL;

	while (1) {
		fputs("# ", stdout);
		if (fgets(line, sizeof(line), stdin) == NULL) {
			putchar('\n');
			break;
		}
		count = sscanf(line, "%i", &nr);
		if (count < 1)
			continue;
		if (nr < 0)
			break;
		disk.dump(nr);
	}
	disk.close();
	return EXIT_SUCCESS;
}
