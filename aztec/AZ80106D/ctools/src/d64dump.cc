//
// $Id: d64dump.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// simple disk image viewer
//
// Copyright (C) 1995-1997  Jochen Metzinger
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
// $Log: d64dump.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: d64dump.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include "err.h"
#include "d64.h"
#include "dump.h"


static void
usage()
{
	puts("d64dump " VERSION "  view c64 disk images");
	puts("Copyright (c) 1995-1997 Jochen Metzinger\n");
	printf("Usage: %s\n\n", short_usage);
	puts("image          disk image\n");
	puts("-h --help      print this help, then exit");
	exit(EXIT_SUCCESS);
}

int 
main(int argc, char *argv[])
{
	int tr, sk, n;
	block sector;
	char line[256];
	class d64 d64;

#ifdef MSDOS
	source = "d64dump";
#else
	source = argv[0];
#endif
	short_usage = "d64dump [--help] image";
	err_init();
	while (TRUE) {
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
			default:
				errx("unknown option %c", c);
		}
	}
	if (optind == argc)
		errx();
	if (optind < argc - 1)
		errx("too many arguments");
	d64.open(argv[optind]);
	printf("%s [15%c1 mode] %s\n",
	       argv[optind],	
	       d64.double_sided()?'7':'4',
	       d64.read_only()?"RO":"RW");
	short_usage = NULL;
	sector[0] = 18;
	sector[1] =  0;
	while (TRUE) {
		fputs(" tr sk: ", stdout);
		if (fgets(line, sizeof(line), stdin) == NULL) {
			putchar('\n');
			break;
		}
		n = sscanf(line, "%i %i", &tr, &sk);
		if (n < 1)
			continue;
		if (tr < 0) 
			break;
		if (tr == 0) {
			tr = sector[0];
			sk = sector[1];
			printf(">tr sk: %i %i\n", tr, sk);
		} else if (n < 2) {
			continue;
		}
		TRY {
			d64.read(tr, sk, sector);
			dump(&sector, 256, 0);
		} CATCH() {
		}
	}
	d64.close();
	return EXIT_SUCCESS;
}
