//
// $Id: ctools.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1995-1997  Jochen Metzinger
//
// main programm of ctools
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
// $Log: ctools.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id";
#endif /* not lint */


#include <ctype.h>
#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include "err.h"
#include "fcb.h"
#include "tools.h"


static void
usage()
{
	puts("ctools " VERSION "  manipulate c64/c128 CP/M disk images");
	puts("Copyright (c) 1995-1997  Jochen Metzinger\n");
	printf("Usage: %s\n\n", short_usage);
	puts("-i --initdir           remove SFCB, XFCB and dir labels");
	puts("-u --user user-array   set default user array (a-p)");
	puts("image                  disk image");
	puts("command                tool:");
	puts("  c                      check directory (correct with -i)");
	puts("  d [file]               show directory");
	puts("  e file                 erase file");
	puts("  g file                 get file from image");
	puts("  h,?                    show this display this help and exit");
	puts("  m file                 output dump of file");
#ifdef MSDOS
	puts("  p file                 put file onto image");
#else
	puts("  p files                put files onto image");
#endif
	puts("  r old new              rename old to new");
	puts("  t file                 output file");
	puts("  u                      unerase files\n");
	puts("-h --help              print this help, then exit");
        exit(EXIT_SUCCESS);
}

int
main (int argc, char *argv[])
{
	char command, *image;
	bool initdir = FALSE;

#ifdef MSDOS
        source = "ctools";
#else
        source = argv[0];
#endif
	short_usage = "ctools [--help] [-i] [-u user-array]" \
	              " image command [options]";
	err_init();
	while (1) {
		static int c, index;
		static struct option long_options[] = {
			{"help",    no_argument,       NULL, 'h'},
			{"initdir", no_argument,       NULL, 'i'},
			{"user",    required_argument, NULL, 'u'},
			{0, 0, 0, 0},
		};

		c = getopt_long (argc, argv, "hiu:", long_options, &index);
		if (c == EOF)
			break;
		switch (c) {
			case 'h':
				usage();
				break;
			case 'i':
				initdir = TRUE;
				break;
			case 'u':
				c = toupper(*optarg);
				if ('A' <= c && c <= 'P')
					default_user_array = c - 'A';
				else
					errx("bad user array %c", c);
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
	image = argv[optind++];
	if (optind == argc)
                errx();
	command = argv[optind++][0];
	if (command == '\0')
		errx();
	if (islower(command))
		command = toupper(command);
	init(image, initdir);
	short_usage = NULL;
	switch (command) {
		case 'C':
			if (initdir)
				flush_dir();
			break;
		case 'D':
			print_dir((optind == argc) ? "*.*" : argv[optind]);
			break;
		case 'E':
			if (optind+1 != argc)
				errx("expected one parameter"); 
			erase_files(argv[optind]);
			break;
		case 'G':
			if (optind+1 != argc)
				errx("expected one parameter"); 
			get_files(argv[optind]);
			break;
		case 'H': case '?':
			usage();
			break;
		case 'M':
			if (optind+1 != argc)
				errx("expected one parameter"); 
			dump_files(argv[optind]);
			break;
		case 'P':
#ifdef MSDOS
			if (optind+1 != argc)
				errx("expected one parameter");
			put_files(argv[optind]);
#else
			if (optind == argc)
				errx("expected parameters"); 
			while (optind < argc) 
				put_file(argv[optind++]);
#endif
			break;
		case 'R':
			if (optind+2 != argc)
				errx("expected two parameters"); 
			rename_files(argv[optind], argv[optind+1]);
			break;
		case 'T':
			if (optind+1 != argc)
				errx("expected one parameter"); 
			print_files(argv[optind]);
			break;
		case 'U':
			if (optind != argc)
				errx("expected no parameter"); 
			unerase_files();
			break;
		default:
			errx("unknown command %c", command);
	}
	done();
	return EXIT_SUCCESS;
}
