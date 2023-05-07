//
// $Id: cformat.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $
// Copyright (C) 1996-1997  Jochen Metzinger
//
// create empty disk images
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
// $Log: cformat.cc,v $
// Revision 1.1  1997/02/20 23:21:23  jochen
// from ctools 3.0
//

#ifndef lint
static const char rcsid[] = "$Id: cformat.cc,v 1.1 1997/02/20 23:21:23 jochen Rel $";
#endif /* not lint */


#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "global.h"
#include "misc.h"
#include "err.h"
#include "d64.h"
#include "bios.h"


block cpm_block = {
	'C','B','M',     /* "CBM" boot id */
	0x00,0x00,       /* 0000h address to load follow sectors */
	0x00,            /*   00h bank    to load follow sectors */
	0x00,            /*    0  follow sectors */
	'\0',            /*    "" text to output (ASCIIZ) */
	'\0',            /*    "" program to load */
	0x78,            /* SEI           ; disable interrupts *boot code* */
	0x20,0x84,0xFF,  /* JSR $FF84     ; call IOINIT */
	0xA9,0x3E,       /* LDA #$3E      ; = 00,11,11,1,0 */
	0x8D,0x00,0xFF,  /* STA $FF00     ; MMU: Bank 0, all RAM, only I/O */
	0xA9,0xC3,       /* LDA #$C3 */
	0x8D,0xEE,0xFF,  /* STA $FFEE     ; [$FFEE] := $C3 <z80: JP> */
	0xA9,0x08,       /* LDA #$08 */
	0x8D,0xEF,0xFF,  /* STA $FFEF     ; [$FFEF] := $08 <z80: xx08> */
	0xA9,0x00,       /* LDA #$00 */
	0x8D,0xF0,0xFF,  /* STA $FFF0     ; [$FFF0] := $00 <z80: 00xx> */
	0x4C,0xD0,0xFF,  /* JMP $FFD0     ; enable Z80 => boot cpm */
	0,
};

block bam_block = {
	0x01,0x05,0x41,0x80,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x43,0x50,0x2F,0x4D,0x20,0x50,0x4C,0x55,
	0x53,0xA0,0xA0,0xA0,0xA0,0xA0,0xA0,0xA0,
	0xA0,0xA0,0x36,0x35,0xA0,0x32,0x41,0xA0,
	0xA0,0xA0,0xA0,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x43,0x42,0x4D,0x00,0x00,0x00,0x00,0x00,
	0x00,0x78,0x20,0x84,0xFF,0xA9,0x3E,0x8D,
	0x00,0xFF,0xA9,0xC3,0x8D,0xEE,0xFF,0xA9,
	0x08,0x8D,0xEF,0xFF,0xA9,0x00,0x8D,0xF0,
};

static const char *type_name[] = {
	"(none)",
	"c64 single sided",
	"c128 single sided",
	"c128 double sided",
};

static int image_type = CPM_DISK_C128_SS;
static const char *image_name = NULL;

static void
usage()
{
	puts("cformat " VERSION "  create empty c64/c128 CP/M disk images");
	puts("Copyright (c) 1996-1997  Jochen Metzinger\n");
	printf("Usage: %s\n\n", short_usage);
	puts("-0 --c64       make a c64 disk image");
	puts("-1 --ss        make a single sided c128 disk image [default]");
	puts("-2 --ds        make a double sided c128 disk image");
	puts("image          disk image\n");
	puts("-h --help      print this help, then exit");
	exit(EXIT_SUCCESS);
}

void
init(int argc, char *argv[])
{
	int c, index;

#ifdef MSDOS
	source = "cformat";
#else
	source = argv[0];
#endif
 	short_usage = "cformat [--help] [-0|-1|-2] image";
	while (1) {
		static struct option long_options[] = {
			{"help", no_argument, NULL, 'h'},
			{"c64", no_argument, NULL, '0'},
			{"ss", no_argument, NULL, '1'},
			{"ds", no_argument, NULL, '2'},
			{0, 0, 0, 0},
		};

		c = getopt_long (argc, argv, "h012", long_options, &index);
		if (c == EOF)
			break;
		switch (c) {
			case 'h':
				usage();
				break;
			case '0':
				image_type = CPM_DISK_C64;
				break;
			case '1':
				image_type = CPM_DISK_C128_SS;
				break;
			case '2':
				image_type = CPM_DISK_C128_DS;
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
	image_name = argv[optind];
	short_usage = NULL;
}

static void
pre_format()
{
	FILE *fp;
	block empty_block;
	int nr;

	// open file
	fp = fopen(image_name, "wb");
	if (fp == NULL) err();
	// setup blocks
	memset(empty_block, 0, sizeof(empty_block));
	switch (image_type) {
		case CPM_DISK_C64:
			cpm_block[0x00] = 'K';
			bam_block[0xE0] = 'K';
			break;
		case CPM_DISK_C128_DS:
			cpm_block[0xFF] = 0xFF;
			bam_block[0x03] = 0x81;
			break;
	}
	// write raw image
	fputs("Side 0: ", stdout);
	if (fwrite(cpm_block, sizeof(block), 1, fp) < 1)
		err();
	for (nr=1; nr <= 356; nr++) {
		if (nr%10 == 0) putchar('#');
		if (fwrite(empty_block, sizeof(block), 1, fp) < 1)
			err();
	}
	if (fwrite(bam_block, sizeof(block), 1, fp) < 1)
		err();
	for (nr=358; nr <= 682; nr++) {
		if (nr%10 == 0) putchar('#');
		if (fwrite(empty_block, sizeof(block), 1, fp) < 1)
			err();
	}
	putchar('\n');
	if (image_type == CPM_DISK_C128_DS) {
		fputs("Side 1: ", stdout);
		for (nr=683; nr <= 1365; nr++) {
			if (nr%10 == 0) putchar('#');
			if (fwrite(empty_block, sizeof(block), 1, fp) < 1)
				err();
		}
		putchar('\n');
	}
	if (fclose(fp) < 0)
		err();
}

static void
cpm_format()
{
	unsigned nr;
	bios_disk disk;
	struct dpb dpb;
	byte *dir;

	disk.open(image_name);
	dpb = disk.dpb();
	dir = new byte[128*dpb.bl];
	memset(dir, 0xE5, 128*dpb.bl);
	fputs("Dir   : ", stdout);
	for (nr=0; nr < (dpb.dir/4)/dpb.bl; nr++) {
		disk.write(nr,dir);
		putchar('%');
	}
	putchar('\n');
	delete[] dir;
	disk.close();
}

int
main(int argc, char *argv[])
{

	init(argc, argv);
	if (exists_file(image_name)) 
		errx("file %s exists", image_name);
	warnx("create %s image %s", type_name[image_type], image_name);
	pre_format();
	cpm_format();
	warnx("done");
	return EXIT_SUCCESS;
}
