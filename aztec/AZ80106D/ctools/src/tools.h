//
// $Id: tools.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
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

#ifndef __TOOLS_H__
#define __TOOLS_H__ 1

#include "global.h"

extern void init(const char name[], bool initdir=FALSE);
        // open disk support
extern void done();
        // close disk support
extern void print_dir(const char filename[]);
        // output directory
extern void erase_files(const char filename[]);
        // erase files
extern void unerase_files();
        // unerase files
extern void rename_files(const char old_name[], const char new_name[]);
        // rename files
extern void print_files(const char filename[]);
        // output files context
extern void dump_files(const char filename[]);
        // output files' dump
extern void get_files(const char filename[]);
        // copy CP/M files to current directory
extern void put_file(const char filename[]);
        // copy a file to CP/M directory
#ifdef MSDOS
extern void put_files(char filename[]);
        // copy MsDos files to CP/M directory
#endif
extern void flush_dir();
	// save the directory buffer to disk

#endif /* __TOOLS_H__ */
