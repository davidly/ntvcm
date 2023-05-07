//
// $Id: fcb.h,v 1.1 1997/02/20 23:21:23 jochen Rel $
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

#ifndef __FCB_H__
#define __FCB_H__ 1

#include "global.h"

#define	CPM_MAX_FNAME	15	// max. string length of dir_name::str()

#define FLAG_1		0x0400	// f1' -- returned by dirname::flags()
#define FLAG_2		0x0200	// f2'
#define FLAG_3		0x0100	// f3'
#define FLAG_4		0x0080	// f4'
#define FLAG_5		0x0040	// f5'
#define FLAG_6		0x0020	// f6'
#define FLAG_7		0x0010	// f7'
#define FLAG_8		0x0008	// f8'
#define FLAG_RO		0x0004	// t1'
#define FLAG_SYS	0x0002	// t2'
#define FLAG_A		0x0001	// t3'

extern byte default_user_array;
	// for dir_name::parse(), default 0

struct dir_name {
	public: // data
		byte dr;	// 1st part of CP/M FCB
		byte f[8];
		byte t[3];
	public: // functions
		dir_name() {}
			// default constructor
		dir_name(const char name[], bool expand=FALSE) {
			parse(name, expand);
		}	// constructor
		void parse(const char filename[], bool expand=FALSE);
			// init with `filename' 
		char *str() const;
			// return file name in formated format
		char *filename() const;
			// return file name w/o drive, spaces and ending dot
		unsigned flags() const;
			// return flag vector
		bool operator==(const dir_name &dn) const;
			// have I and `dn' equal names
		bool operator!=(const dir_name &dn) const {
			return !operator==(dn);
		}	// have I and `dn' different names
		bool match(const dir_name &pattern) const;
			// match with `pattern'
		void rename(const dir_name &new_pattern);
			// rename with new-pattern
};
		
struct cpm_fcb: public dir_name {
	public: // data
		byte ex;	// 2st part of CP/M FCB
		byte s1;
		byte s2;
		byte rc;
		byte d[16];
	public: // functions
		int sectors();
			// return # of used sectors
		int records();
			// return rc including ex
};

#endif /* __FCB_H__ */
