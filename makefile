#
#  makefile - NT Virtual CP/M Machine.
#
#  Copyright(C) 2022 - David Lee/MT
#
#  This  program is free software: you can redistribute it and/or modify it
#  under  the terms of the GNU General Public License as published  by  the
#  Free  Software Foundation, either version 3 of the License, or (at  your
#  option) any later version.
#
#  This  program  is distributed in the hope that it will  be  useful,  but
#  WITHOUT   ANY   WARRANTY;   without even   the   implied   warranty   of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#  Public License for more details.
#
#  You  should have received a copy of the GNU General Public License along
#  with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Note separator (tab) at the beginning of the line CANNOT be a space..!
#
#  https://stackoverflow.com/questions/1079832/
#
#  08 Feb 25   0.1   - Initial version - MT
#                    - Added the ability to create debug code using target-
#                      specific variable values - MT
#
#  Usage:
#
#  make 	     - Rebuild application using release flags.  
#  make all		
#  make debug	     - Rebuild application using debug flags.  
#  make clean        - Deletes object files
#  make VERBOSE=1    - Verbose output
#  make backup       - Backup files
#

PROJECT	= gcc-ntvcm
PROGRAM	= ntvcm
SOURCES = ntvcm.cxx x80.cxx
FILES	= *.cxx *.hxx LICENSE README.md makefile *.md .gitignore #.gitattributes
OBJECTS	= $(SOURCES:.cxx=.o)
OUTPUT	= $(PROGRAM).out
LANG	= LANG_$(shell (echo $$LANG | cut -f 1 -d '_'))
COMMIT	!= git log -1 HEAD --format=%h 2> /dev/null
BUILD	!= printf "%04d" $(shell git rev-list --count HEAD 2> /dev/null)
UNAME	!= uname
CC	= g++

LIBS	= 
LFLAGS	= -static
CFLAGS	= -ggdb -fno-builtin -I .

ifndef VERBOSE
VERBOSE	= 0
endif

ifneq ($(COMMIT),)
CFLAGS	+= -DCOMMIT_ID='" [Commit Id: $(COMMIT)]"'
endif

ifneq ($(BUILD),)
CFLAGS	+= -DBUILD='".$(BUILD)"'
endif

all: clean 
all: CFLAGS	+= -flto -Ofast
all: $(PROGRAM) $(OBJECTS)

$(PROGRAM): $(OBJECTS)
ifneq ($(VERBOSE),0)
	@echo
	@echo $(CC) $(LFLAGS) $(OBJECTS) -o $@ $(LIBS)
	@echo
endif
	@$(CC) $(LFLAGS) $(OBJECTS) -o $@ $(LIBS)
	@ls --color $@  

$(OBJECTS) : $(SOURCES)
ifneq ($(VERBOSE),0)
	@echo 
	@echo $(CC) $(CFLAGS) -c $(SOURCES)
endif
	@$(CC) $(CFLAGS) -c $(SOURCES)

debug: clean
debug: CFLAGS	+= -Og  -D DEBUG
debug: $(PROGRAM)

clean:
#	@rm -f $(PROGRAM) # -v
	@rm -f $(OBJECTS) # -v

backup: clean
	@echo "$(PROJECT)-`date +'%Y%m%d%H%M'`.tar.gz"; tar -czpf ..\/$(PROJECT)-`date +'%Y%m%d%H%M'`.tar.gz $(FILES)	
