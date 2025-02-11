#
#  makefile - NT Virtual CP/M Machine.
#
#  License CC0 1.0 Universal 
#
#  https://stackoverflow.com/questions/1079832/
#
#  08 Feb 25   0.1   - Initial version - MT
#                    - Added the ability to create debug code using target-
#                      specific variable values - MT
#
#  Usage:
#
#  make              - Incremental make.
#  make all		
#  make release      - Rebuild application using release flags.  
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

all: CFLAGS	+= -flto -Ofast -D NDEBUG
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

release: clean
release: all

debug: clean
debug: CFLAGS	+= -Og -D DEBUG
debug: $(PROGRAM)

clean:
#	@rm -f $(PROGRAM) # -v
	@rm -f $(OBJECTS) # -v

backup: clean
	@echo "$(PROJECT)-`date +'%Y%m%d%H%M'`.tar.gz"; tar -czpf ..\/$(PROJECT)-`date +'%Y%m%d%H%M'`.tar.gz $(FILES)	
