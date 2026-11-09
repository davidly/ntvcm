# PL/M-80 2.0 built with gfortran

Intel's PL/M-80 compiler was a Fortran program. Its source survives as
version 2.0 (January 1975, with a 1977 University of Michigan change), from
Michigan Terminal System distribution tapes. `gfplm.py` compiles that Fortran
with gfortran into native programs, then runs them to compile PL/M programs
for CP/M. The resulting `.COM` files run in ntvcm. A compile takes a few
milliseconds. [dos/](../dos/README.md) builds the same source for MS-DOS
instead.

Nothing here is distributed: the Fortran sources go in the Git-ignored
`plm80/fortran/`, and the executables in the Git-ignored `plm80/gfortran/bin/`.

## Setup

* The Fortran sources in `plm80/fortran/mts-1989/`, as for
  [dos/](../dos/README.md#setup).
* gfortran on the `PATH` (on Ubuntu, `sudo apt install gfortran`); set
  `GFORTRAN` to use another compiler. This was tested with gfortran 11.4 on
  Linux. On Windows a MinGW gfortran should work but is untested.

```sh
python3 plm80/gfortran/gfplm.py build
```

That writes `bin/plm81` and `bin/plm82`. The source compiles unchanged except
for removing the DOS end-of-file byte that ends each file. `-std=legacy`
accepts the 1975 FORTRAN, and `-fno-automatic` gives local variables the
static storage that the program expects.

## Compile and run

```sh
python3 plm80/gfortran/gfplm.py compile plm80/examples/hello.plm -o hello.com
./ntvcm hello.com
```

`-o` ending in `.hex` writes Intel HEX, and `--listing` saves both passes'
listings. `EOF` is appended when missing. Errors fail with the listings.
Source gets the same checks as with version 4.1 (see
[../README.md](../README.md#writing-cpm-programs)), except that version 2.0
reads 80 columns rather than 72.

The Fortran opens no files. It reads and writes numbered units, which gfortran
connects to files named `fort.n`. Pass 1 reads the source from unit 2 and
writes its listing (12), code (16), console (11) and symbols (17). Pass 2
reads controls to a blank line (1), the symbols (7) and the code (4), and
writes its listing (12), object (17) and console (11).

## Results

* The MTS sample compiles to exactly the HEX in the 1989 archive, which was
  produced on an IBM mainframe.
* All five [PL/M-80 4.1 examples](../README.md) compile to the same bytes as
  with the MS-DOS build, and run correctly in ntvcm in Z80 and 8080 modes.
  See [the comparison with version 4.1](../README.md#version-20-and-41).

## Tests

```sh
python3 plm80/gfortran/test.py
```

The tests build the compiler, compare the MTS sample with its reference HEX,
compile and run the five examples in ntvcm in both modes, and check error
reporting and the source checks. They skip when gfortran or the sources are
missing.
