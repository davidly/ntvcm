# PL/M-80 2.0 built for MS-DOS, run in ntvdm

Intel's PL/M-80 compiler was a Fortran program. Its source survives as
version 2.0 (January 1975, with a 1977 University of Michigan change), from
Michigan Terminal System distribution tapes. `dosplm.py` compiles that
Fortran into MS-DOS programs with Microsoft Fortran 5.1, running in
[ntvdm](https://github.com/davidly/ntvdm), then runs them in ntvdm to compile
PL/M programs for CP/M. The resulting `.COM` files run in ntvcm.

Nothing here is distributed: the Fortran sources go in the Git-ignored
`plm80/fortran/`, and the DOS executables in the Git-ignored `plm80/dos/bin/`.

## Setup

* The Fortran sources in `plm80/fortran/mts-1989/` (`PLM81.FOR`, `PLM82.FOR`
  and the `PLMSAMP` sample), from `plm80s.zip` on
  [The Unofficial CP/M Web Site](http://www.cpm.z80.de/source.html). Its
  README says the code is free "for personal and educational applications".
* ntvdm, by default `../ntvdm/ntvdm` beside this repository; set `NTVDM` to
  use another path.
* Microsoft Fortran 5.1, by default `../dos_compilers/Microsoft Fortran v51`
  from [dos_compilers](https://github.com/davidly/dos_compilers); set
  `MSFORTRAN` to use another path.

```sh
python3 plm80/dos/dosplm.py build
```

That takes a few seconds and writes `bin/PLM81.EXE` and `bin/PLM82.EXE`.
PLM82's code exceeds the 64K segment Microsoft Fortran gives each source
file, so the build splits `PLM82.FOR` into four files between subroutines;
no statements change. The same sources always build the same executables.

## Compile and run

```sh
python3 plm80/dos/dosplm.py compile plm80/examples/hello.plm -o hello.com
./ntvcm hello.com
```

`-o` ending in `.hex` writes Intel HEX, and `--listing` saves both passes'
listings. `EOF` is appended when missing. Errors fail with the listings.
Source gets the same checks as with version 4.1 (see
[../README.md](../README.md#writing-cpm-programs)), except that version 2.0
reads 80 columns rather than 72.

The Fortran opens its units by number, and Microsoft's runtime asks for a
file name the first time each unit is used. `dosplm.py` answers the prompts
through ntvdm's standard input, in the order each pass uses its units: pass
1's listing (unit 12), source (2), code (16), console (11) and symbols (17);
pass 2's listing (12), controls (1, an empty line), symbols (7), code (4),
object (17) and console (11).

## Results

* The MTS sample compiles to exactly the HEX in the 1989 archive, which was
  produced on an IBM mainframe.
* All five [PL/M-80 4.1 examples](../README.md) compile unchanged and run
  correctly in ntvcm in Z80 and 8080 modes. hello, arithmetic, sieve and e
  are byte-identical to version 4.1's output. ttt is three bytes larger and
  takes 300 more cycles: after a `CASE` statement, version 4.1 knows that
  every branch rejoining at one label has just loaded HL with the same
  address and omits the reload; version 2.0 reloads it.

## Tests

```sh
python3 plm80/dos/test.py
```

The tests build the compiler, compare the MTS sample with its reference HEX,
compile and run the five examples in ntvcm in both modes, and check error
reporting. They skip when ntvdm, Microsoft Fortran or the sources are
missing.
