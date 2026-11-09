# Intel PL/M-80 4.1 for ntvcm

## Optional local-image component — setup first

Intel's original PL/M-80 cross-compiler, passes PLM81 and PLM82 version 4.1,
compiles programs here for CP/M, and the resulting `.COM` files run in ntvcm.
The compiler executables are not included or automatically downloaded.
Redistribution permission for these historical Intel/Tymshare images has not
been established. Pins and provenance are in [upstream/NOTICE.txt](upstream/NOTICE.txt).

If you have the right to use them, recover the images from a lawfully obtained
copy of Tymshare tape 169278:

```sh
python3 plm80/recover.py /path/to/169278.tape.bz2
```

The tape is archived at
[VTDA](https://vtda.org/bits/software/DEC/PDP-10/tymshare/169278.tape.bz2)
(about 379 MiB). Recovery verifies the tape's SHA-256, extracts only
`PLM81.SAV` and `PLM82.SAV` from its `(UPL)` directory, verifies both, and
writes them to `plm80/upstream/`. The images and compiled programs stay ignored
by Git.

The compiler is 1970s Fortran for a PDP-10. `pdp10.py` and `pdp10.cxx` run
it; they are adapted from the runner that ntv8008 uses for Intel's 8008
PL/M-8, MAC8 and INTERP/8 from the same tape. [MAC80](../mac80/README.md) and
[INTERP/80](../int80/README.md) use them too. Build the native runner for
speed (about 0.1 second per compile instead of 1–10 seconds in Python):

```sh
./plm80/m.sh          # Windows: plm80\m.bat
```

## Compile and run

```sh
python3 plm80/compile.py plm80/examples/hello.plm
./ntvcm plm80/examples/hello.com
```

That prints `HELLO, PL/M-80!`. `-o file.com` selects the output; an `-o` path
ending in `.hex` writes Intel HEX instead. `--listing file` saves both passes'
listings, including PLM82's symbol map and code listing. `plm80/rebuild.sh` or
`rebuild.bat` recompiles every example.

| Example | Output | Z80 cycles | 8080 cycles (`-8`) |
| --- | --- | ---: | ---: |
| hello | `HELLO, PL/M-80!` | 256 | 258 |
| arithmetic | `OK` after byte/address arithmetic, arrays and pointers | 16,716 | 17,338 |
| sieve | `1899`, ten passes over 8191 odd candidates | 62,059,510 | 63,575,225 |
| e | 192 digits of e, then `DONE` | 77,709,927 | 81,632,924 |
| ttt | `6493 MOVES`, ten iterations of alpha-beta tic-tac-toe | 171,454,256 | 169,822,051 |

The examples are ports of ntv8008's PL/M-8 examples. Only the console output,
the origin and the exit changed; the algorithms are the same.

## Writing CP/M programs

PL/M-80 4.1 predates CP/M and has no operating-system interface, so programs
reach the BDOS directly:

```
100H:
MON1: PROCEDURE(F,A);
    DECLARE F BYTE, A ADDRESS;
    GO TO 5;
END MON1;
...
CALL MON1(2,'A');      /* console output */
CALL MON1(0,0);        /* return to CP/M */
```

* Start with the numeric label `100H:`, which sets the origin to the CP/M
  transient program area. The wrapper rejects images below 100H.
* The compiler passes a procedure's last argument in E or DE and the one before
  it in C or BC, exactly as the BDOS expects. `MON1` stores its parameters and
  jumps to 5, so the BDOS returns straight to `MON1`'s caller. Functions
  return a byte in A, or an address in A (low) and B (high); CP/M 2.2 sets
  A=L and B=H on BDOS return for PL/M.
* End with `CALL MON1(0,0);`. The compiler ends the main program with
  `EI; HLT`, and ntvcm treats `HLT` as an error exit.
* The program sets its own stack. `INITIAL` data is part of the `.COM` file;
  initialize other variables before use.

The language is the early PL/M-80 of Intel's Fortran compilers; this copy of
PLM81 is dated January 1976 in the tape catalog. Arrays need explicit sizes (no
`(*)`); `DATA` declares constant bytes, as in `DECLARE MSG DATA ('HI$');`.
There are no structures, `PUBLIC` or `EXTERNAL`. Procedure locals are static,
so procedures cannot be recursive, and there is no `REENTRANT`.

Source has the same hazards as ntv8008's PL/M-8, and the wrapper checks them:

* The lexer turns characters outside its alphabet into spaces, even inside
  strings: lowercase, `!` and others. Use uppercase source and numeric
  ASCII values such as `33` for `!`. Comments may use any characters.
* Columns past 72 are ignored without an error, which can silently drop a
  statement. Lines must fit in 72 columns. Tabs expand to eight-column stops.
* `EOF` (a PL/M token) is appended when missing.

Compilation errors fail with both listings, and an existing output file is
left unchanged.

## The Fortran source

The surviving Fortran source of this compiler is the older version 2.0, from
January 1975. Two folders build it and compile the same examples with it:

* [gfortran/](gfortran/README.md) builds it with gfortran, the fastest way to
  run PL/M-80: a compile takes a few milliseconds.
* [dos/](dos/README.md) builds it for MS-DOS with Microsoft Fortran 5.1 in
  ntvdm.

Both builds give the same bytes, and both reproduce the sample HEX compiled on
an IBM mainframe under MTS.

### Version 2.0 and 4.1

A probe of each feature in the 2.0 language summary (`PLMLANG.DOC`), plus
some later PL/M-80 features, compiled with both versions and run in ntvcm,
finds the same language:

* Both accept and give the same results for every documented statement,
  declaration (including `BASED`, `LITERALLY`, `DATA`, `INITIAL` and
  `INTERRUPT n`), operator (including `PLUS`, `MINUS` and embedded `:=`),
  built-in procedure (`LENGTH`, `LAST`, `LOW`, `HIGH`, `DOUBLE`, `ROL`, `ROR`,
  `SCL`, `SCR`, `SHL`, `SHR`, `TIME`, `DEC`, `INPUT`, `OUTPUT`) and
  predefined variable (`CARRY`, `ZERO`, `SIGN`, `PARITY`, `MEMORY`,
  `STACKPTR`). Almost all compile to identical bytes.
* Both reject recursion, `REENTRANT`, `(*)` array dimensions, structures,
  `PUBLIC` and `EXTERNAL`, with the same error numbers. Both have the same
  alphabet: lowercase becomes a space, even inside strings.
* Version 2.0 reads 80 columns, 4.1 only 72.
* Version 4.1 generates slightly better code in places: after a `DO CASE`
  whose branches all leave the same address in HL, it omits the reload, which
  makes ttt three bytes smaller. Version 2.0 writes zero records for the rest
  of a page of variables with `INITIAL` values, which can make its `.COM`
  file longer.

## Runner notes

The PL/M-80 images use a newer Tymshare Fortran runtime than ntv8008's 8008
tools. Besides the accounting wrapper that the runner skips, the runtime sets
up Tymshare's software-interrupt system with monitor calls `CALLI -42` through
`CALLI -33`, and halts unless they succeed. The runner accepts them and never
raises an interrupt; the handlers would run only after a trap such as an
arithmetic overflow. The compiler's code and data are unchanged.

MAC80E and INT80E were built for the KI10 processor. Their runtime checks for
one by running `AOBJN` on -1: a KA10 carries into the left half and a KI10
does not. On a KA10 they print `?KI-10 CODE WILL NOT RUN ON A KA-10`. For
those two images only, the runner selects KI10 halfword arithmetic for `AOBJN`,
`AOBJP`, `PUSH`, `POP`, `PUSHJ` and `POPJ`; `pdp10.cxx` takes `--ki`.

The runner keeps each channel's disk buffer at 0700000 (output) or 0740000
(input), above every image. INT80E's code and data reach 0101032, so it failed
with ntv8008's buffers at 0100000. The PL/M-80 output is byte-identical either
way.

## Tests

```sh
python3 plm80/test.py
```

The tests compile every example and run it in ntvcm in both Z80 and 8080 modes.
They check e against Python's `Decimal`, compare the Python and native runners
on sieve, and check the source checks, compile errors, and HEX output.
Runner tests for the Tymshare monitor calls, KI10 arithmetic and buffer
placement need no compiler images. Build ntvcm first.
