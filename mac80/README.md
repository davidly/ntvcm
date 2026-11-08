# Intel MAC80 2.4 for ntvcm

## Optional local-image component — setup first

Intel's original MAC80 8080 macro cross-assembler builds programs here for
CP/M, and the resulting `.COM` files run in ntvcm. It shipped with PL/M-80 on
the same Tymshare service and runs on the same PDP-10 runner as
[plm80](../plm80/README.md). The assembler executables are not included or
automatically downloaded. Redistribution permission for these historical
Intel/Tymshare images has not been established. Pins and provenance are in
[upstream/NOTICE.txt](upstream/NOTICE.txt).

If you have the right to use them, recover the images from a lawfully obtained
copy of Tymshare tape 169278:

```sh
python3 mac80/recover.py /path/to/169278.tape.bz2
```

Recovery verifies the tape's SHA-256, extracts `MAC80.SAV` and `MAC80E.SAV`
from its `(UPL)` directory, verifies both, and writes them to `mac80/upstream/`.
Build the native PDP-10 runner with `plm80/m.sh` (Windows: `plm80\m.bat`);
without it the Python runner takes a few seconds per file instead of a tenth
of a second.

## Assemble and run

```sh
python3 mac80/assemble.py mac80/examples/hello.asm
./ntvcm mac80/examples/hello.com
```

That prints `HELLO FROM MAC80!`. `-o file.com` selects the output; an `-o`
path ending in `.hex` writes Intel HEX instead. `--listing file` saves the
assembler listing, and `--expanded` selects MAC80E (below).
`mac80/rebuild.sh` or `rebuild.bat` reassembles every example.

| Example | Output | Z80 cycles | 8080 cycles (`-8`) |
| --- | --- | ---: | ---: |
| hello | `HELLO FROM MAC80!` | 98 | 98 |
| sieve | `1899 primes.`, ten passes over 8191 odd candidates | 9,472,249 | 9,240,472 |
| e | 192 digits of e, then `done.` | 4,697,067 | 4,803,284 |
| ttt | `moves: 6493`, one iteration of alpha-beta tic-tac-toe | 3,267,350 | 3,354,322 |

sieve, e and ttt are the 8080 benchmarks from
[cpm_compilers](https://github.com/davidly/cpm_compilers/tree/main/asm),
written for CP/M's `ASM.COM`. The ports change only spelling (below), and
each assembles to exactly the bytes that `ASM.COM` produces from the original:
the same 307, 1,030 and 964 bytes at the same addresses.

## Source format

MAC80 accepts Intel's 8080 assembly language with macros (`MACRO`/`ENDM`),
conditional assembly (`IF`/`ENDIF`), `EQU`, `SET`, `ORG`, `DB`, `DW`, `DS`,
`$` for the location counter, and the operators `+ - * / MOD NOT AND OR XOR
SHL SHR`. Compared with `ASM.COM` source:

* Symbols keep five significant characters, so `DISPLAYDIGIT` and `DISPLAY`
  are one symbol. MAC80 flags two such labels as multiply defined but silently
  resolves a reference such as `LXI H,LONGNOTE` to a label `LONGNAME`, so the
  wrapper rejects any two distinct names that share their first five
  characters. Mnemonics and directives are reserved and cannot be labels.
* There is no `HIGH`, `LOW` or `EQ`; use `SHR 8` and `AND 0FFH`. `END` takes no
  start address, and `!` does not separate statements.
* `$` cannot appear inside names.

The wrapper also checks the hazards it shares with the PL/M compilers. Code
must use MAC80's alphabet, space through `Z`: other characters, including
lowercase and `_[\]^`, become spaces even inside strings, so lowercase text
must be given as ASCII codes. Comments may contain anything, but the listing
shows lowercase as spaces. Code must fit in 72 columns, because MAC80 ignores
the rest; comments may run past column 72. `END` is appended when missing.
Lines starting with `$` are rejected, because MAC80 reads control commands
from the source and the wrapper selects its files. MAC80 flags errors in the
listing's second column (for example U for an undefined symbol, M for a
multiply defined one, Q for a questionable operand). The wrapper prints the
flagged lines, fails, and leaves any existing output file intact.

## MAC80E, the expanded assembler

`MAC80E.SAV` prints `EXPANDED VERSION OF MAC80 7/20/75` and then
`8080 MACRO ASSEMBLER, VER 2.3`, like ntv8008's MAC8E. Its symbol table is
about eight times larger. All the examples assemble to the same bytes with
both versions. MAC80E was built for the KI10 processor and refuses to run on a
KA10; the runner selects KI10 halfword arithmetic for it (see
[plm80/README.md](../plm80/README.md#runner-notes)).

## Tests

```sh
python3 mac80/test.py
```

The tests need the images and a built ntvcm. They:

* assemble the tape's own 1974 `MAC80.EXA` example and compare the object file
  with the one printed in that session;
* assemble every example with MAC80 and MAC80E, compare each with the pinned
  digest of its bytes (for sieve, e and ttt, `ASM.COM`'s output), and run it in
  ntvcm in Z80 and 8080 modes, checking e against Python's `Decimal`;
* assemble all 244 documented 8080 instructions and compare them with
  encodings derived from the instruction set's opcode fields;
* compare the Python and native runners, and check the source checks, error
  reporting and HEX output.

[INTERP/80](../int80/README.md) runs the assembled programs in Intel's own
8080 simulator.
