# Intel INTERP/80 1.5 for ntvcm

## Optional local-image component — setup first

INTERP/80 is the 8080 instruction-set simulator that Intel supplied with
MAC80 and PL/M-80 on Tymshare. Like them it is PDP-10 Fortran, and it runs on
the [plm80](../plm80/README.md) runner. `run.py` gives it a minimal CP/M, so the
same `.COM` files run in INTERP/80 and in ntvcm. The simulator executables are
not included or automatically downloaded. Redistribution permission for these
historical Intel/Tymshare images has not been established. Pins and
provenance are in [upstream/NOTICE.txt](upstream/NOTICE.txt).

If you have the right to use them, recover the images from a lawfully obtained
copy of Tymshare tape 169278:

```sh
python3 int80/recover.py /path/to/169278.tape.bz2
```

Recovery verifies the tape's SHA-256, extracts `INT80.SAV` and `INT80E.SAV`
from its `(UPL)` directory, verifies both, and writes them to `int80/upstream/`.
Build the native PDP-10 runner with `plm80/m.sh` (Windows: `plm80\m.bat`). It
simulates six to eight million 8080 states a second; the Python runner is
more than 100 times slower.

## Run

```sh
python3 int80/run.py -p mac80/examples/sieve.com
```

That prints `1899 primes.` and `9240149 states`. The program may be a CP/M
`.COM` file, an Intel HEX file, or MAC80 source, which `run.py` assembles.
`-p` reports the program's own states (below), `-n` sets the limit before it
fails without an exit, and `--expanded` selects INT80E.

INT80 simulates 24K of memory, 0000H–5FFFH; INT80E, the `EXPANDED VERSION OF
INT80 7/20/75`, simulates 48K, to BFFFH. INT80E was built for the KI10 and,
like MAC80E, gets the runner's KI10 mode. Neither simulates a console, so
`run.py` loads this environment with each program:

* Address 5 jumps to the BDOS entry at the top of memory, which jumps once
  more, exactly as in ntvcm. There an 8080 stub handles BDOS function 0
  (exit), 2 (console output) and 9 (print a `$`-terminated string), writing
  each character with `OUT 1`. Other functions fail with an error.
* Address 0 holds `HLT`, which ends a program that jumps or returns to it.
  The stack starts below the BDOS with a return address of 0. A `HLT`
  anywhere else fails, as ntvcm also treats it as an error.
* All other memory is zero, as in ntvcm; INTERP/80 otherwise fills it with
  `HLT`.

`run.py` traces the stub's entry and its `OUT` instructions, recovers each
character from the A register in INTERP/80's trace, and subtracts the stub's
states from INTERP/80's total. Unlike INTERP/8, INTERP/80 adds no states when
a session starts, so the remainder is the program's own states, from its first
instruction to its exit, including its `CALL 5` instructions.

## Validation

`int80/test.py` runs each program in both simulators. ntvcm's 8080 mode
writes a profile of the cycles spent at each address (`-g`), and the test
converts the program's share to INTERP/80 states with the timing differences
below. Every program must print exactly the same output, and the states must
match exactly:

| Program | ntvcm 8080 cycles | INTERP/80 states | Difference |
| --- | ---: | ---: | ---: |
| MAC80 hello | 44 | 44 | 0 |
| MAC80 sieve | 9,240,118 | 9,240,149 | +31 |
| MAC80 e | 4,797,260 | 4,797,260 | 0 |
| MAC80 ttt | 3,353,548 | 3,353,555 | +7 |
| PL/M-80 hello | 204 | 204 | 0 |
| PL/M-80 arithmetic | 17,194 | 17,212 | +18 |
| PL/M-80 sieve | 63,575,021 | 64,466,572 | +891,551 |
| PL/M-80 e | 81,626,900 | 81,686,493 | +59,593 |
| PL/M-80 ttt | 169,821,667 | 170,013,627 | +191,960 |
| ntvcm's `tests/TST8080.COM` | 4,874 | 4,887 | +13 |
| ntvcm's `tests/8080PRE.COM` | 7,787 | 7,795 | +8 |
| ntvcm's `tests/TEST.COM` | 18,390 | 18,403 | +13 |

The ntvcm cycles count only the program, not ntvcm's BDOS, so they are less
than `ntvcm -p` reports. Both Microcosm CPU diagnostics report
`CPU IS OPERATIONAL` in INTERP/80, and 8080PRE completes. A further program
executes every documented instruction except `RST`, with each condition both
taken and not taken, and also matches.

### Timing differences

INTERP/80 times two cases differently from Intel's documentation and ntvcm:

| Instruction | 8080 states (Intel, ntvcm) | INTERP/80 states |
| --- | ---: | ---: |
| `LHLD`, `SHLD` | 16 | 17 |
| conditional call, taken | 17 | 18 |

Intel's 8080 Microcomputer Systems User's Manual (September 1975) gives 16
states for `LHLD` and `SHLD` and 11/17 for conditional calls; INTERP/80 1.5 is
from the same year. Every other instruction matches. The differences explain
the table above: PL/M-80 code keeps its variables in memory and uses `LHLD`
and `SHLD` heavily, for example.

This comparison also found two ntvcm errors, since fixed: its 8080 mode
charged `XCHG` 5 cycles instead of the manual's 4, and a `MOV H,H` that is
not a BDOS or BIOS hook (ntvcm uses opcode 64H for its hooks) 4 cycles
instead of 5. INTERP/80 had both right.

### Flag differences

Every result and flag matches the 8080 except the auxiliary carry (AC) of
five instructions. The test checks 100 random cases of each against ntvcm,
which passes the 8080 exerciser's hardware-derived checks, and checks
INTERP/80's own rules:

* `SUB` and `CMP` negate the operand and then add it, so a subtrahend with a
  low digit of 0 gives AC 0 where the 8080 gives 1.
* `SBB` adds the negated operand and the negated borrow separately.
* `ANA` and `ANI` always clear AC; the 8080 sets it to the OR of the operands'
  bit 3.
* `DAA` leaves AC unchanged; the 8080 sets it to the carry out of bit 3.

Intel's documentation supports the 8080 behavior. The 8080/8085 Assembly
Language Programming Manual describes `SUB` as adding the one's complement
plus one, which always carries out of bit 3 when the subtrahend's low digit is
0; its `DAA` example sets AC from the low-digit adjustment; and it states the
8080's `ANA` rule. The 1975 user's manual lists AC among the flags `ANA` and
`DAA` affect but does not say how, so INTERP/80's `ANA` may follow an early
reading of the specification.

`DAA` depends on AC, so a decimal adjustment after a subtraction or `ANA` can
also give a different result. SuperSoft's `tests/CPUTEST.COM`, which fits in
INT80's 24K, finds the last of these: its test 0067H runs `DAA` on 56H with AC
set, and reports `REGISTER f CONTAINS 16H BUT SHOULD CONTAIN 06H`. ntvcm passes
it. The 8080 exerciser `8080EX1` takes 23.8 billion cycles in ntvcm, about an
hour in INTERP/80, so it and `8080EXER` are not run.

## Tests

```sh
python3 int80/test.py
```

The tests need the INTERP/80 images and a built ntvcm; they take about a
minute and a half. They run the CPU diagnostics and CPUTEST, check the memory
sizes and exits, and load the tape's 1974 `MAC80.EXA` object as its
`INT80.EXA` session does. With the MAC80 images they also run the MAC80
examples, the timing program and the flag cases; with the PL/M-80 images, the
PL/M-80 examples.
