# z80test for CP/M

This is a CP/M port of Patrik Rak's [z80test](https://github.com/raxoft/z80test),
a set of programs that exhaustively exercise the Z80 instruction set and
compare results against values recorded from a real 48K ZX Spectrum. It is
meant to be a much harsher correctness check than zexall/zexdoc for `ntvcm`'s
Z80 core.

## Why this isn't just "assemble it and go"

z80test's `idea.asm`/`tests.asm` are laced with absolute addresses that get
baked into the compiled binary at assembly time — most importantly, the
workspace scratch location `mem_`, which every `(HL)`/`(IX+d)`/`(IY+d)`/
`(BC)`/`(DE)` memory-indirect test points a register at. That address (and
several others like it) gets pushed onto the stack and folded directly into
the running CRC, alongside the actual test data.

A CP/M `.COM` always loads and starts at `0x100`. The original Spectrum build
assembles at `org 0x8000`. Since `mem_`'s *address itself* is part of what
gets CRC'd, a naive CP/M port (main/print routines swapped for BDOS calls,
everything else at `org 0x100`) computes a different, but perfectly
self-consistent, CRC for every memory-indirect test — not because of any
emulator bug, but because the recorded "expected" CRCs have the Spectrum's
`0x8000` load address permanently baked into them. This was confirmed by
diffing against a cycle-accurate reference (Fuse) instruction-by-instruction:
every byte matched except the ones that are supposed to differ because they
literally encode the load address.

**The fix**: assemble `idea.asm`/`tests.asm`/`testmacros.asm`/`crctab.asm`
completely unmodified at `org 0x8000`, exactly like the official Spectrum
build. A tiny 14-byte loader stub is what actually gets loaded at CP/M's
mandatory `0x100`; it `LDIR`s the `0x8000`-assembled payload to its real
address and jumps there. This makes every embedded address match the
official build byte-for-byte, so the recorded expected CRCs apply directly
with no re-derivation needed.

## Why `sjasm`, not `sjasmplus`

The source is written for and tested against the original Sjoerd Mastijn
`sjasm` (specifically v0.42c). It relies on macro/local-label semantics that
the modern, actively-maintained `sjasmplus` fork does not replicate — in
particular, macro call arguments (like the `mem`/`hl,mem` idiom used
throughout `tests.asm`) are evaluated in the *caller's* scope on real sjasm,
but `sjasmplus` resolves them against the *called macro's own* parameter
names instead, silently producing wrong values for every memory-indirect
test. Getting this right required extensive rewriting under `sjasmplus`;
using real sjasm lets every upstream file stay untouched. (This is also
independently confirmed by upstream: see
[issue #1](https://github.com/raxoft/z80test/issues/1) on the z80test repo,
where another contributor hit the identical "many error messages" wall with
`sjasmplus` and confirmed switching to real `sjasm` fixed it.)

Real `sjasm` 0.42c doesn't build out of the box on a modern 64-bit
Linux/g++ toolchain — two small, genuine bugs in *its own* source get in the
way (a missing `<cstring>` include, and a 64-bit portability bug in
`rawsource.cpp` where `string::npos`, a 64-bit value, was truncated through
`unsigned` in a way that only coincidentally worked on 32-bit builds).
`patches/sjasm-0.42c-linux64-build-fix.patch` fixes both; `build.sh` applies
it automatically.

## Building

```
./build.sh              # builds all six variants
./build.sh doc ccf       # or just specific ones
```

Variants (matching upstream): `full`, `doc`, `flags`, `docflags`, `ccf`,
`memptr`. (The Spectrum-screen-visualization `z80ccfscr` variant isn't
included — it has no CP/M equivalent.) The first run clones and builds
`sjasm` into `.sjasm-build/` (one-time, a few seconds); output is
`<variant>.com` in this directory.

Requires: `git`, a C++ compiler (`g++`), `make`. No network access is needed
beyond the one-time `sjasm` clone — all of z80test's own source is vendored
in `src/` under its original MIT license (see `UPSTREAM-LICENSE.txt`).

## Running

```
ntvcm doc.com
```

Or against any other CP/M emulator (`cpmemu`, `iz-cpm`, `zxcc`, `tnylpo`,
`z88dk`'s `cpm`, etc.) — same invocation.

## Expected results — read this before filing a bug

Not every "FAILED" line here means the emulator's Z80 core is wrong. z80test
deliberately probes some of the most obscure corners of real Z80 hardware,
and several of those corners are either not reproducible under CP/M at all,
or are matters of genuine disagreement between real Z80 chips. Specifically:

**`doc` / `docflags`** (documented flags only) are the two variants that
should be judged as pass/fail signal for a normal CP/M emulator. On a
correct core, expect these caveats and nothing else:

- **Tests that read `IN A,(0xFE)`** (`IN A,(N)`, `IN R,(C)`, `INI`/`IND`/
  `INIR`/`INDR` and friends) check the real Spectrum ULA's floating-bus
  behavior on an unconnected port, which simply doesn't exist under CP/M.
  The port stub in `print.asm`/`main.asm` deliberately leaves this check
  untouched rather than faking a Spectrum-specific answer, so it honestly
  reports `FAILED` with `IN FE:FF Expected:BF`. This is by design, not a bug.

- **`LD A,R` is inherently non-portable across different emulators and even
  different runs.** Its undocumented flags reflect the exact value of the
  Z80 memory-refresh register `R` at the moment it's read. `R` is reset to a
  known value once, at the very start of each individual test — but from
  there it free-runs, incrementing once per opcode fetch (M1 cycle) for the
  rest of that test's entire combinatorial sweep, which can be many
  thousands of instructions. By the time a later test like `LD A,R` runs, R's
  value depends on the *total* number of M1 cycles every single earlier test
  in the suite actually executed. Anything that changes that count even
  slightly — a different number of wait states, a different block-I/O
  timing model, or simply skipping vs. running one of the `IN`-dependent
  tests above — shifts R's value at this point and changes the expected
  flags. This is not a correctness signal for the CPU core; it's a signal
  about exact cumulative instruction-count parity with whatever machine
  originally recorded the expected CRC, which no two independently-built
  emulators (or the real Spectrum vs. any emulator) are ever going to have
  by coincidence. Don't chase this one.

- **`LDIR->NOP'`/`LDDR->NOP'` (tests 089/090) require two separate things to
  get right, and most emulators get neither for free.**

  First: *any* `ED`-prefixed byte that isn't one of the specifically
  documented/duplicated opcodes acts as two plain `NOP`s (8 T-states, R+2,
  nothing else). This is a blanket rule covering the whole undocumented `ED`
  space, not just the block-instruction-adjacent bytes (`0xA4-0xA7`,
  `0xAC-0xAF`, `0xB4-0xB7`, `0xBC-0xBF`) — an emulator that only special-cases
  those and still hard-errors on everything else outside the documented set
  (e.g. `0xED 0x09`) will crash partway through this test rather than
  produce a wrong answer.

  Second, and much sharper: these two tests deliberately make `LDIR`/`LDDR`
  overwrite *their own opcode bytes* mid-repeat (`DE` is pointed at the
  instruction's own second byte, `BC` starts at `0` for a nominal 65536
  reps). Real Z80 hardware never advances `PC` during a block-instruction
  repeat — it re-fetches and re-decodes the same two bytes from memory on
  *every single iteration*, since the repeat is really just "don't advance
  PC, do another M1 cycle." The instant the instruction's own destination
  write overwrites those bytes (which, worked out by hand, happens after
  exactly one iteration here, since `DE` starts one byte past the opcode),
  the very next re-fetch sees different bytes and the instruction stops
  being `LDIR`. An emulator that executes the whole repeat as one internal,
  cached-opcode loop (a natural and otherwise-harmless optimization, since
  self-modifying block-repeat instructions essentially never occur in real
  software) never notices this, keeps running the *original* `LDIR`
  semantics for the full repeat count, and ends up smearing memory across
  address ranges — including its own subsequent code — that real hardware
  never would have touched. Whether that manifests as a wrong CRC, a crash,
  or an outright hang depends on what the corrupted memory happens to
  decode as afterward.

  If your emulator implements block-repeat instructions this way (`ntvcm`
  did, until this was tracked down), fixing it doesn't require rearchitecting
  the whole repeat into a slow, fully re-entrant fetch loop: check, once per
  iteration, whether the opcode's own two bytes in memory still match what
  was originally fetched; if they still match, keep looping exactly as
  before (the overwhelmingly common case, at the cost of one cheap
  comparison per iteration); if they don't, stop the repeat immediately and
  rewind `PC` by 2 so the normal fetch/decode cycle picks up the new bytes
  next. One subtlety that's easy to get wrong even after that: these
  instructions' flags (`P/V` for `LDIR`/`LDDR`, `Z` for the I/O `xxIR`/`xxDR`
  variants) are specified per-iteration as a function of the counter's
  *current* value after each decrement, not as a constant tied to "the loop
  finished." Code that hard-codes the flag to the value that's only correct
  for normal completion (`BC==0`/`B==0`) will still get the wrong answer
  even after the repeat itself terminates correctly, because an
  interrupted-by-self-modification exit leaves that counter nonzero.

**`full` / `flags`** (undocumented flags too) additionally expose the
long-standing, hardware-level **SCF/CCF undocumented-flag ambiguity**: real
Z80 chips themselves disagree on bits 3/5 of the flags register after `SCF`/
`CCF`, depending on whether the preceding instruction touched the flags, and
this reportedly also varies by CPU manufacturer/board. z80test's own
`readme.txt` says as much. Expect `SCF`/`CCF`-adjacent failures here even on
a correct, self-consistent emulator, unless it happens to emulate the exact
same hardware variant the recorded CRCs came from.

**`ccf`** inserts a `CCF` after *every* tested instruction and checks flags
afterward — i.e. it's maximally sensitive to the SCF/CCF ambiguity above.
Upstream's own `readme.txt` says outright: *"it will fail half of the tests
on CPUs which use other variant, so don't bother."* Widespread failures here
are expected and are not a useful signal on their own.

**`memptr`** exhaustively tests the obscure Z80 "MEMPTR"/`WZ` internal
register's effect on `BIT n,(HL)`/`BIT n,(IX+d)`'s undocumented flags, by
appending a `BIT 0,(HL)` probe after *every single instruction in the whole
suite*. MEMPTR is one of the least-commonly-emulated pieces of Z80 internal
state; most emulators fail this variant almost universally as a result.
Upstream's own `readme.txt` calls this out directly: it exists specifically
*"to discover major problems in the MEMPTR emulation."* A near-total failure
here means "this emulator doesn't implement MEMPTR," which is a real and
useful thing to know, but is a single, well-understood gap, not 89
independent bugs. `ntvcm` implements MEMPTR tracking behind the
`TRACK_Z80_MEMPTR` switch (off by default — see below), so it can score
either end of this range depending on the build.

## Layout

```
src/
  idea.asm, tests.asm, testmacros.asm, crctab.asm   - upstream, unmodified
  z80full.asm, z80doc.asm, z80flags.asm,
  z80docflags.asm, z80ccf.asm, z80memptr.asm        - upstream, unmodified
  main.asm, print.asm                               - CP/M port (org 0x8000,
                                                       BDOS-based printing)
  stub.asm                                          - the org-0x100 CP/M
                                                       loader/relocator
patches/
  sjasm-0.42c-linux64-build-fix.patch                - build-only fix, see above
build.sh
UPSTREAM-LICENSE.txt                                 - z80test's original MIT license
```

`main.asm`/`print.asm` are the only files that differ from upstream: they
replace the Spectrum ROM's `CHAN-OPEN`/`RST 0x10` print routines with CP/M
BDOS calls (function 2, write character), replace the BASIC-return epilogue
with a CP/M warm-boot jump, and print plain text instead of using the
Spectrum ROM's `CHR$23`/`CHR$127` screen-positioning control codes. Every
other byte of program logic is identical to upstream, verified by diffing
against the official release's own `.tap` file.

## Emulator comparison

Results of running `doc.com` (160 tests total, 4 of which are always
`Skipped` early on regardless of anything below — they're gated to only run
after some earlier failure, and nothing fails that early) against every
CP/M emulator this port has been validated against:

| Emulator | OK | Failed | Skipped |
|---|---|---|---|
| tnylpo | 147 | 9 | 4 |
| ntvcm (`TRACK_Z80_R_REGISTER` set to `1`, see below) | 147 | 9 | 4 |
| ntvcm (default build) | 146 | 10 | 4 |
| zxcc | 142 | 14 | 4 |
| cpmemu | 141 | 15 | 4 |
| iz-cpm | 141 | 15 | 4 |
| RunCPM | 81 | 4 | 4 (halts at test 089 on an unimplemented opcode) |
| z88dk (`cpm`) | 8 | 0 | 4 (crashes at test 012 on an unimplemented opcode) |
| cpm.exe (Takeda Toshiya's CP/M Player) | 0 | 0 | — (crashes on the test's first unconditional `OUT`; can't run z80test at all) |

Every failure beyond the 9 unavoidable `IN A,(0xFE)` ones is a real,
specific gap in that emulator's Z80 core (see "Expected results" above for
what's expected and why) — not noise. tnylpo and ntvcm (with the switch
below flipped) are the only two that get everything else right.

**Note on ntvcm**: it leaves the Z80 `R` (memory-refresh) register
untracked by default, for performance — a deliberate, measured ~4.6%
runtime cost across every instruction if fully tracked, and almost no real
CP/M software ever reads `R`'s exact value. `LD A,R` (test 157) is the
*only* test in the entire z80test suite this affects; with `R` frozen it
fails, accounting for the whole 146-vs-147 difference above. To get it
back, set `TRACK_Z80_R_REGISTER` to `1` near the top of the `registers`
struct in `x80.hxx` and rebuild — every other result is already identical
either way.

**Note on ntvcm and MEMPTR**: the same applies to the undocumented internal
`MEMPTR`/`WZ` register (see "Expected results" above), gated behind its own
`TRACK_Z80_MEMPTR` switch, off by default. Nearly every instruction that
computes a 16-bit address writes MEMPTR, so tracking it touches a much
broader swath of the interpreter than `R` does; the measured cost is
~5% runtime across `zexall`, noisier but roughly in the same range as the
`R` cost above. Running `memptr.com` (160 tests) against each build:

| ntvcm build | OK | Failed | Skipped |
|---|---|---|---|
| `TRACK_Z80_MEMPTR` set to `1` | 143 | 13 | 4 |
| default build | 0 | 160 | 0 |

With the switch off, MEMPTR never changes from its power-on value, so
essentially every `BIT 0,(HL)` probe in the test disagrees with real
hardware — the expected "doesn't implement MEMPTR" result described above.
(Even test 000's `SELF TEST` fails in the default build, since it too ends
in a MEMPTR-sensitive `BIT 0,(HL)` probe; that failure trips the SCF/CCF
variant-detection gating that normally `Skip`s tests 003-006 in `doc.com`,
so this build fails those 4 outright instead of skipping them — hence 0
`Skipped` here versus 4 in every other table on this page.)
With the switch on, the only 13 remaining failures are `IN A,(N)`, `IN
R,(C)`, `IN (C)`, `INI`, `IND`, `INIR`, `INDR`, `INIR->NOP'`, `INDR->NOP'`,
`OUTI`, `OUTD`, `OTIR`, and `OTDR` — the same pre-existing "no real I/O
device attached" floating-bus gap that also accounts for 9 of `doc.com`'s
10 default-build failures, not a MEMPTR bug. Set `TRACK_Z80_MEMPTR` to `1`
near `TRACK_Z80_R_REGISTER` in `x80.hxx` and rebuild to get this back;
`doc.com`, `zexall`, `zexdoc`, and `zexsup` results are unaffected either
way.
