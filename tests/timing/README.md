# Z80 cycle-timing regression tests

These are minimal, hand-assembled `.com` programs that pin down the exact
Z80 T-state cost of specific opcodes. Each was used to find and verify a
real bug in `x80.cxx`'s cycle model (see the "Fix Z80 T-state
undercounting" commit). The expected values below were computed by hand
from the official Z80 timing tables and cross-checked against
`z88dk-ticks` (`z88dk/src/ticks`), an independent Z80 emulator used as
ground truth, running the same bytes as a raw (non-CP/M) image with
`-pc 100 -end 0`.

Run `./run.sh` (needs `../../ntvcm` built) to check the current binary
against the expected values.

## djnz_loop.com (5 bytes, org 0x100)

```
0100  06 00        LD   B, 0
0102  10 fe        DJNZ 0102          ; loop: -2 = jump to self
0104  c9           RET
```

B starts at 0, so `DJNZ` runs 256 times: 255 taken (B: 0xFF..0x01) + 1
not-taken (B: 0x01->0x00). Expected: `7 + 255*13 + 1*8 + 10 = 3340` T.

Bug found: `case 0x10` in `z80_emulate()` used `cycles = 3/2` (an
M-cycle-count-shaped placeholder) instead of the real T-state cost
`13/8` for taken/not-taken. Every relative jump (`JR`/`JR cc`,
0x18/0x20/0x28/0x30/0x38) had the same class of bug and was fixed at the
same time, though there is no dedicated test file for those here (same
code shape, same fix, verified manually during the investigation).

ntvcm reports 3364, not 3340: it is a real CP/M emulator and actually
executes 2 extra `JP` instructions in the warm-boot chain after this
program's `RET` (a `.com`'s `RET` returns to address 0, which CP/M reboots
via), which `z88dk-ticks`'s minimal harness does not execute at all. This
+24 T constant offset is expected and is not part of what these tests
check (only the delta between "loop ran the wrong opcode costs" and
"loop ran the right opcode costs" matters, i.e. `RESULT - 24 == 3340`).

## jp_cc_taken.com (7 bytes, org 0x100)

```
0100  06 00        LD   B, 0
0102  05           DEC  B
0103  c2 02 01     JP   NZ, 0102
0106  c9           RET
```

255 taken + 1 not-taken `JP NZ`. Expected: `7 + 256*4 + 255*10 + 1*10 + 10
= 3601` T (+24 warm-boot overhead = 3625).

This program is dominated by *taken* conditional jumps, so it barely
moves across the fix (the bug only affected the not-taken cost) -- it is
included as a control: a real regression here (beyond the single
not-taken occurrence, i.e. more than +/-6 T) would indicate a fix broke
the taken-jump cost, which none of the fixes should touch.

## jp_cc_not_taken.com (9 bytes, org 0x100)

```
0100  06 00        LD   B, 0
0102  af           XOR  A
0103  c2 06 01     JP   NZ, 0106      ; never taken (A==0 after XOR A)
0106  10 fa        DJNZ 0102
0108  c9           RET
```

This is the key repro: 256 iterations of a *never-taken* `JP NZ`, plus a
`DJNZ` (255 taken + 1 not-taken) to close the loop. Expected:
`7 + 256*(4+10) + 255*13 + 1*8 + 10 = 6924` T (+24 warm-boot overhead =
6948).

Bug found: conditional `JP` (`0xc2/../0xfa`) subtracted a shared
`cyclesnt = 6` constant from the not-taken cost. That value is only
correct for conditional `RET` (Z80/8080: taken 11, not-taken 5, diff 6);
`JP cc` costs the *same* 10 T whether taken or not, so the subtraction
should not happen at all. With 256 not-taken occurrences the effect is
large and easy to see: pre-fix vs post-fix numbers below differ by far
more than the DJNZ contribution alone.

## Red/green validation (2026-07-05)

Built ntvcm from the commit immediately before the fix (`ab29a7e`, "fix
warning when building with watcom for DOS") and from the fix commit
(`5bc76e5`), ran all three tests against both:

| test                | pre-fix (red) | post-fix (green) | expected (+24 overhead) |
|---------------------|--------------:|------------------:|-------------------------:|
| djnz_loop.com        |           808 |              3,364 |                     3,364 |
| jp_cc_taken.com      |         3,619 |              3,625 |                     3,625 |
| jp_cc_not_taken.com  |         2,856 |              6,948 |                     6,948 |

`jp_cc_taken.com` moves only 6 T (matching the single not-taken `JP NZ`
occurrence in that program * the old `cyclesnt` bug) -- consistent with
the "taken-jump cost is unaffected" analysis above. The other two move
by thousands of T-states, matching the DJNZ/JR and conditional-JP bugs
respectively. All three match hand-calculated expected values exactly
after the fix.
