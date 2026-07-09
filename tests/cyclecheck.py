#!/usr/bin/env python3
"""ntvcm Z80 cycle counting-check.

Self-contained (no assembler/toolchain needed): emits tiny raw-byte .COM
programs, each K straight-line copies of one instruction bracketed by fixed
setup, runs `ntvcm -p`, and asserts the measured per-instruction T-state count
(total_K - total_0)/K matches the documented Zilog Z80 value.

Exits 0 if every instruction matches, 1 otherwise.  Textbook values are the
standard Z80 timings (Zilog Z80 CPU User Manual); z88dk-ticks reproduces every
one of them exactly, which is how these expectations were cross-checked.

Usage:  cyclecheck.py [path-to-ntvcm]      (default: ./ntvcm)
"""
import subprocess, sys, os, tempfile

NTVCM = sys.argv[1] if len(sys.argv) > 1 else "./ntvcm"
K = 64

# Fixed scratch/buffer addresses, clear of the (tiny) program at 0x100.
SCR, BUF1, BUF2 = 0x0300, 0x0800, 0x0C00

def w16(a): return [a & 0xFF, (a >> 8) & 0xFF]

# (name, body-bytes, textbook T-states)
CASES = [
    # --- controls (already correct in ntvcm; guard against over-correction) ---
    ("nop",         [0x00],                 4),
    ("ld bc,nn",    [0x01] + w16(0),        10),
    ("ld (nn),hl",  [0x22] + w16(SCR),      16),
    ("inc ix",      [0xDD, 0x23],           10),
    ("push ix",     [0xDD, 0xE5, 0xDD, 0xE1], 15 + 14),  # push+pop pair (balanced)
    # --- ED 16-bit load/store (ntvcm bug: counts 8) ---
    ("ld (nn),bc",  [0xED, 0x43] + w16(SCR), 20),
    ("ld bc,(nn)",  [0xED, 0x4B] + w16(SCR), 20),
    # --- base table (ntvcm bug: 0x2A counts 20) ---
    ("ld hl,(nn)",  [0x2A] + w16(SCR),      16),
    # --- DD/FD indexed (ntvcm bugs) ---
    ("ld a,(ix+0)", [0xDD, 0x7E, 0x00],     19),
    ("ld (ix+0),a", [0xDD, 0x77, 0x00],     19),
    ("inc (ix+0)",  [0xDD, 0x34, 0x00],     23),
    ("dec (ix+0)",  [0xDD, 0x35, 0x00],     23),
    ("ld (ix+0),n", [0xDD, 0x36, 0x00, 0x00], 19),
    ("ld ix,nn",    [0xDD, 0x21] + w16(SCR), 14),
    ("ld (nn),ix",  [0xDD, 0x22] + w16(SCR), 20),
    ("ld ix,(nn)",  [0xDD, 0x2A] + w16(SCR), 20),
    ("dec ix",      [0xDD, 0x2B],           10),
    ("add ix,bc",   [0xDD, 0x09],           15),
    ("add a,(ix+0)",[0xDD, 0x86, 0x00],     19),
    # --- ED block/arith (ntvcm bug: counts 4) ---
    ("ldi",         [0xED, 0xA0],           16),
    ("ldd",         [0xED, 0xA8],           16),
    ("cpi",         [0xED, 0xA1],           16),
    ("cpd",         [0xED, 0xA9],           16),
    ("adc hl,bc",   [0xED, 0x4A],           15),
]

def make_com(body, k):
    # setup: ld ix,SCR / ld hl,BUF1 / ld de,BUF2 / ld bc,0x0040 / or a
    code = ([0xDD, 0x21] + w16(SCR) +
            [0x21] + w16(BUF1) +
            [0x11] + w16(BUF2) +
            [0x01] + w16(0x0040) +
            [0xB7])
    code += body * k
    code += [0xC7]                          # rst 0 -> warm boot (program exit)
    return bytes(code)

def measure(body, k):
    com = make_com(body, k)
    with tempfile.NamedTemporaryFile(suffix=".com", delete=False) as f:
        f.write(com); path = f.name
    try:
        r = subprocess.run([NTVCM, "-p", path], capture_output=True, text=True)
    finally:
        os.unlink(path)
    for line in r.stdout.splitlines():
        if "cycles:" in line:
            return int(line.split()[-1].replace(",", ""))
    raise SystemExit(f"could not parse cycles from ntvcm output:\n{r.stdout}")

def main():
    base = measure([], 0)
    print(f"{'instruction':<14}{'expect':>7}{'ntvcm':>8}   result")
    fails = 0
    for name, body, tv in CASES:
        per = (measure(body, K) - base) / K
        ok = abs(per - tv) < 0.5
        if not ok: fails += 1
        print(f"{name:<14}{tv:>7}{per:>8.1f}   {'ok' if ok else 'FAIL'}")

    # --- repeating block ops: measured over M iterations ---------------------
    # Zilog: each iteration with BC!=0 after = 21 T, final iteration = 16 T,
    # so an M-iteration run costs M*21 - 5.  We compare a program containing the
    # block op against the identical setup without it; the total delta is the
    # block op's whole cost.  Buffers are zero-filled and A=0xFF so cpir/cpdr
    # never match and run the full count.
    M = 100
    expect_block = M * 21 - 5

    def block(setup, op_bytes):
        # setup bytes ... op ... rst0
        com = bytes(setup + op_bytes + [0xC7])
        with tempfile.NamedTemporaryFile(suffix=".com", delete=False) as f:
            f.write(com); path = f.name
        try:
            r = subprocess.run([NTVCM, "-p", path], capture_output=True, text=True)
        finally:
            os.unlink(path)
        for line in r.stdout.splitlines():
            if "cycles:" in line:
                return int(line.split()[-1].replace(",", ""))
        raise SystemExit("no cycles")

    BLK = [
        # name, setup, op-bytes
        ("ldir", [0x21]+w16(BUF1) + [0x11]+w16(BUF2) + [0x01]+w16(M) + [0xB7], [0xED, 0xB0]),
        ("lddr", [0x21]+w16(BUF1+M-1) + [0x11]+w16(BUF2+M-1) + [0x01]+w16(M) + [0xB7], [0xED, 0xB8]),
        ("cpir", [0x3E,0xFF] + [0x21]+w16(BUF1) + [0x01]+w16(M) + [0xB7], [0xED, 0xB1]),
        ("cpdr", [0x3E,0xFF] + [0x21]+w16(BUF1+M-1) + [0x01]+w16(M) + [0xB7], [0xED, 0xB9]),
    ]
    for name, setup, op in BLK:
        with_op = block(setup, op)
        without = block(setup, [])
        cost = with_op - without
        ok = (cost == expect_block)
        if not ok: fails += 1
        print(f"{name+' x'+str(M):<14}{expect_block:>7}{cost:>8}   {'ok' if ok else 'FAIL'}")

    print()
    if fails:
        print(f"FAIL: {fails} instruction(s) miscounted")
        return 1
    print("PASS: all instruction cycle counts correct")
    return 0

if __name__ == "__main__":
    sys.exit(main())
