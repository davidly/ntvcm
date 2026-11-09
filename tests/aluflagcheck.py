#!/usr/bin/env python3
"""ntvcm exhaustive flag check for the 8080 and Z80 arithmetic instructions.

Self-contained like cyclecheck.py: for each instruction it emits a raw-byte
.COM program that runs every combination of inputs (A, operand or F, and the
carry), printing the result and F in hex, and compares that with a Python
model of the documented flag rules - Z80 (Zilog manual plus Sean Young's
"The Undocumented Z80 Documented" for Y and X), or 8080 (Intel manual).
The programs use only 8080 instructions apart from the one tested, and print
with a table rather than DAA, so they run in both modes.

The 8-bit operations are exhaustive; ADD/ADC/SBC HL use 48 boundary and
random values for each operand. Exits 0 if everything matches, 1 otherwise.

Usage:  aluflagcheck.py [path-to-ntvcm]      (default: ./ntvcm)
"""
import os, random, subprocess, sys, tempfile

NTVCM = sys.argv[1] if len(sys.argv) > 1 else "./ntvcm"

def parity(v): return bin(v & 0xFF).count("1") % 2 == 0

# ---------------------------------------------------------------- a small 8080 assembler

class Asm:
    def __init__(self):
        self.code, self.labels, self.fixups = [], {}, []
    def here(self): return 0x100 + len(self.code)
    def label(self, name): self.labels[name] = self.here()
    def db(self, *b): self.code += list(b)
    def word(self, op, target):  # op + 16-bit address or label
        self.code.append(op)
        if isinstance(target, str):
            self.fixups.append((len(self.code), target))
            self.code += [0, 0]
        else:
            self.code += [target & 0xFF, target >> 8]
    def align(self):
        while self.here() & 0xFF:
            self.code.append(0)
    def build(self):
        if "cell" in self.labels:
            self.labels["cellm5"] = self.labels["cell"] - 5
        for at, name in self.fixups:
            a = self.labels[name]
            self.code[at:at + 2] = [a & 0xFF, a >> 8]
        return bytes(self.code)

def printing(a):
    """prbyte prints A as two hex digits; it keeps BC, DE and HL."""
    a.label("prbyte")
    a.db(0xF5, 0x0F, 0x0F, 0x0F, 0x0F)          # push psw / rrc x4
    a.word(0xCD, "prnib")                        # call prnib
    a.db(0xF1)                                   # pop psw
    a.label("prnib")
    a.db(0xC5, 0xD5, 0xE5, 0xE6, 0x0F, 0x5F, 0x16, 0x00)  # push b,d,h / ani 0fh / mov e,a / mvi d,0
    a.word(0x21, "hex")                          # lxi h,hex
    a.db(0x19, 0x5E, 0x0E, 0x02)                 # dad d / mov e,m / mvi c,2
    a.word(0xCD, 5)                              # call 5
    a.db(0xE1, 0xD1, 0xC1, 0xC9)                 # pop h,d,b / ret
    a.label("hex")
    a.db(*b"0123456789ABCDEF")

def program8(op, ftable, operands=True, cpi=False, pre=(), post=()):
    """Run op for every F in ftable, every A, and (if operands) every E; print A and F. pre runs
    after A and F are pushed, so it may use A and HL; post runs with A and F in H and L."""
    a = Asm()
    a.word(0x31, 0xE000)                         # lxi sp,0e000h
    a.db(0x06, 0x00)                             # mvi b,0: the F index
    a.label("floop")
    a.db(0x16, 0x00)                             # mvi d,0: A
    a.label("aloop")
    a.db(0x1E, 0x00)                             # mvi e,0: the operand
    a.label("eloop")
    if cpi:
        a.db(0xC5)                               # push b (cpi changes bc)
    a.word(0x21, "ftab")                         # lxi h,ftab
    a.db(0x78, 0x85, 0x6F, 0x7E, 0x6F, 0x62, 0xE5)  # mov a,b / add l / mov l,a / mov a,m / mov l,a / mov h,d / push h
    if cpi:
        a.db(0x7B)                               # mov a,e
        a.word(0x32, "cell")                     # sta cell
        a.word(0x21, "cell")                     # lxi h,cell
        a.word(0x01, 2)                          # lxi b,2
    for part in pre:
        part(a) if callable(part) else a.db(*part)
    a.db(0xF1)                                   # pop psw: A = d, F = ftab[b]
    for part in op:
        part(a) if callable(part) else a.db(part)
    a.db(0xF5, 0xE1)                             # push psw / pop h
    for part in post:
        part(a) if callable(part) else a.db(*part)
    a.db(0x7C)                                   # mov a,h
    a.word(0xCD, "prbyte")
    a.db(0x7D)                                   # mov a,l
    a.word(0xCD, "prbyte")
    if cpi:
        a.db(0xC1)                               # pop b
    if operands:
        a.db(0x1C)                               # inr e
        a.word(0xC2, "eloop")                    # jnz eloop
    a.db(0x14)                                   # inr d
    a.word(0xC2, "aloop")
    a.db(0x04, 0x78, 0xFE, len(ftable) & 0xFF)   # inr b / mov a,b / cpi n (256 wraps to 0)
    a.word(0xC2, "floop")
    a.word(0xC3, 0)                              # jmp 0
    printing(a)
    a.label("cell")
    a.db(0)
    a.align()
    a.label("ftab")
    a.db(*ftable)
    return a.build()

VALUES = [0x0000, 0x0001, 0x0002, 0x000F, 0x0010, 0x007F, 0x0080, 0x00FF, 0x0100, 0x07FF, 0x0800, 0x0801,
          0x0FFE, 0x0FFF, 0x1000, 0x1001, 0x1FFF, 0x7FFE, 0x7FFF, 0x8000, 0x8001, 0x8FFF, 0xEFFF, 0xF000,
          0xF7FF, 0xF800, 0xFFF0, 0xFFFE, 0xFFFF]
rng = random.Random(4004)
VALUES += [rng.randrange(0x10000) for _ in range(48 - len(VALUES))]

def program16(op, ix=False):
    """Run op (HL op DE, or IX op DE) for both carries and every pair of VALUES; print the result and F."""
    a = Asm()
    a.word(0x31, 0xE000)
    a.db(0x3E, 0x00); a.word(0x32, "ci")         # mvi a,0 / sta ci
    a.label("cl")
    a.db(0x3E, 0x00); a.word(0x32, "ii")
    a.label("il")
    a.db(0x3E, 0x00); a.word(0x32, "jj")
    a.label("jl")
    a.word(0x3A, "jj"); a.db(0x87, 0x5F, 0x16, 0x00)   # lda jj / add a / mov e,a / mvi d,0
    a.word(0x21, "vtab"); a.db(0x19, 0x5E, 0x23, 0x56, 0xD5)  # lxi h,vtab / dad d / mov e,m / inx h / mov d,m / push d
    a.word(0x3A, "ii"); a.db(0x87, 0x4F, 0x06, 0x00)   # lda ii / add a / mov c,a / mvi b,0
    a.word(0x21, "vtab"); a.db(0x09, 0x7E, 0x23, 0x66, 0x6F, 0xD1)  # lxi h,vtab / dad b / mov a,m / inx h / mov h,m / mov l,a / pop d
    if ix:
        a.db(0xE5, 0xDD, 0xE1)                   # push h / pop ix
    a.word(0x3A, "ci"); a.db(0x1F)               # lda ci / rar: carry = ci
    a.db(*op)
    if ix:
        a.db(0xF5, 0xDD, 0xE5, 0xE1, 0xF1)       # push psw / push ix / pop h / pop psw
    a.db(0xF5, 0x7C); a.word(0xCD, "prbyte")     # push psw / mov a,h / call prbyte
    a.db(0x7D); a.word(0xCD, "prbyte")           # mov a,l / call prbyte
    a.db(0xE1, 0x7D); a.word(0xCD, "prbyte")     # pop h / mov a,l (F) / call prbyte
    for var, limit, target in (("jj", len(VALUES), "jl"), ("ii", len(VALUES), "il"), ("ci", 2, "cl")):
        a.word(0x3A, var); a.db(0x3C); a.word(0x32, var); a.db(0xFE, limit); a.word(0xC2, target)
    a.word(0xC3, 0)
    printing(a)
    for name in ("ci", "ii", "jj"):
        a.label(name); a.db(0)
    a.label("vtab")
    for v in VALUES:
        a.db(v & 0xFF, v >> 8)
    return a.build()

# ---------------------------------------------------------------- the models

S, Z, Y, H, X, P, N, C = 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01

def z80_szyx(r):
    return (r & (S | Y | X)) | (Z if r == 0 else 0)

def z80_alu(kind, a, x, c):
    if kind in ("add", "adc"):
        cin = c if kind == "adc" else 0
        r = a + x + cin
        h = (a & 15) + (x & 15) + cin > 15
        v = (~(a ^ x) & (a ^ r) & 0x80) != 0
        res = r & 0xFF
        return res, z80_szyx(res) | (H if h else 0) | (P if v else 0) | (C if r > 255 else 0)
    if kind in ("sub", "sbc", "cp"):
        cin = c if kind == "sbc" else 0
        r = a - x - cin
        h = (a & 15) < (x & 15) + cin
        v = ((a ^ x) & (a ^ r) & 0x80) != 0
        res = r & 0xFF
        f = (res & S) | (Z if res == 0 else 0) | (H if h else 0) | (P if v else 0) | N | (C if r < 0 else 0)
        f |= (x if kind == "cp" else res) & (Y | X)  # cp takes Y and X from the operand
        return (a if kind == "cp" else res), f
    res = {"and": a & x, "xor": a ^ x, "or": a | x}[kind]
    return res, z80_szyx(res) | (H if kind == "and" else 0) | (P if parity(res) else 0)

def z80_single(kind, a, f):
    c = f & C
    if kind == "inc":
        r = (a + 1) & 0xFF
        return r, z80_szyx(r) | (H if (a & 15) == 15 else 0) | (P if a == 0x7F else 0) | c
    if kind == "dec":
        r = (a - 1) & 0xFF
        return r, z80_szyx(r) | (H if (a & 15) == 0 else 0) | (P if a == 0x80 else 0) | N | c
    if kind == "neg":
        return z80_alu("sub", 0, a, 0)
    if kind == "cpl":
        r = a ^ 0xFF
        return r, (f & (S | Z | P | C)) | (r & (Y | X)) | H | N
    if kind == "scf":
        return a, (f & (S | Z | P)) | (a & (Y | X)) | C
    if kind == "ccf":
        return a, (f & (S | Z | P)) | (a & (Y | X)) | (H if c else 0) | (0 if c else C)
    if kind == "daa":
        hin, nin = f & H, f & N
        diff = 0
        if hin or (a & 15) > 9:
            diff |= 0x06
        cout = c or a > 0x99
        if cout:
            diff |= 0x60
        if nin:
            r = (a - diff) & 0xFF
            h = hin and (a & 15) < 6
        else:
            r = (a + diff) & 0xFF
            h = (a & 15) > 9
        return r, z80_szyx(r) | (H if h else 0) | (P if parity(r) else 0) | nin | (C if cout else 0)

def z80_cpi(a, m, f):
    r = (a - m) & 0xFF
    h = (a & 15) < (m & 15)
    n = (r - (1 if h else 0)) & 0xFF
    return a, (r & S) | (Z if r == 0 else 0) | (H if h else 0) | P | N | (f & C) | (Y if n & 2 else 0) | (n & X)

def z80_cb(kind, x, f):
    """The CB rotates and shifts of x, and BIT n (kind 'bitN')."""
    c = f & C
    if kind.startswith("bit"):
        bit = int(kind[3:])
        zero = not (x >> bit) & 1
        return x, ((S if bit == 7 and not zero else 0) | (Z | P if zero else 0) | H | c | (x & (Y | X)))
    out = {"rlc": x >> 7, "rl": x >> 7, "sla": x >> 7, "sll": x >> 7}.get(kind, x & 1)
    r = {"rlc": (x << 1) | (x >> 7), "rrc": (x >> 1) | (x << 7), "rl": (x << 1) | c, "rr": (x >> 1) | (c << 7),
         "sla": x << 1, "sra": (x >> 1) | (x & 0x80), "sll": (x << 1) | 1, "srl": x >> 1}[kind] & 0xFF
    return r, z80_szyx(r) | (P if parity(r) else 0) | (C if out else 0)

def z80_rotate_a(kind, a, f):
    """RLCA, RRCA, RLA, RRA: S, Z and P/V are kept; H and N cleared."""
    r, cb = z80_cb({"rlca": "rlc", "rrca": "rrc", "rla": "rl", "rra": "rr"}[kind], a, f)
    return r, (f & (S | Z | P)) | (r & (Y | X)) | (cb & C)

def i8080_rotate_a(kind, a, f):
    r, cb = z80_cb({"rlca": "rlc", "rrca": "rrc", "rla": "rl", "rra": "rr"}[kind], a, f)
    return r, (f & (S | Z | H | P)) | 2 | (cb & C)

def z80_rld(kind, a, m, f):
    if kind == "rld":
        r = (a & 0xF0) | (m >> 4)
    else:
        r = (a & 0xF0) | (m & 15)
    return r, z80_szyx(r) | (P if parity(r) else 0) | (f & C)

def i8080_f(r, ac, cy):
    return (r & S) | (Z if r == 0 else 0) | (H if ac else 0) | (P if parity(r) else 0) | 2 | (C if cy else 0)

def i8080_alu(kind, a, x, c):
    if kind in ("add", "adc"):
        cin = c if kind == "adc" else 0
        r = a + x + cin
        return r & 0xFF, i8080_f(r & 0xFF, (a & 15) + (x & 15) + cin > 15, r > 255)
    if kind in ("sub", "sbc", "cp"):
        cin = c if kind == "sbc" else 0
        r = a - x - cin
        ac = (a & 15) + (~x & 15) + (1 - cin) > 15  # a + ~x + 1: the carry out of bit 3
        return (a if kind == "cp" else r & 0xFF), i8080_f(r & 0xFF, ac, r < 0)
    res = {"and": a & x, "xor": a ^ x, "or": a | x}[kind]
    return res, i8080_f(res, kind == "and" and ((a | x) & 8) != 0, False)

def i8080_single(kind, a, f):
    c = f & C
    if kind == "inc":
        r = (a + 1) & 0xFF
        return r, i8080_f(r, (r & 15) == 0, c)
    if kind == "dec":
        r = (a - 1) & 0xFF
        return r, i8080_f(r, (r & 15) != 15, c)
    keep = (f & (S | Z | H | P | C)) | 2
    if kind == "cpl":
        return a ^ 0xFF, keep
    if kind == "scf":
        return a, keep | C
    if kind == "ccf":
        return a, keep ^ C
    if kind == "daa":
        diff, cy = 0, c
        if (a & 15) > 9 or f & H:
            diff += 6
        if (a >> 4) > 9 or c or ((a >> 4) >= 9 and (a & 15) > 9):
            diff += 0x60
            cy = 1
        r = (a + diff) & 0xFF
        return r, i8080_f(r, (a & 15) + (diff & 15) > 15, cy)

def model16(mode, kind, hl, de, c):
    if kind == "add":
        r = hl + de
        f = (H if (hl & 0xFFF) + (de & 0xFFF) > 0xFFF else 0) | (C if r > 0xFFFF else 0)
        f |= ((r >> 8) & (Y | X)) if mode == "z80" else 2
        return r & 0xFFFF, f
    if kind == "adc":
        r = hl + de + c
        h = (hl & 0xFFF) + (de & 0xFFF) + c > 0xFFF
        v = (~(hl ^ de) & (hl ^ r) & 0x8000) != 0
    else:
        r = hl - de - c
        h = (hl & 0xFFF) < (de & 0xFFF) + c
        v = ((hl ^ de) & (hl ^ r) & 0x8000) != 0
    res = r & 0xFFFF
    f = ((res >> 8) & (S | Y | X)) | (Z if res == 0 else 0) | (H if h else 0) | (P if v else 0)
    f |= (N if kind == "sbc" else 0) | (C if r > 0xFFFF or r < 0 else 0)
    return res, f

# ---------------------------------------------------------------- the cases

ALU = {"add": 0x83, "adc": 0x8B, "sub": 0x93, "sbc": 0x9B, "and": 0xA3, "xor": 0xAB, "or": 0xB3, "cp": 0xBB}
SINGLE = {"inc": [0x3C], "dec": [0x3D], "cpl": [0x2F], "daa": [0x27], "scf": [0x37], "ccf": [0x3F]}
ALL_F = list(range(256))

def cases(mode):
    out = []
    for kind, op in ALU.items():
        model = z80_alu if mode == "z80" else i8080_alu
        expect = lambda kind=kind, model=model: "".join(
            f"{r:02X}{f:02X}" for c in (0, 1) for a in range(256) for x in range(256)
            for r, f in [model(kind, a, x, c)])
        out.append((f"{kind} a,e", program8([op], [0, 1]), expect, None))
    singles = dict(SINGLE, **({"neg": [0xED, 0x44]} if mode == "z80" else {}))
    for kind, op in singles.items():
        model = z80_single if mode == "z80" else i8080_single
        mask = None if mode == "z80" else 0xD7  # 8080: bits 5 and 3 of a pushed F are fixed
        expect = lambda kind=kind, model=model: "".join(
            f"{r:02X}{f:02X}" for f0 in ALL_F for a in range(256) for r, f in [model(kind, a, f0)])
        out.append((kind, program8(op, ALL_F, operands=False), expect, mask))
    # Other ways to reach the same operations: immediate, memory and indexed operands, other registers.
    alu = z80_alu if mode == "z80" else i8080_alu
    def immediate(a):
        a.db(0x7B); a.word(0x32, "imm")          # mov a,e / sta imm: the next instruction's operand
    def at_imm(a):
        a.labels["imm"] = a.here() + 1
    def cell_e(a):
        a.db(0x7B); a.word(0x32, "cell"); a.word(0x21, "cell")   # mov a,e / sta cell / lxi h,cell
    def ix_cell(a):
        a.db(0x7B); a.word(0x32, "cell"); a.db(0xDD); a.word(0x21, "cellm5")  # ... / ld ix,cell-5
    def cell_minus_5(a):
        a.labels["cellm5"] = a.labels.get("cell", 0) - 5
    variants = [("add a,n", [at_imm, 0xC6, 0x00], [immediate], "add"), ("sub a,n", [at_imm, 0xD6, 0x00], [immediate], "sub"),
                ("adc a,(hl)", [0x8E], [cell_e], "adc"), ("sbc a,(hl)", [0x9E], [cell_e], "sbc"),
                ("cp (hl)", [0xBE], [cell_e], "cp")]
    if mode == "z80":
        variants += [("add a,(ix+5)", [0xDD, 0x86, 0x05], [ix_cell], "add"), ("sbc a,(ix+5)", [0xDD, 0x9E, 0x05], [ix_cell], "sbc")]
    for name, op, pre, kind in variants:
        expect = lambda kind=kind: "".join(f"{r:02X}{f:02X}" for c in (0, 1) for a in range(256) for x in range(256)
                                           for r, f in [alu(kind, a, x, c)])
        out.append((name, program8(op, [0, 1], pre=pre), expect, None))
    single = z80_single if mode == "z80" else i8080_single
    mask = None if mode == "z80" else 0xD7
    def cell_d(a):
        a.db(0x7A); a.word(0x32, "cell"); a.word(0x21, "cell")   # mov a,d / sta cell / lxi h,cell: the value is A's
    def load_cell(a):
        a.word(0x3A, "cell"); a.db(0x67)         # lda cell / mov h,a: print the memory result
    memory_ops = [("inc (hl)", [0x34], [cell_d], [load_cell], "inc"), ("dec (hl)", [0x35], [cell_d], [load_cell], "dec"),
                  ("inc e", [0x5F, 0x1C, 0x7B], [], [], "inc"), ("dec c", [0x4F, 0x0D, 0x79], [], [], "dec")]
    if mode == "z80":
        def ix_d(a):
            a.db(0x7A); a.word(0x32, "cell"); a.db(0xDD); a.word(0x21, "cellm5")
        memory_ops += [("inc (ix+5)", [0xDD, 0x34, 0x05], [ix_d], [load_cell], "inc"),
                       ("dec (ix+5)", [0xDD, 0x35, 0x05], [ix_d], [load_cell], "dec")]
    for name, op, pre, post, kind in memory_ops:
        expect = lambda kind=kind: "".join(f"{r:02X}{f:02X}" for f0 in ALL_F for a in range(256)
                                           for r, f in [single(kind, a, f0)])
        out.append((name, program8(op, ALL_F, operands=False, pre=pre, post=post), expect, mask))
    rotate = z80_rotate_a if mode == "z80" else i8080_rotate_a
    for kind, op in {"rlca": 0x07, "rrca": 0x0F, "rla": 0x17, "rra": 0x1F}.items():
        expect = lambda kind=kind: "".join(f"{r:02X}{f:02X}" for f0 in ALL_F for a in range(256)
                                           for r, f in [rotate(kind, a, f0)])
        out.append((kind, program8([op], ALL_F, operands=False), expect, mask))
    if mode == "z80":
        for n, kind in enumerate(["rlc", "rrc", "rl", "rr", "sla", "sra", "sll", "srl"]):
            expect = lambda kind=kind: "".join(f"{r:02X}{f:02X}" for f0 in (0, 1) for a in range(256)
                                               for r, f in [z80_cb(kind, a, f0)])
            out.append((f"{kind} a", program8([0xCB, 8 * n + 7], [0, 1], operands=False), expect, None))
        for bit in range(8):
            expect = lambda bit=bit: "".join(f"{a:02X}{f:02X}" for f0 in (0, 1) for a in range(256) for x in range(256)
                                             for _, f in [z80_cb(f"bit{bit}", x, f0)])
            out.append((f"bit {bit},e", program8([0xCB, 0x43 + 8 * bit], [0, 1]), expect, None))
        for kind, op in {"rld": 0x6F, "rrd": 0x67}.items():
            expect = lambda kind=kind: "".join(f"{r:02X}{f:02X}" for f0 in (0, 1) for a in range(256) for m in range(256)
                                               for r, f in [z80_rld(kind, a, m, f0)])
            out.append((kind, program8([0xED, op], [0, 1], pre=[cell_e]), expect, None))
    if mode == "z80":
        expect = lambda: "".join(f"{r:02X}{f:02X}" for f0 in (0, 1) for a in range(256) for m in range(256)
                                 for r, f in [z80_cpi(a, m, f0)])
        out.append(("cpi", program8([0xED, 0xA1], [0, 1], cpi=True), expect, None))
    ops16 = {"add": [0x19]} if mode == "8080" else {"add": [0x19], "adc": [0xED, 0x5A], "sbc": [0xED, 0x52]}
    if mode == "z80":
        ops16["add ix"] = [0xDD, 0x19]
    for kind, op in ops16.items():
        # ADD HL leaves S, Z and P/V as they were: compare only what it sets.
        mask = (H | N | C | Y | X) if kind == "add" and mode == "z80" else C if kind == "add" else None  # 8080 DAD: only CY
        base = "add" if kind == "add ix" else kind
        if kind == "add ix":
            mask = H | N | C | Y | X
        expect = lambda base=base: "".join(f"{r:04X}{f:02X}" for c in (0, 1) for hl in VALUES for de in VALUES
                                           for r, f in [model16(mode, base, hl, de, c)])
        name = "add ix,de" if kind == "add ix" else f"{kind} hl,de"
        out.append((name, program16(op, ix=kind == "add ix"), expect, mask))
    return out

def compare(name, got, want, mask, width):
    """Compare result+F records; return the number that differ, printing a few."""
    size = width + 2
    if len(got) != len(want):
        print(f"FAIL {name}: {len(got) // size} results, expected {len(want) // size}")
        return 1
    bad, shown = 0, 0
    for i in range(0, len(want), size):
        g, w = got[i:i + size], want[i:i + size]
        gr, gf, wr, wf = g[:width], int(g[width:], 16), w[:width], int(w[width:], 16)
        if mask is not None:
            gf, wf = gf & mask, wf & mask
        if gr != wr or gf != wf:
            bad += 1
            if shown < 4:
                shown += 1
                print(f"     case {i // size}: result {gr} F {gf:02X}, expected {wr} F {wf:02X} "
                      f"(differs in {''.join(n for n, b in zip('SZYHXPNC', range(7, -1, -1)) if (gf ^ wf) >> b & 1)})")
    return bad

def main():
    failures = 0
    with tempfile.TemporaryDirectory() as d:
        for mode in ("z80", "8080"):
            for name, program, expect, mask in cases(mode):
                path = os.path.join(d, "ALU.COM")
                with open(path, "wb") as f:
                    f.write(program)
                args = [NTVCM] + (["-8"] if mode == "8080" else []) + [path]
                got = subprocess.run(args, capture_output=True, timeout=600).stdout.decode("latin-1")
                got = "".join(ch for ch in got if ch in "0123456789ABCDEF")
                width = 4 if "hl,de" in name or "ix,de" in name else 2
                bad = compare(name, got, expect(), mask, width)
                failures += bad != 0
                print(f"{'ok  ' if not bad else 'FAIL'} {mode} {name}" + (f": {bad} cases differ" if bad else ""))
    print("all flags match" if not failures else f"{failures} instruction(s) differ")
    return 1 if failures else 0

if __name__ == "__main__":
    sys.exit(main())
