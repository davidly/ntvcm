#!/usr/bin/env python3
"""ntvcm Z80 flag check for SBC HL,rr.

Self-contained like cyclecheck.py: emits a tiny raw-byte .COM program per
case that sets the carry, runs SBC HL,DE, and prints HL and F in hex through
the BDOS; then compares them with the Zilog Z80 CPU User Manual's rules. H is
set on a borrow from bit 12: (HL & 0FFFh) < (DE & 0FFFh) + carry.

The first cases are DE with low bits 0FFFh and the carry set, where ntvcm
used to add the carry to DE before masking it and lose the borrow.

Exits 0 if every case matches, 1 otherwise.

Usage:  flagcheck.py [path-to-ntvcm]      (default: ./ntvcm)
"""
import subprocess, sys, os, tempfile

NTVCM = sys.argv[1] if len(sys.argv) > 1 else "./ntvcm"

def w16(a): return [a & 0xFF, (a >> 8) & 0xFF]

# (carry in, HL, DE): the expected HL and F follow from the Z80 rules below.
CASES = [
    (1, 0x0FFF, 0x0FFF),   # 0FFFh - 0FFFh - 1 borrows from bit 12: H
    (1, 0x0FFF, 0xFFFF),   # DE + carry wraps to 0 in 16 bits, too: H
    (1, 0x0123, 0x0FFF),
    (0, 0x0FFF, 0x0FFF),   # controls
    (1, 0x1000, 0x0001),
    (0, 0x8000, 0x0001),   # signed overflow
    (1, 0x0000, 0x0000),
]

def expected(carry, hl, de):
    """(HL, F) for SBC HL,DE."""
    result = (hl - de - carry) & 0xFFFF
    s = result >> 15
    z = int(result == 0)
    h = int((hl & 0xFFF) < (de & 0xFFF) + carry)
    hs = hl if hl < 0x8000 else hl - 0x10000
    v = int(not -0x8000 <= hs - (de if de < 0x8000 else de - 0x10000) - carry <= 0x7FFF)
    c = int(hl < de + carry)
    high = result >> 8  # the undocumented Y and X flags copy bits 5 and 3 of the result's high byte
    f = s << 7 | z << 6 | (high & 0x20) | h << 4 | (high & 0x08) | v << 2 | 1 << 1 | c
    return result, f

def program(carry, hl, de):
    """SCF (and CCF if no carry), LD HL, LD DE, SBC HL,DE, then print HL and F in hex."""
    code = [0x37] + ([0x3F] if not carry else [])        # scf / ccf
    code += [0x21] + w16(hl) + [0x11] + w16(de)        # ld hl,hl / ld de,de
    code += [0xED, 0x52]                               # sbc hl,de
    code += [0xF5]                                     # push af
    code += [0x7C, 0xCD, 0, 0, 0x7D, 0xCD, 0, 0]       # ld a,h / call hex / ld a,l / call hex
    code += [0xE1, 0x7D, 0xCD, 0, 0]                   # pop hl / ld a,l (F) / call hex
    code += [0xC3, 0x00, 0x00]                         # jp 0
    hexbyte = 0x100 + len(code)
    for offset in (i for i in range(len(code) - 2) if code[i] == 0xCD):
        code[offset + 1:offset + 3] = w16(hexbyte)
    nibble = hexbyte + 9                               # after push af, four rrca and the call, pop af
    code += [0xF5, 0x0F, 0x0F, 0x0F, 0x0F, 0xCD] + w16(nibble) + [0xF1]  # push af, rrca x4, call, pop af
    code += [0xE6, 0x0F, 0xC6, 0x90, 0x27, 0xCE, 0x40, 0x27]          # and 0Fh, add 90h, daa, adc 40h, daa
    code += [0x5F, 0x0E, 0x02, 0xCD, 0x05, 0x00, 0xC9]                # ld e,a / ld c,2 / call 5 / ret
    return bytes(code)

def main():
    failures = 0
    with tempfile.TemporaryDirectory() as d:
        for carry, hl, de in CASES:
            path = os.path.join(d, "SBC.COM")
            with open(path, "wb") as f:
                f.write(program(carry, hl, de))
            out = subprocess.run([NTVCM, path], capture_output=True, timeout=60).stdout.decode('latin-1').strip()
            want_hl, want_f = expected(carry, hl, de)
            want = f"{want_hl:04X}{want_f:02X}"
            ok = out == want
            failures += not ok
            print(f"{'ok  ' if ok else 'FAIL'} carry {carry} {hl:04X}h - {de:04X}h: HL F = {out[:4]} {out[4:]}"
                  f"{'' if ok else f', expected {want[:4]} {want[4:]}'}")
    print("all SBC HL flags match" if not failures else f"{failures} case(s) differ")
    return 1 if failures else 0

if __name__ == "__main__":
    sys.exit(main())
