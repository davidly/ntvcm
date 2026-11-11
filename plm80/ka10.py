"""Approximate DEC KA10 execution times, for estimating how long a program
took on the processor of 1967.

The times are from the PDP-10 System Reference Manual (DEC-10-HGAA-D, May
1968): each instruction's time in user mode, the figure in parentheses, which
includes relocation. They assume 1.00 microsecond MA10 core memory with the
accumulators in fast registers, and direct addressing; the manual's Appendix C
timing chart adds .28 microseconds for indexing and 1.09 for each level of
indirection. Variable terms (shift counts, byte sizes, BLT words, floating
point alignment and normalization) are computed from the operands, and a
memory operand in a fast register saves .34 microseconds. The KA10's logic is
asynchronous, so it has no clock rate; the manual gives its times as +-5%.

Monitor calls (opcodes 040 to 077: CALLI, TTCALL, the channel UUOs, Tymshare's
AUXCAL and CHANIO) ran in the operating system, not the program, so they are
counted but not timed. Instructions only the KI10 has (FIX, FIXR, FLTR and
the double-word ones) are given rough times, and counted.
"""
from pdp10 import RH, SIGN

CPU = 'DEC KA10'
YEAR = 1967
CLOCK = 'asynchronous logic, 1.00 us MA10 core memory'
INDEX = 28  # Appendix C: adding the index register, from fast memory
INDIRECT = 109  # Appendix C: another fetch in user mode: 17 + 11 + 55 + 17, then 9
FAST_OPERAND = 34  # most memory-operand instructions, when E is an accumulator

# Times in hundredths of a microsecond, kept as integers so the native runners
# total exactly the same, for the four modes, as the manual orders them: basic,
# Immediate, Memory, and Self or Both.
HALF = (243, 147, 258, 287)  # the Zeros, Ones and Extend half word transfers
HALF_LL = (257, 161, 301, 287)  # HLL, HRR
HALF_RL = (292, 196, 301, 287)  # HRL, HLR, which swap halves
MOVE = (243, 147, 258, 287)  # MOVE, MOVS
MOVN = (261, 165, 276, 305)  # MOVN, MOVM
BOOLEAN = {0: (147, 147, 244, 244), 15: (147, 147, 244, 244),  # SETZ, SETO
           6: (161, 161, 258, 258), 10: (161, 161, 258, 258),  # SETA, SETCA
           3: (243, 147, 287, 287), 12: (243, 147, 287, 287),  # SETM, SETCM
           2: (292, 196, 363, 363), 8: (292, 196, 363, 363),  # ANDCA, ANDCB
           11: (292, 196, 363, 363), 14: (292, 196, 363, 363)}  # ORCA, ORCB
BOOLEAN_OTHER = (257, 161, 301, 301)  # AND, ANDCM, XOR, IOR, EQV, ORCM
ARITHMETIC = {0o220: (981, 820, 1078, 1078),  # IMUL: averages
              0o224: (1082, 869, 1163, 1163),  # MUL
              0o230: (1670, 1580, 1760, 1760),  # IDIV
              0o234: (1640, 1550, 1730, 1730),  # DIV
              0o270: (275, 179, 319, 319),  # ADD
              0o274: (275, 179, 319, 319)}  # SUB
FLOATING = {0o140: (468, 553, 565, 565),  # FAD, FADL, FADM, FADB: + 15D + 25N
            0o144: (468, 381, 565, 565),  # FADR, FADRI, ...
            0o150: (486, 571, 583, 583),  # FSB
            0o154: (486, 399, 583, 583),  # FSBR
            0o160: (1051, 1136, 1148, 1148),  # FMP: averages for normalized operands
            0o164: (1051, 847, 1148, 1148),  # FMPR
            0o170: (1430, 1580, 1530, 1530),  # FDV
            0o174: (1430, 1340, 1530, 1530)}  # FDVR
SINGLE = {0o130: 484,  # UFA, + 15D
          0o131: 354,  # DFN
          0o132: 286,  # FSC, + 25N
          0o133: 298,  # IBP, + 26 past a word
          0o134: 457,  # ILDB, + 15(P + S), + 26 past a word
          0o135: 435,  # LDB, + 15(P + S)
          0o136: 551,  # IDPB, + 15(P + S), + 26 past a word
          0o137: 520,  # DPB, + 15(P + S)
          0o243: 230,  # JFFO, + 20 (leading zeros mod 18)
          0o250: 301,  # EXCH
          0o251: 108,  # BLT, + 283 a word (248 from fast memory)
          0o252: 179, 0o253: 179,  # AOBJP, AOBJN
          0o254: 147,  # JRST
          0o255: 147,  # JFCL
          0o256: 147,  # XCT, then the instruction it executes
          0o257: 147,  # MAP (KI10): as a no-op
          0o260: 311,  # PUSHJ
          0o261: 407,  # PUSH
          0o262: 415,  # POP
          0o263: 318,  # POPJ
          0o264: 243,  # JSR
          0o265: 147,  # JSP
          0o266: 293,  # JSA
          0o267: 314}  # JRA
UUO = 244  # a local UUO, 001 to 037; its handler's instructions are timed too
KI_ONLY = 300  # FIX, FIXR, FLTR, DMOVE and the like, which the KA10 did in software


def seconds(hundredths):
    """Hundredths of a microsecond as seconds to the microsecond, in integers."""
    return f'{hundredths // 10 ** 8}.{hundredths // 100 % 10 ** 6:06d}'


def signed18(e):
    return e - (1 << 18) if e & 1 << 17 else e


class Timer:
    """Wraps a PDP10's execute to total the KA10 time of what it runs."""

    def __init__(self, cpu):
        self.cpu = cpu
        self.hundredths = 0  # of a microsecond
        self.instructions = 0
        self.monitor_calls = 0
        self.ki_only = 0
        self.execute = cpu.execute
        cpu.execute = self.timed

    def timed(self, ir, at):
        self.hundredths += self.time(ir)
        self.instructions += 1
        return self.execute(ir, at)

    def address(self, ir):
        """The effective address, and the time its calculation adds."""
        m, word, added = self.cpu.m, ir, 0
        for _ in range(30):
            x = word >> 18 & 15
            e = word & RH
            if x:
                e = e + m[x] & RH
                added += INDEX
            if not word & 1 << 22:
                return e, added
            word = m[e]
            added += INDIRECT
        return e, added

    def time(self, ir):
        op, ac = ir >> 27, ir >> 23 & 15
        if 0o40 <= op <= 0o77 or op >= 0o700:
            self.monitor_calls += 1  # the monitor's time, not the program's
            return 0
        if op < 0o40:
            return UUO
        e, t = self.address(ir)
        mode = op & 3
        m = self.cpu.m
        reads_memory = mode != 1
        if 0o500 <= op <= 0o577:
            family = op >> 2 & 15
            t += (HALF_LL if family in (0, 8) else HALF_RL if family in (1, 9) else HALF)[mode]
        elif 0o200 <= op <= 0o217:
            t += (MOVN if op >= 0o210 else MOVE)[mode]
        elif 0o400 <= op <= 0o477:
            t += BOOLEAN.get(op >> 2 & 15, BOOLEAN_OTHER)[mode]
        elif op & ~3 in ARITHMETIC:
            t += ARITHMETIC[op & ~3][mode]
        elif op & ~3 in FLOATING:
            t += FLOATING[op & ~3][mode]
            immediate = op & 4 and mode == 1  # FADRI and the like: the operand is E,,0
            if op < 0o160:  # FAD and FSB: alignment and normalization
                t += self.float_terms(m[ac], e << 18 if immediate else m[e])
            reads_memory = not immediate
        elif 0o300 <= op <= 0o377:
            group = op >> 3 & 7
            t += (179, 275, 179, 261, 179, 305, 179, 305)[group]  # CAI CAM JUMP SKIP AOJ AOS SOJ SOS
            reads_memory = group in (1, 3, 5, 7)
        elif 0o600 <= op <= 0o677:
            reads_memory = bool(op & 0o10)  # TD and TS test memory; TR and TL, E
            t += 292 if reads_memory else 196
        elif op in (0o240, 0o241, 0o242, 0o244, 0o245, 0o246):  # shifts: + 15 a place
            n = signed18(e)
            combined = op >= 0o244
            t += (211 if combined else 173) if n >= 0 else (195 if combined else 157)
            t += 15 * min(abs(n), 255)
            reads_memory = False
        elif op in SINGLE:
            t += SINGLE[op]
            reads_memory = op in (0o250, 0o261, 0o262)
            if 0o133 <= op <= 0o137:
                t += self.byte_terms(op, m[e])
                reads_memory = False
            elif op == 0o243:
                v = m[ac]
                t += 20 * ((36 - v.bit_length()) % 18 if v else 0)
            elif op == 0o251:
                words = max(0, e - (m[ac] & RH) + 1)
                t += (248 if m[ac] >> 18 < 16 else 283) * words
            elif op == 0o130:
                t += self.float_terms(m[ac], m[e], normalize=False)
            elif op == 0o132:
                t += 25 * 1  # one normalization step, the usual case
        else:
            self.ki_only += 1
            return t + KI_ONLY
        if reads_memory and e < 16:
            t -= FAST_OPERAND
        return t

    @staticmethod
    def exponent(word):
        if word & SIGN:
            word = -word & (1 << 36) - 1
        return word >> 27 & 255

    def float_terms(self, a, b, normalize=True):
        """15 a place of exponent difference D, and 25 a normalization shift N (at most one, for
        normalized operands)."""
        if not a or not b:
            return 0
        d = min(abs(self.exponent(a) - self.exponent(b)), 63)
        return 15 * d + (25 if normalize else 0)

    @staticmethod
    def byte_terms(op, pointer):
        """15 a place of the pointer's position and size; 26 when incrementing passes a word."""
        p, s = pointer >> 30 & 63, pointer >> 24 & 63
        crosses = op in (0o133, 0o134, 0o136) and p < s
        return (15 * (p + s) if op != 0o133 else 0) + (26 if crosses else 0)

    def seconds(self):
        return self.hundredths / 10 ** 8

    def report(self):
        return report([self])


def report(timers):
    """The CPU, its clock and year, and the estimate, as ntv4004 reports the 4004's; several
    timers' runs (a compiler's passes) are added together."""
    hundredths = sum(t.hundredths for t in timers)
    monitor_calls = sum(t.monitor_calls for t in timers)
    ki_only = sum(t.ki_only for t in timers)
    notes = f'excluding {monitor_calls:,} monitor calls and I/O waits'
    if ki_only:
        notes += f'; {ki_only:,} KI10-only instructions estimated'
    return f'approximate {CPU} runtime: {seconds(hundredths)} seconds on a {YEAR} {CPU}, {CLOCK} ({notes})'
