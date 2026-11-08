#!/usr/bin/env python3
"""Run programs in Intel's INTERP/80 and compare them with ntvcm's 8080 mode."""
import csv
import importlib.util
import os
from pathlib import Path
import random
import subprocess
import sys
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(ROOT / 'mac80'))
import run as int80  # noqa: E402
from compile import CompileError  # noqa: E402
NTVCM = ROOT / ('ntvcm.exe' if os.name == 'nt' else 'ntvcm')
INT80 = all((HERE / 'upstream' / name).is_file() for name in ('int80.sav', 'int80e.sav'))
MAC80 = (ROOT / 'mac80/upstream/mac80.sav').is_file()
PLM80 = all((ROOT / 'plm80/upstream' / name).is_file() for name in ('plm81.sav', 'plm82.sav'))

# Opcodes whose timing differs: (ntvcm 8080 cycles, INTERP/80 states).
# Intel's 8080 Microcomputer Systems User's Manual (September 1975) gives 16
# for LHLD and SHLD.
TIMING = {0x22: (16, 17), 0x2A: (16, 17)}
# A taken conditional call takes 17 cycles in ntvcm and 18 states in
# INTERP/80; both take 11 when it is not taken. Flag and value that take it.
CALLS = {0xC4: ('z', 0), 0xCC: ('z', 1), 0xD4: ('cy', 0), 0xDC: ('cy', 1),
         0xE4: ('p', 0), 0xEC: ('p', 1), 0xF4: ('s', 0), 0xFC: ('s', 1)}


def ntvcm(memory):
    """Run in ntvcm's 8080 mode; return the output and the cycles at each PC."""
    with tempfile.TemporaryDirectory(prefix='int80 test ') as directory:
        image = bytes(memory.get(a, 0) for a in range(int80.TPA, max(memory) + 1))
        Path(directory, 'TEST.COM').write_bytes(image)
        result = subprocess.run([str(NTVCM), '-c', '-8', '-g:profile.csv', 'TEST.COM'], cwd=directory,
                                capture_output=True, timeout=600)
        if result.returncode:
            raise AssertionError(f'ntvcm failed: {result.stdout!r}')
        with open(Path(directory, 'profile.csv'), newline='') as profile:
            cycles = {int(row['pc']): int(row['count']) for row in csv.DictReader(profile)}
    return result.stdout.decode('ascii').replace('\r\n', '\n'), cycles


def compare(memory, expanded=False):
    """Return (ntvcm output, INTERP/80 output, INTERP/80 states, states expected from ntvcm).

    ntvcm's profile gives the cycles spent in the program itself; the known
    timing differences convert them to INTERP/80 states. INTERP/80 traces each
    conditional call to tell whether it was taken.
    """
    expected_output, profile = ntvcm(memory)
    program = {pc: cycles for pc, cycles in profile.items() if int80.TPA <= pc < 0xFE00}  # ntvcm's BDOS is at FEFC
    expected = sum(program.values())
    for pc, cycles in program.items():
        if memory.get(pc) in TIMING:
            before, after = TIMING[memory[pc]]
            expected += cycles // before * (after - before)
    watch = [pc for pc in program if memory.get(pc) in CALLS]
    output, states, watched = int80.run_program(memory, expanded, watch)
    for pc, registers in watched:
        flag, value = CALLS[memory[pc]]
        expected += registers[flag] == value
    return expected_output, output.replace('\r\n', '\n'), states, expected


def com(path):
    return {int80.TPA + i: b for i, b in enumerate(Path(path).read_bytes())}


def assemble(source):
    import assemble as mac80
    return mac80.assemble_source(source)[0]


# Print the bytes from SP up to the stack's starting point in hex, then exit.
DUMP = '''        LXI H,0
        DAD SP
        MVI B,{count}
DLOOP:  MOV A,M
        RRC
        RRC
        RRC
        RRC
        CALL HEX
        MOV A,M
        CALL HEX
        INX H
        DCR B
        JNZ DLOOP
        JMP 0
HEX:    ANI 0FH
        CPI 10
        JC DIGIT
        ADI 7
DIGIT:  ADI '0'
        MOV E,A
        MVI C,2
        PUSH H
        PUSH B
        CALL 5
        POP B
        POP H
        RET
'''


def timing_source():
    """Every documented instruction but RST, with conditions both taken and not."""
    setup = ['LXI H,2000H', 'LXI B,2100H', 'LXI D,2200H', 'MVI A,20H']
    lines, labels = [], iter(range(10000))
    for op in range(256):
        if op in (0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38, 0x76, 0xCB, 0xD9, 0xDD, 0xED, 0xFD) or op >= 0xC0:
            continue  # undocumented, HLT, and the control group below
        operands = (2 if op in (0x01, 0x11, 0x21, 0x31, 0x22, 0x2A, 0x32, 0x3A) else
                    1 if op & 0xC7 == 0x06 else 0)
        lines += setup + ['DB ' + ','.join(map(str, [op] + [0x20] * operands))]
    for op in range(0xC0, 0x100):
        if op & 7 in (6,):
            lines += setup + [f'DB {op},20H']
    lines += setup + ['PUSH B', 'POP B', 'PUSH D', 'POP D', 'PUSH H', 'POP H', 'PUSH PSW', 'POP PSW',
                      'XTHL', 'XTHL', 'XCHG', 'OUT 9', 'IN 9', 'DI', 'EI', 'LXI H,0', 'DAD SP', 'SPHL']
    for flags in (['XRA A'], ['MVI A,80H', 'ORA A', 'STC']):
        for condition in ('NZ', 'Z', 'NC', 'C', 'PO', 'PE', 'P', 'M'):
            label = f'T{next(labels)}'
            lines += flags + [f'J{condition} {label}', f'{label}:', f'C{condition} SUBR',
                              f'CALL Q{condition}']
    lines += ['JMP JUMP', 'JUMP:   LXI H,PCHLT', 'PCHL', 'PCHLT:  CALL SUBR', 'JMP 0', 'SUBR:   RET']
    for condition in ('NZ', 'Z', 'NC', 'C', 'PO', 'PE', 'P', 'M'):
        lines += [f'Q{condition}:  R{condition}', 'RET']
    return '        ORG 100H\n' + ''.join(line + '\n' if ':' in line else f'        {line}\n' for line in lines)


@unittest.skipUnless(INT80, 'optional INTERP/80 images not supplied; see int80/README.md')
class INT80(unittest.TestCase):
    def assert_same(self, memory, expanded=False, output=None):
        expected_output, actual, states, expected = compare(memory, expanded)
        self.assertEqual(actual, expected_output)
        if output is not None:
            self.assertEqual(actual, output)
        self.assertEqual(states, expected)

    def test_cpu_diagnostics(self):
        # ntvcm's 8080 test programs pass in INTERP/80, with matching states.
        for name, line in (('TST8080', 'CPU IS OPERATIONAL'), ('8080PRE', '8080 Preliminary tests complete'),
                           ('TEST', 'CPU IS OPERATIONAL')):
            with self.subTest(name=name):
                memory = com(ROOT / f'tests/{name}.COM')
                self.assertIn(line, compare(memory)[1])
                self.assert_same(memory)

    def test_supersoft_cputest(self):
        # It fails one check, which ntvcm passes: INTERP/80 leaves the
        # auxiliary carry set after DAA. The test then stops.
        output = int80.run_program(com(ROOT / 'tests/CPUTEST.COM'))[0]
        self.assertIn('INSTRUCTION SEQUENCE WAS 270000H\r\nREGISTER f CONTAINS 16H\r\n'
                      'BUT SHOULD CONTAIN 06H', output)
        self.assertIn('TEST NUMBER  0067H', output)
        self.assertEqual(output.count('ERROR COUNT'), 1)

    def test_memory_sizes(self):
        source = [0x21, 0x00, 0x90, 0x36, 0x41, 0x5E, 0x0E, 0x02, 0xCD, 0x05, 0x00, 0xC9]  # print M at 9000H
        memory = {int80.TPA + i: b for i, b in enumerate(source)}
        with self.assertRaisesRegex(CompileError, 'EXECUTION ERROR'):
            int80.run_program(memory)
        self.assertEqual(int80.run_program(memory, expanded=True)[:2], ('A', 61))
        self.assertEqual(compare(memory, expanded=True)[0], 'A')
        with self.assertRaisesRegex(CompileError, 'must fit'):
            int80.run_program({0x5F00: 0})

    def test_exits_and_errors(self):
        for code, states in (([0xC9], 10), ([0xC3, 0, 0], 10), ([0x0E, 0, 0xCD, 5, 0], 24)):
            with self.subTest(code=code):
                memory = {int80.TPA + i: b for i, b in enumerate(code)}
                self.assertEqual(int80.run_program(memory)[:2], ('', states))
                self.assert_same(memory)
        with self.assertRaisesRegex(CompileError, 'HLT at 0100H'):
            int80.run_program({0x100: 0x76})
        with self.assertRaisesRegex(CompileError, 'BDOS function 1 is not supported'):
            int80.run_program({0x100: 0x0E, 0x101: 1, 0x102: 0xCD, 0x103: 5, 0x104: 0})
        with self.assertRaisesRegex(CompileError, 'no exit within 100 states'):
            int80.run_program({0x100: 0xC3, 0x101: 0, 0x102: 1}, limit=100)


@unittest.skipUnless(INT80 and MAC80, 'optional INTERP/80 and MAC80 images not supplied')
class Assembled(unittest.TestCase):
    def test_tape_example(self):
        # The tape's INT80.EXA session loads MAC80.EXA's object and symbols.
        import assemble as mac80
        spec = importlib.util.spec_from_file_location('mac80_test', ROOT / 'mac80/test.py')
        mac80_test = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mac80_test)
        obj, _ = mac80.assemble_object(mac80_test.DECADD, symbols=True)
        transcript = int80.simulate(obj, '$F\nLOAD 7 7.\nDISPLAY SYM.\nBASE HEX.\nDIS CPU.\n$E\n', 10 ** 8)
        self.assertIn('19 LOAD OK', transcript)
        self.assertIn('000423Q 00275 0113H  ALPHA', transcript)
        self.assertIn('*0000*00H*00H*00H*00H*00H*00H*00H*0000H*0000H*0000H', transcript)

    def test_mac80_examples(self):
        for name in ('hello', 'sieve', 'e', 'ttt'):
            with self.subTest(name=name):
                memory = assemble((ROOT / f'mac80/examples/{name}.asm').read_text())
                INT80.assert_same(self, memory)

    def test_instruction_timing(self):
        INT80.assert_same(self, assemble(timing_source()))

    def test_flags(self):
        # Flags and results match the 8080 except the auxiliary carry (AC) of
        # five instructions, which follows INTERP/80's own rules.
        rules = {
            # SUB and CMP negate the operand, then add it.
            'SUB B': lambda a, b, cy: (a & 15) + (-b & 15) > 15,
            'CMP B': lambda a, b, cy: (a & 15) + (-b & 15) > 15,
            # SBB adds the negated operand and the negated borrow separately.
            'SBB B': lambda a, b, cy: (a & 15) + (-b & 15) + (-cy & 15) > 15,
            # ANA always clears it; the 8080 ORs bit 3 of the operands.
            'ANA B': lambda a, b, cy: False,
            # DAA leaves it as it was; here, ADD B's carry out of bit 3.
            'ADD B\n        DAA': lambda a, b, cy: (a & 15) + (b & 15) > 15,
        }
        exact = ['ADD B', 'ADC B', 'XRA B', 'ORA B', 'INR A', 'DCR A', 'ADI 37H', 'SUI 37H', 'CMA',
                 'RLC', 'RRC', 'RAL', 'RAR']
        generator = random.Random(8080)
        cases = [(generator.randrange(256), generator.randrange(256), generator.randrange(2)) for _ in range(100)]
        for instruction in exact + list(rules):
            source = '        ORG 100H\n' + ''.join(
                f'        MVI A,{a}\n        MVI B,{b}\n        {"STC" if cy else "ORA A"}\n'
                f'        {instruction}\n        PUSH PSW\n' for a, b, cy in cases)
            memory = assemble(source + DUMP.format(count=2 * len(cases)))
            expected, actual, _, _ = compare(memory)
            # The dump lists the last push first: flags, then A.
            pairs = [(int(text[i:i + 2], 16), int(text[i + 2:i + 4], 16), int(other[i:i + 2], 16),
                      int(other[i + 2:i + 4], 16)) for text, other in [(expected, actual)]
                     for i in range(0, len(text), 4)][::-1]
            self.assertEqual(len(pairs), len(cases))
            for (a, b, cy), (flags, result, int80_flags, int80_result) in zip(cases, pairs):
                with self.subTest(instruction=instruction, a=a, b=b, cy=cy):
                    self.assertEqual(int80_result, result)
                    if instruction in rules:
                        self.assertEqual(int80_flags & ~0x10, flags & ~0x10)
                        self.assertEqual(bool(int80_flags & 0x10), rules[instruction](a, b, cy))
                    else:
                        self.assertEqual(int80_flags, flags)


@unittest.skipUnless(INT80 and PLM80, 'optional INTERP/80 and PL/M-80 images not supplied')
class Compiled(unittest.TestCase):
    def test_plm80_examples(self):
        sys.path.insert(0, str(ROOT / 'plm80'))
        import compile as plm80
        for name in ('hello', 'arithmetic', 'sieve', 'e', 'ttt'):
            with self.subTest(name=name):
                memory, _ = plm80.compile_source((ROOT / f'plm80/examples/{name}.plm').read_text())
                INT80.assert_same(self, memory)


if __name__ == '__main__':
    unittest.main()
