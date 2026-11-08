#!/usr/bin/env python3
"""Assemble the examples with Intel's MAC80 and run them in ntvcm."""
from decimal import Decimal, localcontext
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import assemble  # noqa: E402
from compile import CompileError, com_file, hex_file  # noqa: E402
from pdp10 import NATIVE  # noqa: E402
NTVCM = HERE.parent / ('ntvcm.exe' if os.name == 'nt' else 'ntvcm')
IMAGES = all((HERE / 'upstream' / name).is_file() for name in ('mac80.sav', 'mac80e.sav'))

# SHA-256 of each example's Intel HEX. The sieve, e and ttt values are also
# the bytes CP/M's ASM.COM assembles from the original cpm_compilers sources.
DIGESTS = {
    'hello': '43311ec07476d22a1e58b7ca9471dcfd16b148340282d993fd3fdefc1e9b55c1',
    'sieve': '2555dc8dfd9b9b3ab1ef361f3f382781c1190de0b982a5b4e212417ce28fa107',
    'e': '367c98d6b0f338298429e06a619c944ce9a5f14f0df0a452c7e33fe8c790041a',
    'ttt': '36706b40e76f3f0f274254c891fba934f1e6d07aa65ba847df60ecb49bd4fe79',
}

# The tape's MAC80.EXA session, with the object file it printed in 1974.
DECADD = '''        ORG 100H     ;DECIMAL ADDITION EXAMPLE
START:  LXI D,ALPHA  ;LOAD D AND E IMMEDIATE
        LXI H,BETA   ;LOAD H AND L IMMEDIATE
        MVI C,8      ;LOAD C WITH 8
        XRA A        ;CLEARS CARRY
LOOP:   LDAX D       ;LOAD A WITH (DE)
        ADC M        ;ADD M TO A (HL)
        DAA          ;DECIMAL ADJUST
        STAX D       ;STORE A TO (DE)
        INX H        ;INCREMENT HL
        INX D        ;INCREMENT DE
        DCR C        ;DECREMENT C
        JNZ LOOP     ;IF NOT ZERO GO TO LOOP
ALPHA:  DS 4         ;ARGUMENT 1
BETA:   DS 4         ;SECOND ARGUMENT
        END
'''


def opcode_table():
    """Every documented 8080 instruction and its encoding, from the opcode fields."""
    registers = 'BCDEHLMA'
    pairs, stack = ('B', 'D', 'H', 'SP'), ('B', 'D', 'H', 'PSW')
    conditions = ('NZ', 'Z', 'NC', 'C', 'PO', 'PE', 'P', 'M')
    table = [('NOP', [0]), ('HLT', [0x76])]
    for d, dst in enumerate(registers):
        for s, src in enumerate(registers):
            if dst != 'M' or src != 'M':
                table.append((f'MOV {dst},{src}', [0x40 | d << 3 | s]))
        table += [(f'MVI {dst},12H', [0x06 | d << 3, 0x12]), (f'INR {dst}', [0x04 | d << 3]),
                  (f'DCR {dst}', [0x05 | d << 3])]
    for p, pair in enumerate(pairs):
        table += [(f'LXI {pair},1234H', [0x01 | p << 4, 0x34, 0x12]), (f'INX {pair}', [0x03 | p << 4]),
                  (f'DCX {pair}', [0x0B | p << 4]), (f'DAD {pair}', [0x09 | p << 4]),
                  (f'PUSH {stack[p]}', [0xC5 | p << 4]), (f'POP {stack[p]}', [0xC1 | p << 4])]
    table += [('STAX B', [0x02]), ('STAX D', [0x12]), ('LDAX B', [0x0A]), ('LDAX D', [0x1A]),
              ('SHLD 1234H', [0x22, 0x34, 0x12]), ('LHLD 1234H', [0x2A, 0x34, 0x12]),
              ('STA 1234H', [0x32, 0x34, 0x12]), ('LDA 1234H', [0x3A, 0x34, 0x12])]
    table += [(name, [0x07 | i << 3]) for i, name in enumerate(('RLC', 'RRC', 'RAL', 'RAR', 'DAA', 'CMA', 'STC', 'CMC'))]
    for i, (name, immediate) in enumerate(zip(('ADD', 'ADC', 'SUB', 'SBB', 'ANA', 'XRA', 'ORA', 'CMP'),
                                              ('ADI', 'ACI', 'SUI', 'SBI', 'ANI', 'XRI', 'ORI', 'CPI'))):
        table += [(f'{name} {r}', [0x80 | i << 3 | s]) for s, r in enumerate(registers)]
        table.append((f'{immediate} 12H', [0xC6 | i << 3, 0x12]))
    for c, condition in enumerate(conditions):
        table += [(f'R{condition}', [0xC0 | c << 3]), (f'J{condition} 1234H', [0xC2 | c << 3, 0x34, 0x12]),
                  (f'C{condition} 1234H', [0xC4 | c << 3, 0x34, 0x12])]
    table += [(f'RST {n}', [0xC7 | n << 3]) for n in range(8)]
    table += [('JMP 1234H', [0xC3, 0x34, 0x12]), ('CALL 1234H', [0xCD, 0x34, 0x12]), ('RET', [0xC9]),
              ('OUT 12H', [0xD3, 0x12]), ('IN 12H', [0xDB, 0x12]), ('XTHL', [0xE3]), ('PCHL', [0xE9]),
              ('XCHG', [0xEB]), ('SPHL', [0xF9]), ('DI', [0xF3]), ('EI', [0xFB])]
    return table


def run_com(memory, mode=()):
    with tempfile.TemporaryDirectory(prefix='mac80 test ') as directory:
        Path(directory, 'TEST.COM').write_bytes(com_file(memory))
        result = subprocess.run([str(NTVCM), '-c', *mode, 'TEST.COM'], cwd=directory,
                                capture_output=True, timeout=300)
    return result.returncode, result.stdout.replace(b'\r\n', b'\n')


@unittest.skipUnless(IMAGES, 'optional MAC80 images not supplied; see mac80/README.md')
class MAC80(unittest.TestCase):
    def test_tape_example(self):
        obj, listing = assemble.assemble_object(DECADD, symbols=True)
        self.assertIn(' 015 START 000400Q\n', obj)
        self.assertTrue(obj.endswith(' $\n :100100001113012117010E08AF1A8E271223130DA8\n'
                                     ' :03011000C2090120\n :0000000000\n $\n'), obj)
        self.assertIn('   0110   C20901           JNZ LOOP', listing)

    def test_examples(self):
        with localcontext() as context:
            context.prec = 220
            digits = str(Decimal(1).exp()).replace('.', '')[:192]
        outputs = {'hello': b'HELLO FROM MAC80!\n', 'sieve': b'1899 primes.\n',
                   'e': (digits + '\ndone.\n').encode(), 'ttt': b'moves: 6493\niterations: 1\n'}
        for name, output in outputs.items():
            source = (HERE / f'examples/{name}.asm').read_text()
            memory, listing = assemble.assemble_source(source)
            self.assertIn('NO PROGRAM ERRORS', listing)
            with self.subTest(name=name):
                self.assertEqual(hashlib.sha256(hex_file(memory).encode()).hexdigest(), DIGESTS[name])
                # MAC80E, the expanded 2.3 assembler, produces the same bytes.
                self.assertEqual(assemble.assemble_source(source, expanded=True)[0], memory)
            for mode in ((), ('-8',)):  # Z80 and 8080 instruction sets
                with self.subTest(name=name, mode=mode):
                    self.assertEqual(run_com(memory, mode), (0, output))

    def test_opcodes(self):
        table = opcode_table()
        self.assertEqual(len(table), 244)  # the documented 8080 instructions
        source = '        ORG 100H\n' + ''.join(f'        {text}\n' for text, _ in table)
        memory, _ = assemble.assemble_source(source)
        expected = [byte for _, code in table for byte in code]
        self.assertEqual([memory[0x100 + i] for i in range(len(memory))], expected)

    def test_runners_agree(self):
        if not NATIVE.is_file():
            self.skipTest('native PDP-10 runner not built; run plm80/m.sh or m.bat')
        source = (HERE / 'examples/hello.asm').read_text()
        native = assemble.assemble_object(source, symbols=True)
        os.environ['PLM80_PYTHON'] = '1'
        try:
            self.assertEqual(assemble.assemble_object(source, symbols=True), native)
        finally:
            del os.environ['PLM80_PYTHON']

    def test_source_checks(self):
        for source, message in (("        DB 'a'\n", 'alphabet'),
                                ('        MVI A,1' + ' ' * 60 + '+1\n', '72-column'),
                                ('$T=1\n', 'control lines'),
                                ('LONGNAME EQU 1\n        LXI H,LONGNOTE\n', 'LONGNOTE and LONGNAME')):
            with self.subTest(message=message), self.assertRaisesRegex(CompileError, message):
                assemble.assemble_source(source)
        # Comments may hold anything, and END is appended when missing.
        memory, _ = assemble.assemble_source("        ORG 100H\n        NOP ; lowercase {}\n")
        self.assertEqual(memory, {0x100: 0})

    def test_cli_errors_and_hex(self):
        with tempfile.TemporaryDirectory(prefix='mac80 test ') as directory:
            bad, output = Path(directory, 'bad.asm'), Path(directory, 'old.com')
            bad.write_text('        ORG 100H\n        JMP NOWHR\n', encoding='ascii')
            output.write_bytes(b'keep')
            result = subprocess.run([sys.executable, str(HERE / 'assemble.py'), str(bad), '-o', str(output)],
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 1)
            self.assertIn(' U 0100', result.stderr)  # MAC80 flags the undefined symbol
            self.assertEqual(output.read_bytes(), b'keep')
            low = Path(directory, 'low.asm')
            low.write_text('        NOP\n', encoding='ascii')
            result = subprocess.run([sys.executable, str(HERE / 'assemble.py'), str(low)],
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 1)
            self.assertIn('below the CP/M TPA', result.stderr)
            hexfile = Path(directory, 'hello.hex')
            result = subprocess.run([sys.executable, str(HERE / 'assemble.py'), str(HERE / 'examples/hello.asm'),
                                     '-o', str(hexfile)], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            text = hexfile.read_text()
        self.assertTrue(text.startswith(':10010000') and text.endswith(':00000001FF\n'))


if __name__ == '__main__':
    unittest.main()
