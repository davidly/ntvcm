#!/usr/bin/env python3
"""Compile the examples with Intel's PL/M-80 4.1 and run them in ntvcm."""
from decimal import Decimal, localcontext
import io
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import compile as plm80  # noqa: E402
from pdp10 import NATIVE, PDP10, RH  # noqa: E402
NTVCM = HERE.parent / ('ntvcm.exe' if os.name == 'nt' else 'ntvcm')
IMAGES = all((HERE / 'upstream' / name).is_file() for name in ('plm81.sav', 'plm82.sav'))


def instruction(op, ac=0, address=0):
    return op << 27 | ac << 23 | address


class Runner(unittest.TestCase):
    """Runner behavior that needs no compiler images."""

    def machine(self):
        cpu = object.__new__(PDP10)
        cpu.m = [0] * (RH + 1)
        cpu.channels = {}
        cpu.input = io.StringIO()
        cpu.output = io.StringIO()
        cpu.pc = 64
        return cpu

    def test_tymshare_interrupt_calls_succeed(self):
        # Each call must skip the HALT the runtime places after it.
        for native in (False, True):
            if native and not NATIVE.is_file():
                continue
            for call in range(0o777736, 0o777746):
                with self.subTest(native=native, call=oct(call)):
                    cpu = self.machine()
                    cpu.m[64:67] = [instruction(0o47, 1, call), instruction(0o254, 4),
                                    instruction(0o47, address=0o12)]
                    if native:
                        with tempfile.TemporaryDirectory(prefix='plm80 runner ') as directory:
                            cpu.directory = Path(directory)
                            self.assertEqual(cpu.run_native(5), '')
                    else:
                        self.assertEqual(cpu.run(5), '')


@unittest.skipUnless(IMAGES, 'optional PL/M-80 images not supplied; see plm80/README.md')
class PLM80(unittest.TestCase):
    def run_com(self, memory, mode=()):
        with tempfile.TemporaryDirectory(prefix='plm80 test ') as directory:
            Path(directory, 'TEST.COM').write_bytes(plm80.com_file(memory))
            result = subprocess.run([str(NTVCM), *mode, 'TEST.COM'], cwd=directory,
                                    capture_output=True, timeout=300)
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout.replace(b'\r\n', b'\n')

    def test_examples(self):
        with localcontext() as context:
            context.prec = 220
            digits = str(Decimal(1).exp()).replace('.', '')[:192]
        for name, output in (('hello', b'HELLO, PL/M-80!\n'), ('arithmetic', b'OK\n'), ('sieve', b'1899\n'),
                             ('e', (digits + '\nDONE\n').encode()), ('ttt', b'6493 MOVES\n')):
            memory, listing = plm80.compile_source((HERE / f'examples/{name}.plm').read_text())
            self.assertEqual(listing.count('NO PROGRAM ERRORS'), 2, listing)
            for mode in ((), ('-8',)):  # Z80 and 8080 instruction sets
                with self.subTest(name=name, mode=mode):
                    self.assertEqual(self.run_com(memory, mode), output)

    def test_runners_agree(self):
        if not NATIVE.is_file():
            self.skipTest('native PDP-10 runner not built; run plm80/m.sh or m.bat')
        source = (HERE / 'examples/sieve.plm').read_text()
        native = plm80.compile_source(source)
        os.environ['PLM80_PYTHON'] = '1'
        try:
            self.assertEqual(plm80.compile_source(source), native)
        finally:
            del os.environ['PLM80_PYTHON']

    def test_source_checks(self):
        for source, message in (("100H: DECLARE A BYTE INITIAL ('a');\n", 'alphabet'),
                                ('100H: DECLARE A BYTE;' + ' ' * 60 + 'A=1;\n', '72-column'),
                                ('DECLARE A BYTE INITIAL (1);\n', 'below the CP/M TPA')):
            with self.subTest(message=message), self.assertRaisesRegex(plm80.CompileError, message):
                plm80.com_file(plm80.compile_source(source)[0])

    def test_cli_errors_and_hex(self):
        with tempfile.TemporaryDirectory(prefix='plm80 test ') as directory:
            bad, output = Path(directory, 'bad.plm'), Path(directory, 'old.com')
            bad.write_text('100H: DECLARE X BYTE;\nX = ;\n', encoding='ascii')
            output.write_bytes(b'keep')
            result = subprocess.run([sys.executable, str(HERE / 'compile.py'), str(bad), '-o', str(output)],
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 1)
            self.assertIn('ERROR', result.stderr)
            self.assertEqual(output.read_bytes(), b'keep')
            hexfile = Path(directory, 'hello.hex')
            result = subprocess.run([sys.executable, str(HERE / 'compile.py'), str(HERE / 'examples/hello.plm'),
                                     '-o', str(hexfile)], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            text = hexfile.read_text()
        self.assertTrue(text.startswith(':10010000') and text.endswith(':00000001FF\n'))


if __name__ == '__main__':
    unittest.main()
