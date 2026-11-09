#!/usr/bin/env python3
"""Build Intel's PL/M-80 2.0 with gfortran, then run its programs in ntvcm."""
from decimal import Decimal, localcontext
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import gfplm  # noqa: E402
NTVCM = HERE.parents[1] / ('ntvcm.exe' if os.name == 'nt' else 'ntvcm')
READY = shutil.which(gfplm.GFORTRAN) and (gfplm.SOURCES / 'PLM81.FOR').is_file()


@unittest.skipUnless(READY, 'needs gfortran and the Fortran sources; see README.md')
class GFortran(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        gfplm.build()

    def run_com(self, memory, mode=()):
        with tempfile.TemporaryDirectory(prefix='gfplm test ') as directory:
            Path(directory, 'TEST.COM').write_bytes(gfplm.com_file(memory))
            result = subprocess.run([str(NTVCM), '-c', *mode, 'TEST.COM'], cwd=directory,
                                    capture_output=True, timeout=300)
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout.replace(b'\r\n', b'\n')

    def test_mts_sample(self):
        # The sample's HEX, as compiled on an IBM mainframe under MTS.
        sample = gfplm.SOURCES / 'PLMSAMP.PLM'
        memory, listing = gfplm.compile_source(sample.read_text(encoding='ascii'))
        reference = [line.strip() for line in (gfplm.SOURCES / 'PLMSAMP.HEX').read_text().splitlines()
                     if line.strip().startswith(':')]
        expected = gfplm.read_object(' $\n' + '\n'.join(reference) + '\n')
        self.assertEqual(memory, expected)
        self.assertEqual(listing.count('NO PROGRAM ERRORS'), 2)

    def test_examples(self):
        with localcontext() as context:
            context.prec = 220
            digits = str(Decimal(1).exp()).replace('.', '')[:192]
        examples = HERE.parent / 'examples'
        for name, output in (('hello', b'HELLO, PL/M-80!\n'), ('arithmetic', b'OK\n'), ('sieve', b'1899\n'),
                             ('e', (digits + '\nDONE\n').encode()), ('ttt', b'6493 MOVES\n')):
            memory, _ = gfplm.compile_source((examples / f'{name}.plm').read_text())
            for mode in ((), ('-8',)):
                with self.subTest(name=name, mode=mode):
                    self.assertEqual(self.run_com(memory, mode), output)

    def test_errors(self):
        with self.assertRaisesRegex(gfplm.CompileError, 'ERROR 3'):
            gfplm.compile_source('100H: DECLARE X BYTE;\nX = ;\n')
        # Version 2.0 reads 80 columns, where 4.1 reads 72, and has the same alphabet.
        gfplm.check_source('100H: DECLARE A BYTE;' + ' ' * 55 + 'A=1;\n', gfplm.COLUMNS)
        for source, message in (('100H: DECLARE A BYTE;' + ' ' * 56 + 'A=1;\n', '80-column'),
                                ("100H: DECLARE S(2) BYTE INITIAL ('ab');\n", 'alphabet')):
            with self.subTest(message=message), self.assertRaisesRegex(gfplm.CompileError, message):
                gfplm.compile_source(source)


if __name__ == '__main__':
    unittest.main()
