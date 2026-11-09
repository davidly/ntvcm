#!/usr/bin/env python3
"""Build Intel's PL/M-80 2.0 Fortran compiler with gfortran and run it.

  gfplm.py build              compile PLM81.FOR and PLM82.FOR into bin/
  gfplm.py compile prog.plm   run both passes; write prog.com
"""
import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from compile import CompileError, check_source, com_file, hex_file, read_object  # noqa: E402

SOURCES = HERE.parent / 'fortran' / 'mts-1989'
COLUMNS = 80  # version 2.0 reads 80 columns; 4.1 reads 72
BIN = HERE / 'bin'
EXE = '.exe' if os.name == 'nt' else ''
GFORTRAN = os.environ.get('GFORTRAN', 'gfortran')
# The 1975 Fortran assumes static locals, as FORTRAN IV compilers gave them.
FLAGS = ['-std=legacy', '-O2', '-fno-automatic']


def build():
    for name in ('PLM81.FOR', 'PLM82.FOR'):
        if not (SOURCES / name).is_file():
            raise CompileError(f'{SOURCES / name} not found; see README.md')
    if not shutil.which(GFORTRAN):
        raise CompileError(f'{GFORTRAN} not found; install gfortran or set GFORTRAN')
    BIN.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='gfplm-') as directory:
        for name in ('PLM81', 'PLM82'):
            # The MTS files end with a DOS end-of-file byte, which gfortran rejects.
            text = (SOURCES / f'{name}.FOR').read_bytes().replace(b'\r', b'').rstrip(b'\x1a')
            source = Path(directory, f'{name}.f')
            source.write_bytes(text)
            program = BIN / f'{name.lower()}{EXE}'
            result = subprocess.run([GFORTRAN, *FLAGS, str(source), '-o', str(program)],
                                    capture_output=True, text=True)
            if result.returncode:
                raise CompileError(result.stderr)
            print(f'built {program}')


def compile_source(source):
    """Return the memory image and both listings for PL/M source."""
    passes = [BIN / f'plm81{EXE}', BIN / f'plm82{EXE}']
    for program in passes:
        if not program.is_file():
            raise CompileError(f'{program} not found; run gfplm.py build')
    check_source(source, COLUMNS)
    source = source.replace('\r', '').expandtabs(8)
    # EOF is a PL/M token, not host EOF. Accept files with or without it.
    if not re.search(r'\bEOF\s*$', source):
        source = source.rstrip() + '\nEOF\n'
    with tempfile.TemporaryDirectory(prefix='gfplm-') as directory:
        work = Path(directory)

        def unit(number):
            return work / f'fort.{number}'  # gfortran's file for an unopened unit
        # Pass 1 reads the source on unit 2 and writes its listing (12), code
        # (16), console (11) and symbols (17).
        unit(2).write_text(source, encoding='ascii')
        subprocess.run([str(passes[0])], cwd=work, stdin=subprocess.DEVNULL, capture_output=True, timeout=300)
        consoles = [unit(11).read_text(encoding='latin-1') if unit(11).is_file() else '']
        listings = [unit(12).read_text(encoding='latin-1') if unit(12).is_file() else '']
        if 'NO PROGRAM ERRORS' in consoles[0]:
            # Pass 2 reads controls to a blank line (1), symbols (7) and code
            # (4), and writes its listing (12), object (17) and console (11).
            unit(16).replace(unit(4))
            unit(17).replace(unit(7))
            for number in (11, 12):
                unit(number).unlink(missing_ok=True)
            unit(1).write_text('\n', encoding='ascii')
            subprocess.run([str(passes[1])], cwd=work, stdin=subprocess.DEVNULL, capture_output=True, timeout=300)
            consoles.append(unit(11).read_text(encoding='latin-1') if unit(11).is_file() else '')
            listings.append(unit(12).read_text(encoding='latin-1') if unit(12).is_file() else '')
        listing = '\n'.join(listings)
        if len(consoles) < 2 or 'NO PROGRAM ERRORS' not in consoles[1]:
            raise CompileError(listing or 'compiler failed')
        text = unit(17).read_text(encoding='latin-1')
        # The object file starts with the symbol table; HEX follows the first $.
        return read_object(text[text.index('$'):]), listing


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest='command', required=True)
    commands.add_parser('build', help='build bin/plm81 and bin/plm82')
    run = commands.add_parser('compile', help='compile a PL/M program to a CP/M .COM file')
    run.add_argument('source', type=Path)
    run.add_argument('-o', '--output', type=Path, help='.COM path (default: source.com); .hex writes Intel HEX')
    run.add_argument('--listing', type=Path, help="save both passes' listings")
    args = parser.parse_args()
    try:
        if args.command == 'build':
            build()
            return 0
        memory, listing = compile_source(args.source.read_text(encoding='ascii'))
        destination = args.output or args.source.with_suffix('.com')
        output = hex_file(memory).encode('ascii') if destination.suffix.lower() == '.hex' else com_file(memory)
        if args.listing:
            args.listing.write_text(listing, encoding='ascii', errors='replace')
        destination.write_bytes(output)
        print(f'{args.source}: compiled to {destination}')
    except (OSError, ValueError, CompileError, subprocess.SubprocessError) as error:
        print(f'PL/M-80 2.0: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
