#!/usr/bin/env python3
"""Build Intel's PL/M-80 2.0 Fortran compiler for MS-DOS and run it in ntvdm.

  dosplm.py build              compile PLM81.FOR and PLM82.FOR with Microsoft
                               Fortran 5.1, running in ntvdm, into bin/
  dosplm.py compile prog.plm   run both passes in ntvdm; write prog.com
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
from compile import CompileError, com_file, hex_file, read_object  # noqa: E402

GITHUB = HERE.parents[2]
SOURCES = HERE.parent / 'fortran' / 'mts-1989'
BIN = HERE / 'bin'
EXE = '.exe' if os.name == 'nt' else ''
NTVDM = Path(os.environ.get('NTVDM', GITHUB / 'ntvdm' / ('ntvdm' + EXE)))
FORTRAN = Path(os.environ.get('MSFORTRAN', GITHUB / 'dos_compilers' / 'Microsoft Fortran v51'))
# DOS uppercases file names; Linux ntvdm's -u matches them to the uppercase
# files written here. Windows names ignore case, and its ntvdm has no -u.
ROOT = ([] if os.name == 'nt' else ['-u']) + ['-r:.']
FL = ROOT + ['-e:include=include,init=bin,path=bin;binb,lib=lib,tmp=tmp', 'binb\\fl', '/FPc', '/Ot']


def ntvdm(directory, arguments, answers=()):
    """Run ntvdm in directory, answering the Fortran runtime's file-name prompts."""
    if not NTVDM.is_file():
        raise CompileError(f'ntvdm not found at {NTVDM}; set NTVDM')
    stdin = ''.join(answer + '\r\n' for answer in answers).encode('ascii')
    result = subprocess.run([str(NTVDM), *arguments], cwd=directory, input=stdin,
                            capture_output=True, timeout=900)
    return result.stdout.decode('latin-1').replace('\r', '')


def split_units(text, parts):
    """Split Fortran source at program-unit headers into about parts files.

    Microsoft Fortran puts each source file's code in one 64K segment, which
    PLM82 overflows. Splitting between subroutines changes no statements.
    """
    lines = text.split('\n')
    starts = [i for i, line in enumerate(lines)
              if re.match(r' {6}\s*((INTEGER|LOGICAL|REAL)\s+)?(SUBROUTINE|FUNCTION|BLOCK DATA)\b', line)]
    bounds = [0] + starts + [len(lines)]
    target = len(lines) // parts + 1
    files, current = [], []
    for first, last in zip(bounds, bounds[1:]):
        current += lines[first:last]
        if len(current) >= target:
            files.append(current)
            current = []
    if current:
        files.append(current)
    return ['\r\n'.join(part).rstrip() + '\r\n' for part in files]


def build():
    for name in ('PLM81.FOR', 'PLM82.FOR'):
        if not (SOURCES / name).is_file():
            raise CompileError(f'{SOURCES / name} not found; see README.md')
    if not (FORTRAN / 'BINB').is_dir():
        raise CompileError(f'Microsoft Fortran 5.1 not found at {FORTRAN}; set MSFORTRAN')
    BIN.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='dosplm-') as directory:
        work = Path(directory)
        for folder in ('BIN', 'BINB', 'INCLUDE', 'LIB', 'TMP'):
            shutil.copytree(FORTRAN / folder, work / folder)
        for name, parts in (('PLM81', 1), ('PLM82', 4)):
            text = (SOURCES / f'{name}.FOR').read_text(encoding='ascii').replace('\r', '')
            objects = []
            for number, part in enumerate(split_units(text, parts), 1):
                source = f'{name[:3]}{name[-1]}{number}.FOR'
                (work / source).write_bytes(part.encode('ascii'))
                log = ntvdm(work, FL + ['/c', source])
                if not (work / f'{source[:-4]}.OBJ').is_file():
                    raise CompileError(log)
                objects.append(f'{source[:-4]}.OBJ')
            log = ntvdm(work, FL + [f'/Fe{name}.EXE', *objects, '/link', 'llibce.lib', '/NOE'])
            if not (work / f'{name}.EXE').is_file():
                raise CompileError(log)
            shutil.copy(work / f'{name}.EXE', BIN / f'{name}.EXE')
            print(f'built {BIN / name}.EXE')


def compile_source(source):
    """Return the memory image and both listings for PL/M source."""
    for name in ('PLM81.EXE', 'PLM82.EXE'):
        if not (BIN / name).is_file():
            raise CompileError(f'{BIN / name} not found; run dosplm.py build')
    source = source.replace('\r', '')
    # EOF is a PL/M token, not host EOF. Accept files with or without it.
    if not re.search(r'\bEOF\s*$', source):
        source = source.rstrip() + '\nEOF\n'
    with tempfile.TemporaryDirectory(prefix='dosplm-') as directory:
        work = Path(directory)
        for name in ('PLM81.EXE', 'PLM82.EXE'):
            shutil.copy(BIN / name, work / name)
        (work / 'SOURCE.PLM').write_bytes(source.replace('\n', '\r\n').encode('ascii'))
        (work / 'CONTROLS.TXT').write_bytes(b'\r\n')  # pass 2 reads controls to a blank line
        # Each pass asks for its files in this order the first time it uses
        # each Fortran unit: listing 12, input 2 or controls 1, work files,
        # object 17 and console 11.
        ntvdm(work, ROOT + ['PLM81.EXE'],
              ['LIST1.TXT', 'SOURCE.PLM', 'CODE.TMP', 'CONSOLE1.TXT', 'SYMBOLS.TMP'])
        ntvdm(work, ROOT + ['PLM82.EXE'],
              ['LIST2.TXT', 'CONTROLS.TXT', 'SYMBOLS.TMP', 'CODE.TMP', 'OBJECT.TXT', 'CONSOLE2.TXT'])
        listing = '\n'.join((work / name).read_text(encoding='latin-1').replace('\r', '')
                            for name in ('LIST1.TXT', 'LIST2.TXT') if (work / name).is_file())
        for console in ('CONSOLE1.TXT', 'CONSOLE2.TXT'):
            path = work / console
            if not path.is_file() or 'NO PROGRAM ERRORS' not in path.read_text(encoding='latin-1'):
                raise CompileError(listing or 'compiler failed')
        text = (work / 'OBJECT.TXT').read_text(encoding='latin-1').replace('\r', '')
        # The object file starts with the symbol table; HEX follows the first $.
        return read_object(text[text.index('$'):]), listing


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest='command', required=True)
    commands.add_parser('build', help='build bin/PLM81.EXE and bin/PLM82.EXE')
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
