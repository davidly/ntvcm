#!/usr/bin/env python3
"""Run Intel's original PL/M-80 4.1 cross-compiler and write a CP/M .COM file."""
import argparse
import os
from pathlib import Path
import re
import sys
import tempfile

from pdp10 import NATIVE, PDP10

UPSTREAM = Path(__file__).resolve().parent / 'upstream'
# Pass 1 lists to FOR17.DAT; $I=6 then switches input to the source in
# FOR20.DAT. Pass 2 writes symbols and Intel HEX to FOR21.DAT and its code
# listing to FOR07.DAT. These are the controls from the tape's PLM81.EXA.
PASS1 = '$O=4\n$M=1\n$I=6\n'
PASS2 = '$B=7\n$F=1\n$G=1\n$Q=1\n$O=3\n\n'
BUDGET = 1_000_000_000
TPA_TOP = 0xE000  # keep programs well below a CP/M BDOS


class CompileError(Exception):
    pass


def read_object(text):
    """Return the memory image from pass 2's object file."""
    memory = {}
    for line in text.splitlines():
        line = line.strip()
        if not line.startswith(':'):
            continue  # symbol table, $ and * framing
        try:
            raw = bytes.fromhex(line[1:])
        except ValueError as error:
            raise CompileError('invalid compiler HEX record') from error
        if len(raw) < 5 or len(raw) != raw[0] + 5 or sum(raw) & 255:
            raise CompileError('invalid compiler HEX length or checksum')
        if raw[0] == 0:
            return memory
        address = int.from_bytes(raw[1:3], 'big')
        memory.update((address + i, b) for i, b in enumerate(raw[4:-1]))
    raise CompileError('compiler object file has no end record')


def com_file(memory):
    if not memory:
        raise CompileError('empty compiler output')
    if min(memory) < 0x100:
        raise CompileError(f'code at {min(memory):04X}H is below the CP/M TPA; start the program with 100H:')
    if max(memory) >= TPA_TOP:
        raise CompileError(f'code at {max(memory):04X}H is too high for a CP/M .COM file')
    return bytes(memory.get(a, 0) for a in range(0x100, max(memory) + 1))


def hex_file(memory):
    lines = []
    addresses = sorted(memory)
    index = 0
    while index < len(addresses):
        start, data = addresses[index], []
        while index < len(addresses) and addresses[index] == start + len(data) and len(data) < 16:
            data.append(memory[addresses[index]])
            index += 1
        record = bytes([len(data), start >> 8, start & 255, 0, *data])
        lines.append(':' + (record + bytes([-sum(record) & 255])).hex().upper())
    return '\n'.join(lines + [':00000001FF']) + '\n'


def compile_source(source):
    """Return the memory image and both passes' listings."""
    source.encode('ascii')
    # The original lexer maps characters outside its alphabet to spaces,
    # even inside strings. Reject them rather than changing text.
    code = re.sub(r'/\*.*?\*/', '', source, flags=re.S)
    allowed = set(" ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$=./()+-'*,<>:;\n\r\t")
    for character in code:
        if character not in allowed:
            raise CompileError(f'character {character!r} is outside the original PL/M alphabet; '
                               'use uppercase source and numeric ASCII values for other characters')
    # Columns past 72 are silently ignored, which can drop whole statements.
    for number, line in enumerate(source.splitlines(), 1):
        if len(line.expandtabs(8)) > 72:
            raise CompileError(f'line {number} exceeds the compiler\'s 72-column limit')
    # EOF is a PL/M token, not host EOF. Accept files with or without it.
    if not re.search(r'\bEOF\s*$', source):
        source = source.rstrip() + '\nEOF\n'
    logs = []
    with tempfile.TemporaryDirectory(prefix='plm80-') as directory:
        Path(directory, 'FOR20.DAT').write_text(source.expandtabs(8), encoding='ascii')
        for name, controls, listing in (('plm81.sav', PASS1, 'FOR17.DAT'), ('plm82.sav', PASS2, 'FOR07.DAT')):
            runner = PDP10(UPSTREAM / name, controls, directory)
            try:
                # The native runner is much faster; PLM80_PYTHON=1 forces the reference.
                native = NATIVE.is_file() and not os.environ.get('PLM80_PYTHON')
                console = runner.run_native(BUDGET) if native else runner.run(BUDGET)
            except (RuntimeError, OSError, StopIteration, ZeroDivisionError) as error:
                raise CompileError(f'{name}: {error}\n{runner.output.getvalue()}') from error
            path = Path(directory, listing)
            logs.append(path.read_text(encoding='ascii').replace('\r', '') if path.exists() else console)
            if 'NO PROGRAM ERRORS' not in console:
                raise CompileError('\n'.join(logs))
        return read_object(Path(directory, 'FOR21.DAT').read_text(encoding='ascii')), '\n'.join(logs)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('-o', '--output', type=Path, help='.COM path (default: source.com); a .hex suffix writes Intel HEX')
    parser.add_argument('--listing', type=Path, help='save both compiler passes\' listings')
    args = parser.parse_args()
    try:
        memory, listing = compile_source(args.source.read_text(encoding='ascii'))
        destination = args.output or args.source.with_suffix('.com')
        output = hex_file(memory).encode('ascii') if destination.suffix.lower() == '.hex' else com_file(memory)
        if args.listing:
            args.listing.write_text(listing, encoding='ascii')
        destination.write_bytes(output)
        print(f'{args.source}: compiled to {destination}')
    except (OSError, ValueError, CompileError) as error:
        print(f'PL/M-80: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
