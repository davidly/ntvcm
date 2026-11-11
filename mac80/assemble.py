#!/usr/bin/env python3
"""Run Intel's original MAC80 2.4 8080 macro cross-assembler and write a CP/M .COM file."""
import argparse
import os
from pathlib import Path
import re
import sys
import tempfile

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / 'plm80'))
from compile import CompileError, com_file, hex_file, read_object  # noqa: E402
import ka10  # noqa: E402
from pdp10 import NATIVE, PDP10  # noqa: E402

UPSTREAM = HERE / 'upstream'
# Listing to FOR22.DAT, then read the rest from FOR20.DAT. MAC80 takes later
# control lines from the new input unit, so $I must come last. The default
# hexadecimal object goes to FOR21.DAT. These are the tape's MAC80.EXA controls.
CONTROLS = '$O=3\n$I=2\n'
BUDGET = 1_000_000_000
SIGNIFICANT = 5  # characters MAC80 keeps of each symbol


def code_field(line):
    """Return the part of a source line before any comment."""
    quoted = False
    for index, character in enumerate(line):
        if character == "'":
            quoted = not quoted  # a doubled quote toggles twice
        elif character == ';' and not quoted:
            return line[:index]
    return line


def check_source(lines):
    symbols = {}
    for number, line in enumerate(lines, 1):
        code = code_field(line).rstrip()
        # MAC80's character table covers space through Z. Others, including
        # lowercase, become spaces even inside strings, so reject them.
        for character in code:
            if not ' ' <= character <= 'Z':
                raise CompileError(f'line {number}: character {character!r} is outside the MAC80 alphabet; '
                                   'use uppercase source and numeric values for other characters')
        # Columns past 72 are silently ignored, which can change an operand.
        if len(code) > 72:
            raise CompileError(f'line {number}: code exceeds MAC80\'s 72-column limit (comments may be longer)')
        if code.startswith('$'):
            raise CompileError(f'line {number}: MAC80 control lines are set by this wrapper')
        # MAC80 keeps five characters of a symbol, so longer names can alias
        # each other. It flags two such labels, but not a reference to one.
        for name in re.findall(r"'(?:[^']|'')*'|(?<![0-9A-Z?@])[A-Z?@][A-Z0-9?@]*", code):
            if not name.startswith("'"):
                other = symbols.setdefault(name[:SIGNIFICANT], name)
                if other != name:
                    raise CompileError(f'line {number}: {name} and {other} are the same symbol to MAC80, '
                                       f'which keeps only {SIGNIFICANT} characters')


def assemble_object(source, symbols=False, expanded=False):
    """Return MAC80's own object file and the listing; symbols adds its symbol table.

    expanded selects MAC80E, the 1975 expanded MAC80 2.3, whose symbol table is
    eight times larger.
    """
    source.encode('ascii')
    lines = source.expandtabs(8).splitlines()
    check_source(lines)
    if not any(re.match(r'([A-Z?@][A-Z0-9?@]*:?)?\s+END\s*$', code_field(line)) for line in lines):
        lines.append('        END')
    with tempfile.TemporaryDirectory(prefix='mac80-') as directory:
        Path(directory, 'FOR20.DAT').write_text('\n'.join(lines) + '\n', encoding='ascii')
        image = UPSTREAM / ('mac80e.sav' if expanded else 'mac80.sav')
        runner = PDP10(image, ('$T=1\n' if symbols else '') + CONTROLS, directory)
        try:
            # The native runner is much faster; PLM80_PYTHON=1 forces the reference.
            native = NATIVE.is_file() and not os.environ.get('PLM80_PYTHON')
            console = runner.run_native(BUDGET) if native else runner.run(BUDGET)
        except (RuntimeError, OSError, StopIteration, ZeroDivisionError) as error:
            raise CompileError(f'{image.name}: {error}\n{runner.output.getvalue()}') from error
        listing = Path(directory, 'FOR22.DAT').read_text(encoding='ascii').replace('\r', '')
        if 'NO PROGRAM ERRORS' not in console:
            # MAC80 flags each erroneous listing line in its second column.
            flagged = [line for line in listing.splitlines() if re.match(r' [A-Z] ', line)]
            raise CompileError('\n'.join(flagged + [console.strip().splitlines()[-1]]))
        return Path(directory, 'FOR21.DAT').read_text(encoding='ascii').replace('\r', ''), listing


def assemble_source(source, expanded=False):
    """Return the memory image, {address: byte}, and the listing."""
    result, listing = assemble_object(source, expanded=expanded)
    return read_object(result), listing


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('-o', '--output', type=Path, help='.COM path (default: source.com); a .hex suffix writes Intel HEX')
    parser.add_argument('--listing', type=Path, help='save the assembler listing')
    parser.add_argument('--expanded', action='store_true',
                        help='use MAC80E, the expanded 2.3 assembler with about 4,000 symbols instead of 500')
    parser.add_argument('--ka10', action='store_true', help='report how long MAC80 took on a 1967 DEC KA10')
    args = parser.parse_args()
    if args.ka10:
        PDP10.timers = []
    try:
        memory, listing = assemble_source(args.source.read_text(encoding='ascii'), args.expanded)
        destination = args.output or args.source.with_suffix('.com')
        if memory and min(memory) < 0x100 and destination.suffix.lower() != '.hex':
            raise CompileError(f'code at {min(memory):04X}H is below the CP/M TPA; start with ORG 100H')
        output = hex_file(memory).encode('ascii') if destination.suffix.lower() == '.hex' else com_file(memory)
        if args.listing:
            args.listing.write_text(listing, encoding='ascii')
        destination.write_bytes(output)
        print(f'{args.source}: assembled to {destination}')
        if args.ka10:
            print(ka10.report(PDP10.timers), file=sys.stderr)
    except (OSError, ValueError, CompileError) as error:
        print(f'MAC80: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
