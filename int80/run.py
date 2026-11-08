#!/usr/bin/env python3
"""Run CP/M programs in Intel's original INTERP/80 1.5 8080 simulator (INT80)."""
import argparse
import os
from pathlib import Path
import re
import sys
import tempfile

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / 'plm80'))
from compile import CompileError  # noqa: E402
from pdp10 import NATIVE, PDP10  # noqa: E402

UPSTREAM = HERE / 'upstream'
# Simulated memory: INT80 has 24K and the expanded INT80E 48K.
MEMORY = {False: 0x6000, True: 0xC000}
CONSOLE = 1  # the BDOS stub's output port
TPA = 0x100


class Environment:
    """A minimal CP/M for INT80, which simulates only the CPU and memory.

    As in ntvcm, address 5 jumps to the BDOS entry, which jumps once more.
    ntvcm then calls back to C; here an 8080 stub handles BDOS functions 0
    (exit), 2 (console output) and 9 (print string) and writes each character
    with OUT 1. A HLT at 0 ends a program that jumps or returns to CP/M.
    """

    def __init__(self, top):
        self.entry = top - 0x100      # ntvcm's FEFC: JMP to the stub
        self.stub = stub = self.entry + 3
        self.character = stub + 16
        self.string = stub + 20
        self.boot = stub + 30
        self.unsupported = stub + 15
        code = [0x79,                                   # MOV A,C
                0xFE, 2, 0xCA, *word(self.character),    # CPI 2; JZ CHAR
                0xFE, 9, 0xCA, *word(self.string),       # CPI 9; JZ STRING
                0xB7, 0xCA, *word(self.boot),            # ORA A; JZ BOOT
                0x76,                                   # HLT: unsupported function
                0x7B, 0xD3, CONSOLE, 0xC9,               # CHAR: MOV A,E; OUT 1; RET
                0x1A, 0xFE, ord('$'), 0xC8,              # STRING: LDAX D; CPI '$'; RZ
                0xD3, CONSOLE, 0x13, 0xC3, *word(self.string),  # OUT 1; INX D; JMP STRING
                0x76]                                   # BOOT: HLT
        self.outputs = (self.character + 1, self.string + 4)
        self.memory = {a: 0 for a in range(top)}  # ntvcm clears memory; INT80 fills it with HLT
        self.memory.update({0: 0x76, 5: 0xC3, 6: self.entry & 255, 7: self.entry >> 8,
                            self.entry: 0xC3, self.entry + 1: stub & 255, self.entry + 2: stub >> 8})
        self.memory.update((stub + i, b) for i, b in enumerate(code))
        # The stack starts below the BDOS with a return address of 0, as in CP/M.
        self.stack = self.entry - 2

    def states(self, function, characters):
        """INT80 states from the BDOS jump at 5 to the stub's return."""
        jumps = 20
        if function == 2:
            return jumps + 5 + 7 + 10 + 5 + 10 + 10
        if function == 9:
            return jumps + 39 + 44 * characters + 25
        return jumps + 39 + 4 + 10 + 7  # function 0 ends at the BOOT HLT


def word(value):
    return value & 255, value >> 8


def read_hex(text):
    """Return the memory image of an Intel HEX file."""
    memory = {}
    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            raw = bytes.fromhex(line[1:]) if line[0] == ':' else b''
        except ValueError:
            raw = b''
        if len(raw) < 5 or len(raw) != raw[0] + 5 or sum(raw) & 255:
            raise CompileError(f'invalid Intel HEX record: {line}')
        if raw[0] == 0 or raw[3] == 1:
            return memory
        if raw[3] != 0:
            raise CompileError(f'unsupported HEX record type: {line}')
        address = int.from_bytes(raw[1:3], 'big')
        memory.update((address + i, b) for i, b in enumerate(raw[4:-1]))
    raise CompileError('HEX file has no end record')


def mac80_object(memory):
    """Write memory as a MAC80-style hexadecimal object, the format INT80 loads."""
    lines, addresses = [], sorted(memory)
    index = 0
    while index < len(addresses):
        start, data = addresses[index], []
        while index < len(addresses) and addresses[index] == start + len(data) and len(data) < 16:
            data.append(memory[addresses[index]])
            index += 1
        record = bytes([len(data), start >> 8, start & 255, 0, *data])
        lines.append(' :' + (record + bytes([-sum(record) & 255])).hex().upper())
    return ' $\n' + '\n'.join(lines) + '\n :0000000000\n $\n'


def load_program(path):
    """Return the program's memory image from a .COM, Intel HEX or MAC80 source file."""
    path = Path(path)
    suffix = path.suffix.lower()
    if suffix == '.com':
        return {TPA + i: b for i, b in enumerate(path.read_bytes())}
    if suffix == '.hex':
        return read_hex(path.read_text(encoding='ascii'))
    sys.path.insert(0, str(HERE.parent / 'mac80'))
    import assemble
    return assemble.assemble_source(path.read_text(encoding='ascii'))[0]


def simulate(obj, commands, budget, expanded=False):
    """Run one INT80 session and return its console transcript."""
    with tempfile.TemporaryDirectory(prefix='int80-') as directory:
        Path(directory, 'FOR21.DAT').write_text(obj, encoding='ascii')
        image = UPSTREAM / ('int80e.sav' if expanded else 'int80.sav')
        runner = PDP10(image, commands, directory)
        try:
            native = NATIVE.is_file() and not os.environ.get('PLM80_PYTHON')
            return (runner.run_native(budget) if native else runner.run(budget)).replace('\r', '')
        except (RuntimeError, OSError, StopIteration, ZeroDivisionError) as error:
            raise CompileError(f'{image.name}: {error}\n{runner.output.getvalue()}') from error


ROW = re.compile(r'^[ *]([01]{4})[ *](\d{3})[ *](\d{3})[ *](\d{3})[ *](\d{3})[ *](\d{3})[ *](\d{3})'
                 r'[ *](\d{3})[ *](\d{5})[ *](\d{5})[ *](\d{5})$')
FIELDS = ('flags', 'a', 'b', 'c', 'd', 'e', 'h', 'l', 'hl', 'sp', 'pc')


def registers(match):
    values = dict(zip(FIELDS, match.groups()))
    flags = values.pop('flags')
    state = {name: int(value) for name, value in values.items()}
    # INT80 shows the carry, zero, sign and parity flags as CYZSP digits.
    state.update(zip(('cy', 'z', 's', 'p'), (int(flag) for flag in flags)))
    return state


def run_program(memory, expanded=False, watch=(), limit=2_000_000_000):
    """Run a CP/M program to its exit.

    Return the console output, the program's own INT80 states (from its first
    instruction to its exit, without the BDOS stub, INT80's startup states and
    the final HLT), and the registers before each executed instruction at a
    watched address, as (address, registers) pairs.
    """
    top = MEMORY[expanded]
    environment = Environment(top)
    if memory and (min(memory) < TPA or max(memory) >= environment.entry - 0x100):
        raise CompileError(f'program must fit between {TPA:04X}H and {environment.entry - 0x100:04X}H '
                           f'in {"INT80E" if expanded else "INT80"}\'s {top // 1024}K')
    image = dict(environment.memory)
    image.update(memory)
    points = sorted({environment.stub, *environment.outputs, *watch})
    traces = ''.join(f'TRACE {a}.\n' for a in points)
    session = (f'$F\nLOAD 7.\n{traces}SET SP = {environment.stack}.\nCYCLE {limit}.\n'
               f'SET PC = {TPA}.\nGO.\nDIS CPU.\n$E\n')
    # Roughly 35 PDP-10 instructions simulate one 8080 state.
    transcript = simulate(mac80_object(image), session, 100 * limit + 50_000_000, expanded)
    for line in transcript.splitlines():
        if 'ERROR' in line:
            raise CompileError('INTERP/80: ' + line.strip())
    if 'CYCLE AT' in transcript:
        raise CompileError(f'INTERP/80: no exit within {limit} states')
    halted = re.search(r'^HLT CYCLE (\d+)$', transcript, re.M)
    if not halted:
        raise CompileError('INTERP/80 did not halt:\n' + transcript)
    output, watched, calls = [], [], []
    state = None
    for line in transcript.splitlines():
        row = ROW.match(line)
        if row:
            state = registers(row)
            continue
        if state is None or not line.strip():
            continue
        pc = state['pc']
        if pc == environment.stub:
            calls.append([state['c'], 0])
        elif pc in environment.outputs:
            output.append(chr(state['a']))
            calls[-1][1] += 1
        if pc in watch:
            watched.append((pc, state))
        state = None
    final = [registers(row) for row in map(ROW.match, transcript.splitlines()) if row][-1]
    stopped = final['pc'] - 1  # INT80 shows the PC after the HLT
    if stopped == environment.unsupported:
        raise CompileError(f'BDOS function {calls[-1][0]} is not supported')
    if stopped not in (0, environment.boot):
        raise CompileError(f'program executed HLT at {stopped:04X}H')
    # Unlike INTERP/8, INTERP/80 adds no states when a session starts.
    states = int(halted.group(1)) - sum(environment.states(*call) for call in calls)
    if stopped == 0:
        states -= 7  # the HLT at 0
    return ''.join(output), states, watched


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('program', type=Path, help='CP/M .COM file, Intel HEX file, or MAC80 source')
    parser.add_argument('--expanded', action='store_true',
                        help='use INT80E, the expanded simulator with 48K of memory instead of 24K')
    parser.add_argument('-n', '--states', type=int, default=2_000_000_000,
                        help='fail without an exit after this many states')
    parser.add_argument('-p', action='store_true', help="report the program's INTERP/80 states")
    args = parser.parse_args()
    try:
        output, states, _ = run_program(load_program(args.program), args.expanded, limit=args.states)
        sys.stdout.write(output)
        if args.p:
            print(f'{states} states')
    except (OSError, ValueError, CompileError) as error:
        print(f'INT80: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
