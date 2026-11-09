#!/usr/bin/env python3
"""PDP-10 integer/Fortran runtime subset for the recovered PL/M-80 4.1, MAC80 and INT80.

Adapted from ntv8008's plm8/pdp10.py, which runs Intel's 8008 tools from the
same tape.

This is a compiler runner, not a general PDP-10 or TYMCOM-X emulator.
Instruction encodings and monitor calls: DEC PDP-10 System Reference Manual
(1968), TOPS-10 Monitor Calls Manual. See README.md for compatibility details.
"""
import hashlib
import io
import os
from pathlib import Path
import struct
import subprocess
import tempfile

# pdp10.cxx, built by m.sh/m.bat: a native translation of execute() below.
NATIVE = Path(__file__).with_name('pdp10.exe' if os.name == 'nt' else 'pdp10')

MASK = (1 << 36) - 1
RH = (1 << 18) - 1
SIGN = 1 << 35


def signed(value):
    return value - (1 << 36) if value & SIGN else value


def pred(value, condition):
    return (False, value < 0, value == 0, value <= 0,
            True, value >= 0, value != 0, value > 0)[condition]


class Finished(Exception):
    pass


# SHA-256 of each accepted image, and whether it was built for the KI10.
IMAGES = {
    '3c3b416a18ad1e47aec178ab5cc415ffd7a23c306d4f94ebbf68996d5c0cf6cb': False,  # PLM81
    '35e1f1b0bf25f15a29f89a206551e69aad5fb8322b5e5dc420e51caab4c76409': False,  # PLM82
    'c08a664dec521cbda5622a67668405084f2f1196d5562c8fe56fc37a9e6d095d': False,  # MAC80
    '5b703a57e771e3dbe0588955fd4fc1c0d415de15decdb2fda97a17e56ed0f546': True,   # MAC80E
    '8da64ca5b89b256556e08f52e311069dce06920bc54b32799e4acde84f79c279': False,  # INT80
    '77f7c0aab8f743b96a67a626c7a451682576705e71574c376b444292ac598419': True,   # INT80E
}
# Disk buffers, one 0o1000-word area per channel, sit above every image's
# code and data (INT80E's reaches 0o101032).
OUT_BUFFERS = 0o700000
IN_BUFFERS = 0o740000


class PDP10:
    ki = False  # KA10 unless the image was built for the KI10

    def __init__(self, image, source, directory):
        self.m = [0] * (1 << 18)
        self.channels = {}
        self.directory = Path(directory)
        self.input = io.StringIO(source)
        self.output = io.StringIO()
        raw = Path(image).read_bytes()
        digest = hashlib.sha256(raw).hexdigest()
        if digest not in IMAGES:
            raise ValueError('compiler image checksum mismatch')
        # MAC80E and INT80E were built for the KI10, whose stack and AOBJ
        # instructions count the two halves separately; their runtime refuses
        # to run on a KA10.
        self.ki = IMAGES[digest]
        if len(raw) % 8:
            raise ValueError('truncated compiler image')
        words = struct.unpack('<' + 'Q' * (len(raw) // 8), raw)
        offset = 0
        while offset < len(words):
            word = words[offset]
            offset += 1
            if word & SIGN:
                length = (-(word >> 18)) & RH
                address = (word & RH) + 1
                if address + length > len(self.m) or offset + length > len(words):
                    raise ValueError('invalid compiler load block')
                self.m[address:address + length] = words[offset:offset + length]
                offset += length
            elif word >> 27 == 0o254:
                self.pc = word & RH
                break
            else:
                raise ValueError('invalid compiler image record')
        else:
            raise ValueError('missing compiler entry point')
        # Install the saved Fortran local-UUO vector, as the monitor would.
        self.m[0o41] = self.m[0o122]
        # The original wrapper logs usage to the discontinued Tymshare RTYACT
        # service. Return from that wrapper only; compiler code is unchanged.
        # Verify the surrounding instructions so another image cannot be
        # mistaken for one of the recovered executables. MAC80's wrapper has
        # one more instruction (MOVNI 2,1) before its return, MAC80E's one fewer.
        matches = [a for a in range(len(self.m) - 11)
                   if self.m[a + 3] == 0o200076000000
                   and self.m[a + 4] == 0o200136000001
                   and 0o267716000002 in (self.m[a + 8], self.m[a + 9], self.m[a + 10])]
        if len(matches) != 1:
            raise ValueError('unrecognized Tymshare accounting wrapper')
        self.m[matches[0]] = 0o267716000002  # JRA 16,2(16)

    def run(self, budget=50_000_000):
        memory = self.m
        execute = self.execute
        try:
            for _ in range(budget):
                address = self.pc
                self.pc = (address + 1) & RH
                execute(memory[address], address)
            raise RuntimeError('compiler instruction limit exceeded')
        except Finished:
            return self.output.getvalue()
        finally:
            for channel in self.channels.values():
                if 'file' in channel:
                    channel['file'].close()

    def run_native(self, budget=50_000_000, state=False):
        """Run the loaded image with the native runner; same results as run().

        With state=True, also copy the final memory and PC back (for tests).
        """
        with tempfile.TemporaryDirectory(prefix='pdp10-') as scratch:
            memory, source, listing, final = (Path(scratch, name) for name in
                                              ('memory.bin', 'input.txt', 'output.txt', 'final.bin'))
            memory.write_bytes(struct.pack('<%dQ' % len(self.m), *self.m))
            source.write_bytes(self.input.getvalue().encode('ascii'))
            command = [str(NATIVE), *(['--ki'] if self.ki else []), str(memory), str(self.pc), str(source),
                       str(self.directory), str(listing), str(budget)]
            result = subprocess.run(command + ([str(final)] if state else []), capture_output=True)
            text = listing.read_bytes().decode('ascii') if listing.exists() else ''
            if state and final.exists():
                words = struct.unpack('<%dQ' % (len(self.m) + 1), final.read_bytes())
                self.m[:] = words[:-1]
                self.pc = words[-1]
        self.output = io.StringIO(text)
        self.output.seek(0, io.SEEK_END)
        if result.returncode:
            raise RuntimeError(result.stderr.decode('ascii', 'replace').strip() or
                               f'native PDP-10 runner failed ({result.returncode})')
        return text

    def step(self, word, delta):
        """Add delta to both halves: carrying across them on a KA10, not on a KI10."""
        if self.ki:
            return ((word >> 18) + delta & RH) << 18 | (word + delta & RH)
        return (word + delta * 0o1000001) & MASK

    def ea(self, ir):
        for _ in range(30):
            a = ir & RH
            x = ir >> 18 & 15
            if x:
                a = a + self.m[x] & RH
            if not ir & 1 << 22:
                return a
            ir = self.m[a]
        raise RuntimeError('PDP-10 indirection limit exceeded')

    def flush_channel(self, ac):
        # Fortran can retain its latest byte pointer in an accumulator while
        # closing/switching units. Track stores as well as the shared header,
        # and flush only bytes not previously sent to the local file.
        c = self.channels.get(ac, {})
        if c.get('ready'):
            h = c['out']
            base = OUT_BUFFERS + ac * 0o1000
            n = c.get('written', 0)
            text = ''.join((chr(self.m[base + i // 5] >> 29 - 7 * (i % 5) & 127) for i in range(c.get('flushed', 0), min(640, n))))
            if 'file' in c:
                c['file'].write(text.encode('ascii'))
                c['file'].flush()
            else:
                self.output.write(text)
            c['flushed'] = n

    def execute(self, ir, at):
        m = self.m
        op = ir >> 27
        ac = ir >> 23 & 15
        if ir & 1 << 22:
            e = self.ea(ir)
        else:
            e = ir & RH
            x = ir >> 18 & 15
            if x:
                e = e + m[x] & RH
        v = m[e]
        av = m[ac]
        form = op & 3
        if op == 0o200:  # MOVE
            m[ac] = v
        elif op == 0o202:  # MOVEM
            m[e] = av
        elif op == 0o201:  # MOVEI
            m[ac] = e
        elif 0o200 <= op <= 0o217:  # MOVE, MOVS, MOVN, MOVM
            val = e if form == 1 else av if form == 2 else v
            grp = op >> 2 & 3
            if grp == 1:
                val = (val << 18 | val >> 18) & MASK
            elif grp == 2:
                val = -val & MASK
            elif grp == 3:
                val = abs(signed(val))
            if form < 2:
                m[ac] = val
            else:
                m[e] = val
                if form == 3 and ac:
                    m[ac] = val
        elif 0o270 <= op <= 0o277 or 0o220 <= op <= 0o237:  # integer arithmetic
            arg = e if form == 1 else v
            if 0o270 <= op <= 0o277:
                val = av + arg if op < 0o274 else av - arg
            elif op < 0o224:
                val = signed(av) * signed(arg)
            elif op >= 0o230 and op < 0o234:
                a = signed(av)
                d = signed(arg)
                val = abs(a) // abs(d) * (-1 if (a < 0) != (d < 0) else 1)
                m[ac + 1 & 15] = a - val * d & MASK
            else:
                raise RuntimeError(('arithmetic', oct(op)))
            val &= MASK
            if form < 2:
                m[ac] = val
            else:
                m[e] = val
                if form == 3:
                    m[ac] = val
        elif 0o300 <= op <= 0o377:  # compare, skip, jump, increment/decrement
            grp = op >> 3 & 7
            k = op & 7
            if grp < 2:
                if pred(signed(av) - signed(e if grp == 0 else v), k):
                    self.pc = self.pc + 1 & RH
            elif grp == 2:
                if pred(signed(av), k):
                    self.pc = e
            elif grp == 3:
                if ac:
                    m[ac] = v
                if pred(signed(v), k):
                    self.pc = self.pc + 1 & RH
            else:
                val = (av if grp in (4, 6) else v) + (1 if grp in (4, 5) else -1) & MASK
                if grp in (4, 6):
                    m[ac] = val
                else:
                    m[e] = val
                    if ac:
                        m[ac] = val
                if pred(signed(val), k):
                    self.pc = e if grp in (4, 6) else self.pc + 1 & RH
        elif 0o400 <= op <= 0o477:  # Boolean operations
            arg = e if form == 1 else v
            f = op >> 2 & 15
            val = 0
            if f & 1:
                val |= av & arg
            if f & 2:
                val |= ~av & arg
            if f & 4:
                val |= av & ~arg
            if f & 8:
                val |= ~(av | arg)
            val &= MASK
            if form < 2:
                m[ac] = val
            else:
                m[e] = val
                if form == 3:
                    m[ac] = val
        elif 0o500 <= op <= 0o577:  # halfword transfers
            src = e if form == 1 else av if form == 2 else v
            dest = av if form < 2 else v
            leftdest = not bool(op & 32)
            leftsrc = leftdest != bool(op & 4)
            half = (src >> 18 if leftsrc else src) & RH
            fill = op >> 3 & 3
            other = (dest & RH if leftdest else dest >> 18) if fill == 0 else 0 if fill == 1 else RH if fill == 2 or half & 1 << 17 else 0
            val = half << 18 | other if leftdest else other << 18 | half
            if form < 2:
                m[ac] = val
            else:
                m[e] = val
                if form == 3 and ac:
                    m[ac] = val
        elif 0o600 <= op <= 0o677:  # bit tests and modifications
            k = op >> 1 & 3
            mod = op >> 4 & 3
            arg = (e << 18 if op & 1 else e) if not op & 8 else (v << 18 | v >> 18) & MASK if op & 1 else v
            yes = bool(av & arg)
            if k == 2 or (k == 1 and (not yes)) or (k == 3 and yes):
                self.pc = self.pc + 1 & RH
            if mod == 1:
                m[ac] = av & ~arg
            elif mod == 2:
                m[ac] = av ^ arg
            elif mod == 3:
                m[ac] = av | arg
        elif op == 0o254:
            if ac == 4:
                raise RuntimeError(('HALT', oct(at), oct(e)))
            self.pc = e
        elif op == 0o255:
            pass
        elif op == 0o260:
            m[ac] = self.step(av, 1)
            m[m[ac] & RH] = self.pc
            self.pc = e
        elif op == 0o261:
            m[ac] = self.step(av, 1)
            m[m[ac] & RH] = v
        elif op == 0o262:
            m[e] = m[av & RH]
            m[ac] = self.step(av, -1)
        elif op == 0o263:
            self.pc = m[av & RH] & RH
            m[ac] = self.step(av, -1)
        elif op == 0o264:
            m[e] = self.pc
            self.pc = e + 1 & RH
        elif op == 0o265:
            m[ac] = self.pc
            self.pc = e
        elif op == 0o266:
            m[e] = av
            m[ac] = e << 18 | self.pc
            self.pc = e + 1 & RH
        elif op == 0o267:
            m[ac] = m[av >> 18 & RH]
            self.pc = e
        elif op == 0o256:
            self.execute(v, e)
        elif op == 0o250:
            (m[ac], m[e]) = (v, av)
        elif op == 0o251:
            a = av >> 18
            d = av & RH
            while d <= e:
                m[d] = m[a]
                a = a + 1 & RH
                d += 1
            m[ac] = (a << 18 | d) & MASK
        elif op in (0o242, 0o240, 0o241):
            n = e if e < 131072 else e - (1 << 18)
            if op == 0o242:
                val = (av << min(n, 36) if n >= 0 else av >> min(-n, 36)) & MASK
            elif op == 0o240:
                val = (signed(av) << min(n, 36) if n >= 0 else signed(av) >> min(-n, 36)) & MASK
            else:
                n %= 36
                val = (av << n | av >> 36 - n) & MASK
            m[ac] = val
        elif op == 0o246:
            n = e if e < 131072 else e - (1 << 18)
            val = av << 36 | m[ac + 1 & 15]
            val = val << min(n, 72) if n >= 0 else val >> min(-n, 72)
            m[ac] = val >> 36 & MASK
            m[ac + 1 & 15] = val & MASK
        elif op in (0o252, 0o253):
            m[ac] = self.step(av, 1)
            if (signed(m[ac]) < 0) == (op == 0o253):
                self.pc = e
        elif 0o133 <= op <= 0o137:  # IBP, ILDB, LDB, IDPB, DPB
            bp = v
            if op in (0o133, 0o134, 0o136):
                pos = (bp >> 30) - (bp >> 24 & 63)
                if pos < 0:
                    pos = 36 - (bp >> 24 & 63)
                    bp = bp & ~RH | bp + 1 & RH
                bp = bp & (1 << 30) - 1 | pos << 30
                m[e] = bp
            if op != 0o133:
                a = self.ea(bp)
                pos = bp >> 30
                sz = bp >> 24 & 63
                mask = (1 << sz) - 1
                if op in (0o134, 0o135):
                    m[ac] = m[a] >> pos & mask
                else:
                    m[a] = m[a] & ~(mask << pos) | (av & mask) << pos
                    if sz == 7:
                        for (ch, c) in self.channels.items():
                            base = OUT_BUFFERS + ch * 0o1000
                            if c.get('ready') and base <= a < base + 128:
                                c['written'] = max(c.get('written', 0), (a - base) * 5 + (29 - pos) // 7 + 1)
        elif op < 0o40:  # Fortran local UUOs, dispatched through the saved vector
            m[0o40] = op << 27 | ac << 23 | e
            self.execute(m[0o41], 0o41)
        elif op == 0o47 and e in (0, 0o16):  # RESET, APRENB
            pass
        elif op == 0o47 and e == 0o11:  # CORE: memory is fixed, so any request succeeds
            m[ac] = 0o400
            self.pc += 1
        elif op == 0o47 and e in (0o23, 0o27, 0o30, 0o24):  # timing/job queries
            m[ac] = 0
        elif op == 0o47 and 0o777736 <= e <= 0o777745:
            # Tymshare software-interrupt setup (CALLI -42 to -33) in newer
            # runtimes such as PLM81's. They halt unless these succeed; no
            # interrupt is ever raised.
            self.pc += 1
        elif op == 0o47 and e >= 0o400000:
            pass
        elif op == 0o51:  # TTCALL
            if ac in (0o11, 0o12, 0o13):
                pass  # CLRBFI, CLRBFO; SKPINC doesn't skip: nothing has been typed
            elif ac == 3:
                a = e
                while True:
                    end = False
                    for sh in (29, 22, 15, 8, 1):
                        c = m[a] >> sh & 127
                        if not c:
                            end = True
                            break
                        self.output.write(chr(c))
                    if end:
                        break
                    a += 1
            elif ac == 1:
                self.output.write(chr(v & 127))
            else:
                raise RuntimeError(('TTCALL', ac, oct(e)))
        elif op == 0o47 and e == 0o41:
            pass  # GETTAB is unavailable; use the runtime's fallback
        elif op == 0o50:  # OPEN
            self.flush_channel(ac)
            if 'file' in self.channels.get(ac, {}):
                self.channels[ac]['file'].close()
            self.channels[ac] = {'mode': m[e], 'out': m[e + 2] >> 18, 'in': m[e + 2] & RH}
            self.pc += 1
        elif op in (0o76, 0o77):  # LOOKUP, ENTER (extended or original block)

            def six(w):
                return ''.join((chr((w >> sh & 63) + 32) for sh in (30, 24, 18, 12, 6, 0))).strip()
            f = e + 2 if v < 64 else e
            name = six(m[f]) + '.' + six(m[f + 1] >> 18 << 18).strip()
            if op == 0o77:
                c = self.channels[ac]
                file = open(self.directory / name, 'w+b')
                if 'file' in c:
                    c['file'].close()  # replaced, as the native runner does
                c['file'] = file
                self.pc += 1
            else:
                try:
                    c = self.channels[ac]
                    file = open(self.directory / name, 'rb')
                    if 'file' in c:
                        c['file'].close()
                    c['file'] = file
                    c['lines'] = iter(c['file'].read().decode('ascii').splitlines())
                    self.pc += 1
                except FileNotFoundError:
                    m[f + 1] = m[f + 1] & ~RH | 0
        elif op == 0o47 and e == 4:
            name = ''.join((chr((av >> sh & 63) + 32) for sh in (30, 24, 18, 12, 6, 0))).strip()
            # DEVCHR: available, input/output capable TTY or directory DSK.
            flags = 0o53 if name == 'TTY' else 0o200047
            m[ac] = (flags << 18) | 0o640003 if name in ('TTY', 'DSK') else 0
        elif op == 0o47 and e in (0o53, 0o101):
            pass  # optional DEVTYP/DEVSIZ; runtime falls back to DEVCHR
        elif op in (0o57, 0o67):  # OUT, OUTPUT
            c = self.channels[ac]
            h = c['out']
            base = OUT_BUFFERS + ac * 0o1000
            self.flush_channel(ac)
            c['written'] = 0
            c['flushed'] = 0
            c['ready'] = True
            # One 128-word buffer in a ring of one: the header points at its
            # link word, whose size field selects the runtime's CRLF line mode.
            m[base - 2] = 0o201 << 18 | base - 2
            m[h] = base - 2
            m[h + 1] = 7 << 24 | base - 1
            m[h + 2] = 640
        elif op in (0o56, 0o66):  # IN, INPUT
            c = self.channels[ac]
            h = c['in']
            base = IN_BUFFERS + ac * 0o1000
            if 'lines' in c:
                line = next(c['lines'], None)
                if line is None:
                    raise RuntimeError('unexpected end of compiler work file')
            else:
                line = self.input.readline()
                if not line:
                    raise RuntimeError('unexpected end of compiler input')
            data = line.rstrip('\n') + '\r\n'
            for i in range(128):
                m[base + i] = 0
            for (i, ch) in enumerate(data):
                m[base + i // 5] |= ord(ch) << 29 - 7 * (i % 5)
            m[h] = base
            m[h + 1] = 7 << 24 | base - 1
            m[h + 2] = len(data)
        elif op == 0o62:
            m[e] = 0
        elif op in (0o70, 0o71):  # CLOSE, RELEASE
            c = self.channels.get(ac, {})
            self.flush_channel(ac)
            if 'file' in c:
                c['file'].flush()
        elif op == 0o47 and e == 0o12:  # EXIT
            for channel in self.channels:
                self.flush_channel(channel)
            raise Finished()
        else:
            raise RuntimeError(f'unsupported PDP-10 opcode {op:03o} at {at:06o}')
