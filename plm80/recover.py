#!/usr/bin/env python3
"""Re-extract the original PL/M-80 4.1 images from Tymshare tape 169278."""
import argparse
import bz2
import hashlib
from pathlib import Path
import struct

TAPE_SHA256 = 'efc4e6661dc5033f3c95c399a491608bd3aace1ab665ecf9ab2922f620410315'
IMAGES = {
    'plm81.sav': '3c3b416a18ad1e47aec178ab5cc415ffd7a23c306d4f94ebbf68996d5c0cf6cb',
    'plm82.sav': '35e1f1b0bf25f15a29f89a206551e69aad5fb8322b5e5dc420e51caab4c76409',
}


def word(record, index):
    first = index * 5
    b = record[first:first + 5]
    if len(b) != 5:
        raise ValueError('truncated tape word')
    return (int.from_bytes(b[:4], 'big') << 4) | b[4]


def sixbit(value):
    return ''.join(chr(((value >> shift) & 63) + 32)
                   for shift in (30, 24, 18, 12, 6, 0)).strip()


def records(stream):
    while True:
        header = stream.read(4)
        if not header:
            return
        length, = struct.unpack('<I', header)
        if length == 0:  # tape mark
            continue
        if length & 0x80000000:
            raise ValueError('unsupported tape error or gap')
        data = stream.read(length)
        trailer = stream.read(4)
        # The archive uses E11 framing; also accept SIMH's odd-byte padding.
        if trailer != header and length & 1:
            trailer = trailer[1:] + stream.read(1)
        if trailer != header or len(data) != length or length % 5:
            raise ValueError('invalid tape record framing')
        yield data


def recover(tape, destination, wanted=IMAGES):
    """Extract the files named in wanted, {name: SHA-256}, from (UPL) on the tape."""
    digest = hashlib.sha256()
    with tape.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            digest.update(block)
    if digest.hexdigest() != TAPE_SHA256:
        raise ValueError('tape checksum mismatch (expected 169278.tape.bz2)')
    found = {}
    current = None
    with bz2.open(tape, 'rb') as stream:
        for record in records(stream):
            tag = word(record, 0) >> 18
            if tag == 0o777777:  # TITO file header, then inline file data
                directory = sixbit(word(record, 0o71))
                name = sixbit(word(record, 4)).lower()
                ext = sixbit(word(record, 5) & (0o777777 << 18)).lower()
                filename = name + '.' + ext
                current = filename if directory == 'UPL' and filename in wanted else None
                if current:
                    found[current] = bytearray()
                offset = 0o101
            elif tag == 0:  # continuation record; word zero is its length
                offset = 1
            else:
                current = None
                continue
            if current:
                # Include the last word of the first record. The old tito
                # extractor's size-offset expression drops that word.
                for index in range(offset, len(record) // 5):
                    found[current].extend(struct.pack('<Q', word(record, index)))
                if len(found) == len(wanted) and all(hashlib.sha256(found[name]).hexdigest() == digest
                                          for name, digest in wanted.items()):
                    break
    for name, expected in wanted.items():
        if name not in found or hashlib.sha256(found[name]).hexdigest() != expected:
            raise ValueError(f'{name}: recovered image checksum mismatch')
    destination.mkdir(parents=True, exist_ok=True)
    for name, image in found.items():
        (destination / name).write_bytes(image)
        print(f'recovered {name}: {len(image)} bytes; SHA-256 verified')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('tape', type=Path)
    parser.add_argument('--output', type=Path, default=Path(__file__).resolve().parent / 'upstream')
    args = parser.parse_args()
    recover(args.tape, args.output)


if __name__ == '__main__':
    main()
