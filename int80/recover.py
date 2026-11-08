#!/usr/bin/env python3
"""Re-extract the original INTERP/80 1.5 images, INT80 and INT80E, from Tymshare tape 169278."""
import argparse
from pathlib import Path
import sys

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / 'plm80'))
from recover import recover  # noqa: E402

IMAGES = {
    'int80.sav': '8da64ca5b89b256556e08f52e311069dce06920bc54b32799e4acde84f79c279',
    'int80e.sav': '77f7c0aab8f743b96a67a626c7a451682576705e71574c376b444292ac598419',
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('tape', type=Path)
    parser.add_argument('--output', type=Path, default=HERE / 'upstream')
    args = parser.parse_args()
    recover(args.tape, args.output, IMAGES)


if __name__ == '__main__':
    main()
