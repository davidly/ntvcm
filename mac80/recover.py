#!/usr/bin/env python3
"""Re-extract the original MAC80 2.4 and MAC80E images from Tymshare tape 169278."""
import argparse
from pathlib import Path
import sys

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / 'plm80'))
from recover import recover  # noqa: E402

IMAGES = {
    'mac80.sav': 'c08a664dec521cbda5622a67668405084f2f1196d5562c8fe56fc37a9e6d095d',
    'mac80e.sav': '5b703a57e771e3dbe0588955fd4fc1c0d415de15decdb2fda97a17e56ed0f546',
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('tape', type=Path)
    parser.add_argument('--output', type=Path, default=HERE / 'upstream')
    args = parser.parse_args()
    recover(args.tape, args.output, IMAGES)


if __name__ == '__main__':
    main()
