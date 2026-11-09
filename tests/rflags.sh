#!/usr/bin/env bash
# Z80 SBC HL,rr flag regression test.
# Run from the repo root after building ntvcm:  bash tests/rflags.sh
python3 tests/flagcheck.py ./ntvcm
