#!/usr/bin/env bash
# Z80 and 8080 flag regression tests.
# Run from the repo root after building ntvcm:  bash tests/rflags.sh
python3 tests/flagcheck.py ./ntvcm
python3 tests/aluflagcheck.py ./ntvcm
