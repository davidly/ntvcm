#!/usr/bin/env bash
# Z80 instruction cycle-count regression test.
# Run from the repo root after building ntvcm:  bash tests/rcycles.sh
python3 tests/cyclecheck.py ./ntvcm
