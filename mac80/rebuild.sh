#!/bin/sh
# Reassemble the examples to .COM files.
set -eu
cd "$(dirname "$0")"
for app in hello sieve e ttt; do
    "${PYTHON:-python3}" assemble.py "examples/$app.asm"
done
