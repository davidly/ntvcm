#!/bin/sh
# Recompile the examples to .COM files.
set -eu
cd "$(dirname "$0")"
for app in hello arithmetic sieve e ttt; do
    "${PYTHON:-python3}" compile.py "examples/$app.plm"
done
