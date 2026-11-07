#!/bin/sh
# Build the native PDP-10 runner; compile.py falls back to Python without it.
set -eu
cd "$(dirname "$0")"
${CXX:-c++} -std=c++11 -O3 -flto -Wall -Wextra -pedantic pdp10.cxx -o pdp10
