#!/bin/bash
# Regression check for the Z80 cycle-timing bugs fixed in commit
# "Fix Z80 T-state undercounting: DJNZ/JR, conditional JP/CALL, IX/IY
# indexed memory". See README.md for how each expected value was derived
# and for the red/green (pre-fix vs post-fix) numbers.
#
# Each expected value already includes the +24 T constant overhead from
# ntvcm executing the real CP/M warm-boot JP chain after RET-to-0, which
# a minimal raw-CPU harness (z88dk-ticks) does not execute.
set -u
cd "$(dirname "$0")"
NTVCM=../../ntvcm
fail=0

check() {
    local prog=$1 expected=$2
    local out
    out=$( "$NTVCM" -p "$prog" 2>&1 | grep -o 'cycles: *[0-9,]*' | tr -d 'cycles: ,' )
    if [ "$out" = "$expected" ]; then
        echo "PASS $prog: $out cycles"
    else
        echo "FAIL $prog: expected $expected cycles, got $out"
        fail=1
    fi
}

check djnz_loop.com 3364
check jp_cc_taken.com 3625
check jp_cc_not_taken.com 6948

exit $fail
