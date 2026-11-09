#!/usr/bin/env bash
# Run the 8080 and Z80 CPU exercisers. The programs' names are uppercase,
# which matters on case-sensitive file systems.
cd "$(dirname "$0")"
for test in "-8 8080EX1.COM" "-8 8080PRE.COM" "-8 TST8080.COM" "-8 TEST.COM" "-8 CPUTEST.COM" "CPUTEST.COM"; do
    echo "test ${test}"
    ../ntvcm ${test}
    echo
done
