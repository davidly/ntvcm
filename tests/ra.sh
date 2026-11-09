#!/usr/bin/env bash
# Run the 8080 and Z80 CPU exercisers. The programs' names are uppercase,
# which matters on case-sensitive file systems.
cd "$(dirname "$0")"
for test in "-8 8080EX1.COM" "-8 8080PRE.COM" "-8 TST8080.COM" "-8 TEST.COM" "-8 CPUTEST.COM" "CPUTEST.COM"; do
    echo "test ${test}"
    ../ntvcm ${test}
    echo
done

# Also run zexsup and zexusup, cpm_compilers' supplemental and undocumented
# Z80 tests, assembled with its M80 and L80, when that repo is beside this
# one or CPM_COMPILERS names it. They're built in a scratch directory so
# nothing is written into cpm_compilers. ZEXUSUP's last test ends by
# stopping the emulator on purpose.
m80="${CPM_COMPILERS:-../../cpm_compilers}/m80"
if [ -f "$m80/ZEXSUP.MAC" ] && [ -f "$m80/ZEXUSUP.MAC" ] && [ -f "$m80/M80.COM" ] && [ -f "$m80/L80.COM" ]; then
    ntvcm="$(pwd)/../ntvcm"
    work="$(mktemp -d)"
    cp "$m80/M80.COM" "$m80/L80.COM" "$m80/ZEXSUP.MAC" "$m80/ZEXUSUP.MAC" "$work"
    for test in ZEXSUP ZEXUSUP; do
        echo "test ${test}.COM from cpm_compilers"
        ( cd "$work"
          "$ntvcm" M80 =${test}.MAC /X /O /L /Z > /dev/null
          "$ntvcm" L80 ${test},${test}/N/E > /dev/null
          if [ -f ${test}.COM ]; then "$ntvcm" ${test}.COM; else echo "${test} could not be built"; fi )
        echo
    done
    rm -rf "$work"
else
    echo "cpm_compilers not found beside ntvcm (or in CPM_COMPILERS): skipping zexsup and zexusup"
fi
