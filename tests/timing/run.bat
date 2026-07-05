@echo off
rem Regression check for the Z80 cycle-timing bugs fixed in commit
rem "Fix Z80 T-state undercounting: DJNZ/JR, conditional JP/CALL, IX/IY
rem indexed memory". See README.md for how each expected value was derived
rem and for the red/green (pre-fix vs post-fix) numbers.
rem
rem Each expected value already includes the +24 T constant overhead from
rem ntvcm executing the real CP/M warm-boot JP chain after RET-to-0, which
rem a minimal raw-CPU harness (z88dk-ticks) does not execute.
setlocal enabledelayedexpansion
cd /d "%~dp0"
set "NTVCM=..\..\ntvcm.exe"
set fail=0

call :check djnz_loop.com 3364
call :check jp_cc_taken.com 3625
call :check jp_cc_not_taken.com 6948

exit /b %fail%

:check
set "prog=%~1"
set "expected=%~2"
set "line="
for /f "usebackq delims=" %%L in (`%NTVCM% -p %prog% 2^>^&1 ^| findstr /c:"cycles:"`) do set "line=%%L"
for /f "tokens=1,2 delims=:" %%A in ("!line!") do set "num=%%B"
set "num=!num: =!"
set "num=!num:,=!"
if "!num!"=="!expected!" (
    echo PASS !prog!: !num! cycles
) else (
    echo FAIL !prog!: expected !expected! cycles, got !num!
    set fail=1
)
goto :eof
