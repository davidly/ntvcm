@echo off
rem Reassemble the examples to .COM files.
setlocal
pushd "%~dp0" || exit /b 1
if defined PYTHON goto python_ready
where py >nul 2>nul
if errorlevel 1 (set "PYTHON=python") else (set "PYTHON=py -3")
:python_ready
for %%a in (hello sieve e ttt) do (
    %PYTHON% assemble.py examples\%%a.asm
    if errorlevel 1 goto failed
)
popd
exit /b 0
:failed
set "result=%errorlevel%"
popd
exit /b %result%
