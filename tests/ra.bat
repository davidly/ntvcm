echo test 8080ex1.com
ntvcm -8 8080ex1.com

echo:
echo test 8080pre.com
ntvcm -8 8080pre.com

echo:
echo test tst8080.com
ntvcm -8 tst8080.com

echo:
echo test test.com
ntvcm -8 test.com

echo:
echo test cputest in 8080 mode
ntvcm -8 cputest.com

echo:
echo test cputest in Z80 mode
ntvcm cputest.com
echo:

rem Also run zexsup and zexusup, cpm_compilers' supplemental and undocumented
rem Z80 tests, assembled with its M80 and L80, when that repo is beside this
rem one or CPM_COMPILERS names it. They're built in a scratch directory so
rem nothing is written into cpm_compilers. ZEXUSUP's last test ends by
rem stopping the emulator on purpose.
setlocal
if "%CPM_COMPILERS%"=="" (set "m80=%~dp0..\..\cpm_compilers\m80") else (set "m80=%CPM_COMPILERS%\m80")
if not exist "%m80%\ZEXSUP.MAC" goto nocpm
if not exist "%m80%\ZEXUSUP.MAC" goto nocpm
if not exist "%m80%\M80.COM" goto nocpm
if not exist "%m80%\L80.COM" goto nocpm
set "work=%TEMP%\ntvcm-zexsup"
if exist "%work%" rmdir /s /q "%work%"
mkdir "%work%"
for %%f in (M80.COM L80.COM ZEXSUP.MAC ZEXUSUP.MAC) do copy "%m80%\%%f" "%work%" >nul
pushd "%work%"
for %%t in (ZEXSUP ZEXUSUP) do (
    echo test %%t.COM from cpm_compilers
    ntvcm M80 =%%t.MAC /X /O /L /Z >nul
    ntvcm L80 %%t,%%t/N/E >nul
    if exist %%t.COM (ntvcm %%t.COM) else (echo %%t could not be built)
    echo:
)
popd
rmdir /s /q "%work%"
goto done
:nocpm
echo cpm_compilers not found beside ntvcm (or in CPM_COMPILERS): skipping zexsup and zexusup
:done
endlocal
