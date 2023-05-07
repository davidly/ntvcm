@echo off
if "%CLIB%" == "" goto END
if exist azsample.d64 del azsample.d64
call cformat -1 azsample.d64
call ctools azsample.d64 p exmpl.com
call ctools azsample.d64 p keybrd.com
call ctools azsample.d64 p ov.com
call ctools azsample.d64 p ov1.ovr
call ctools azsample.d64 p ov2.ovr
call ctools azsample.d64 p find.com
call ctools azsample.d64 p textfile.com
:END