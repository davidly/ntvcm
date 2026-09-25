@echo off

rem Select the native MSVC toolchain for the Windows platform.
set "MSVC_ARCH=%PROCESSOR_ARCHITEW6432%"
if not defined MSVC_ARCH set "MSVC_ARCH=%PROCESSOR_ARCHITECTURE%"

if /I "%MSVC_ARCH%"=="x86" (
    set "MSVC_ARCH=x86"
    set "MSVC_COMPONENT=Microsoft.VisualStudio.Component.VC.Tools.x86.x64"
) else if /I "%MSVC_ARCH%"=="AMD64" (
    set "MSVC_ARCH=amd64"
    set "MSVC_COMPONENT=Microsoft.VisualStudio.Component.VC.Tools.x86.x64"
) else if /I "%MSVC_ARCH%"=="ARM64" (
    set "MSVC_ARCH=arm64"
    set "MSVC_COMPONENT=Microsoft.VisualStudio.Component.VC.Tools.ARM64"
) else (
    echo Unsupported Windows platform: %MSVC_ARCH% 1>&2
    exit /b 1
)

set "VSWHERE_ROOT=%ProgramFiles(x86)%"
if not defined VSWHERE_ROOT set "VSWHERE_ROOT=%ProgramFiles%"
set "VSWHERE=%VSWHERE_ROOT%\Microsoft Visual Studio\Installer\vswhere.exe"
if not exist "%VSWHERE%" (
    echo Visual Studio Installer's vswhere.exe was not found. 1>&2
    exit /b 1
)

set "VCVARSALL="
for /f "usebackq delims=" %%I in (`"%VSWHERE%" -latest -products * -requires %MSVC_COMPONENT% -find VC\Auxiliary\Build\vcvarsall.bat`) do set "VCVARSALL=%%I"
if not defined VCVARSALL (
    echo A Visual Studio C++ toolchain was not found. 1>&2
    exit /b 1
)

call "%VCVARSALL%" %MSVC_ARCH% >nul
if errorlevel 1 (
    echo The MSVC %MSVC_ARCH% toolchain could not be initialized. 1>&2
    exit /b 1
)

rem /jumptablerdata is only supported by the x64 compiler.
set "MSVC_ARCH_OPTIONS="
if /I "%MSVC_ARCH%"=="amd64" set "MSVC_ARCH_OPTIONS=/jumptablerdata"

exit /b 0
