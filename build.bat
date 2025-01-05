@echo off

REM Compile the compiler
set SOURCE_FILES=compiler/src/library_main.c compiler/cli/main.c compiler/src/astnodes.c compiler/src/builtins.c compiler/src/checker.c compiler/src/clone.c compiler/src/doc.c compiler/src/entities.c compiler/src/errors.c compiler/src/lex.c compiler/src/parser.c compiler/src/types.c compiler/src/utils.c compiler/src/wasm_emit.c compiler/src/wasm_runtime.c compiler/src/extensions.c

if "%1" == "1" (
    set FLAGS=/Od /MTd /Z7
) else (
    set FLAGS=/O2 /MT /Z7
)

for /f "delims=" %%i in ('cd') do set PWD=%%i

set LINK_OPTIONS="%PWD%\shared\lib\windows_x86_64\lib\wasmer.lib" ws2_32.lib Advapi32.lib userenv.lib bcrypt.lib kernel32.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:msvcrtd.lib
set FLAGS=%FLAGS% "/I%PWD%\shared\include" /DONYX_RUNTIME_LIBRARY=wasmer

rc.exe misc/icon_resource.rc
cl.exe %FLAGS% /Icompiler/include /std:c17 /TC %SOURCE_FILES% /link /IGNORE:4217 %LINK_OPTIONS% /DEBUG /OUT:onyx.exe /incremental:no /opt:ref /subsystem:console misc\icon_resource.res

REM Don't continue if we had compilation errors. This prevents CI to succeed.
if %ERRORLEVEL% neq 0 (
    echo Compiler compilation failed.
    exit /b %ERRORLEVEL%
)

set SOURCE_FILES=compiler/src/library_main.c compiler/src/astnodes.c compiler/src/builtins.c compiler/src/checker.c compiler/src/clone.c compiler/src/doc.c compiler/src/entities.c compiler/src/errors.c compiler/src/lex.c compiler/src/parser.c compiler/src/types.c compiler/src/utils.c compiler/src/wasm_emit.c compiler/src/wasm_runtime.c compiler/src/extensions.c
cl.exe %FLAGS% /Icompiler/include /std:c17 /TC %SOURCE_FILES% /link /DLL /IGNORE:4217 %LINK_OPTIONS% /OUT:onyx.dll

REM Don't continue if we had compilation errors. This prevents CI to succeed.
if %ERRORLEVEL% neq 0 (
    echo Compiler library compilation failed.
    exit /b %ERRORLEVEL%
)

del *.pdb > NUL 2> NUL
del *.ilk > NUL 2> NUL
del *.obj > NUL 2> NUL
del *.exp > NUL 2> NUL
del misc\icon_resource.res

cl /MT /std:c17 /TC /I compiler/include /I shared/include /D_USRDLL /D_WINDLL runtime\onyx_runtime.c /link /DLL ws2_32.lib bcrypt.lib Synchronization.lib kernel32.lib /OUT:onyx_runtime.dll

if %ERRORLEVEL% neq 0 (
    echo Onyx runtime compilation failed.
    exit /b %ERRORLEVEL%
)

del onyx_runtime.obj
del onyx_runtime.lib
del onyx_runtime.exp

if "%1" == "dist" (
    mkdir dist
    xcopy core dist\core /s /e /h /I
    xcopy examples dist\examples /s /e /h /I

    mkdir dist\misc
    copy misc\onyx-windows.sublime-build dist\misc\onyx-windows.sublime-build
    copy misc\onyx-mode.el dist\misc\onyx-mode.el
    copy misc\onyx.sublime-syntax dist\misc\onyx.sublime-syntax
    copy misc\vscode\onyxlang-0.1.9.vsix dist\misc\onyxlang-0.1.9.vsix

    mkdir dist\include
    copy shared\include\onyx.h dist\include\onyx.h

    mkdir dist\lib
    copy onyx_runtime.dll dist\lib\onyx_runtime.dll
    copy onyx.dll dist\lib\onyx.dll
    copy onyx.lib dist\lib\onyx.lib
    copy onyx.exe dist\onyx.exe

    mkdir dist\tools
    copy scripts\onyx-pkg.onyx dist\tools\onyx-pkg.onyx
    mkdir dist\tools\pkg_templates
    copy scripts\default.json  dist\tools\pkg_templates\default.json
    copy scripts\lsp.wasm dist\tools\lsp.wasm

    powershell Compress-Archive dist onyx.zip
    move onyx.zip dist/onyx.zip
)
