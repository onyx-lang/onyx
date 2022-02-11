@echo off

REM Compile the compiler
set SOURCE_FILES=src/onyx.c src/astnodes.c src/builtins.c src/checker.c src/clone.c src/doc.c src/entities.c src/errors.c src/lex.c src/parser.c src/symres.c src/types.c src/utils.c src/wasm_emit.c src/wasm_runtime.c

if "%1" == "1" (
    set FLAGS=/Od /MTd /Z7
) else (
    set FLAGS=/O2 /MT /Z7
)

for /f "delims=" %%i in ('cd') do set PWD=%%i

set LINK_OPTIONS="%PWD%\lib\windows_x86_64\lib\wasmer.lib" ws2_32.lib Advapi32.lib userenv.lib bcrypt.lib kernel32.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:msvcrtd.lib
set FLAGS=%FLAGS% "/I%PWD%\lib\common\include" /DENABLE_RUN_WITH_WASMER=1

rc.exe misc/icon_resource.rc
cl.exe %FLAGS% /Iinclude /std:c17 /TC %SOURCE_FILES% /link /IGNORE:4217 %LINK_OPTIONS% /DEBUG /OUT:onyx.exe /incremental:no /opt:ref /subsystem:console misc\icon_resource.res

del *.pdb > NUL 2> NUL
del *.ilk > NUL 2> NUL
del *.obj > NUL 2> NUL
del misc\icon_resource.res

REM Compile the onyxrun tool
set SOURCE_FILES=src/onyxrun.c src/wasm_runtime.c

rc.exe misc/icon_resource.rc
cl.exe %FLAGS% /Iinclude /std:c17 /TC %SOURCE_FILES% /link /IGNORE:4217 %LINK_OPTIONS% /DEBUG /OUT:onyxrun.exe /incremental:no /opt:ref /subsystem:console misc\icon_resource.res

del *.pdb > NUL 2> NUL
del *.ilk > NUL 2> NUL
del *.obj > NUL 2> NUL
del misc\icon_resource.res

call modules\onyx_runtime\build
