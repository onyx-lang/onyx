@echo off

set SOURCE_FILES=src/onyx.c src/astnodes.c src/builtins.c src/checker.c src/clone.c src/doc.c src/entities.c src/errors.c src/lex.c src/parser.c src/symres.c src/types.c src/utils.c src/wasm_emit.c src/wasm_runtime.c

if "%1" == "1" (
    set FLAGS=/Od /MTd /Z7
) else (
    set FLAGS=/O2 /MT /Z7
)

for /f "delims=" %%i in ('cd') do set PWD=%%i

set LINK_OPTIONS="%PWD%\lib\windows_x86_64\lib\wasmer.lib" ws2_32.lib Advapi32.lib userenv.lib bcrypt.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:msvcrtd.lib
set FLAGS=%FLAGS% "/I%PWD%\lib\windows_x86_64\include" /DENABLE_RUN_WITH_WASMER=1

del *.pdb > NUL 2> NUL
del *.ilk > NUL 2> NUL

cl.exe %FLAGS% /Iinclude /std:c17 /TC %SOURCE_FILES% /link /IGNORE:4217 %LINK_OPTIONS% /DEBUG /OUT:onyx.exe /incremental:no /opt:ref /subsystem:console

del *.obj > NUL 2> NUL
