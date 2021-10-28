@echo off

set SOURCE_FILES=src/onyx.c src/astnodes.c src/builtins.c src/checker.c src/clone.c src/doc.c src/entities.c src/errors.c src/lex.c src/parser.c src/symres.c src/types.c src/utils.c src/wasm_emit.c

if "%1" == "1" (
    set FLAGS=/Od /MDd /Z7
) else (
    set FLAGS=/O2 /MT /Z7
)

del *.pdb > NUL 2> NUL
del *.ilk > NUL 2> NUL

cl.exe %FLAGS% /I include /std:c17 /Tc %SOURCE_FILES% /link /DEBUG /OUT:onyx.exe /incremental:no /opt:ref /subsystem:console

del *.obj > NUL 2> NUL
