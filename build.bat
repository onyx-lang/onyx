@echo off

set SOURCE_FILES=src/onyx.c src/onyxastnodes.c src/onyxbuiltins.c src/onyxchecker.c src/onyxclone.c src/onyxdoc.c src/onyxentities.c src/onyxerrors.c src/onyxlex.c src/onyxparser.c src/onyxsempass.c src/onyxsymres.c src/onyxtypes.c src/onyxutils.c src/onyxwasm.c

if "%1" == "1" (
    set FLAGS=/O2 /MT /Z7
) else (
    set FLAGS=/Od /MDd /Z7
)

del *.pdb > NUL 2> NUL
del *.ilk > NUL 2> NUL

cl.exe %FLAGS% /I include /std:c17 /Tc %SOURCE_FILES% /link /DEBUG /OUT:onyx.exe /incremental:no /opt:ref /subsystem:console

del *.obj > NUL 2> NUL