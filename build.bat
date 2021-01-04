@echo off

del *.pdb > NUL 2> NUL
del *.ilk > NUL 2> NUL

cl.exe /Od /MDd /Z7 /I include /std:c17 /Tc src/onyx.c src/onyxbuiltins.c src/onyxchecker.c src/onyxclone.c src/onyxdoc.c src/onyxentities.c src/onyxerrors.c src/onyxlex.c src/onyxparser.c src/onyxsempass.c src/onyxsymres.c src/onyxtypes.c src/onyxutils.c src/onyxwasm.c /link /DEBUG /OUT:onyx.exe /incremental:no /opt:ref /subsystem:console

del *.obj > NUL 2> NUL