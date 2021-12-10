@echo off

call "tools\dev.bat"

cl /MT /std:c17 /TC /I include /I lib/common/include /D_USRDLL /D_WINDLL modules\onyx_runtime\onyx_runtime.c /link /DLL /OUT:onyx_runtime.dll

del onyx_runtime.obj
del onyx_runtime.lib
del onyx_runtime.exp