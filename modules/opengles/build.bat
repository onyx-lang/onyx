@echo off

call "tools\dev.bat"

cl /MT /std:c17 /TC /I include /I lib/common/include /IC:\dev\heartbreak\lib\common\include /D_USRDLL /D_WINDLL modules\opengles\onyx_opengles.c /link opengl32.lib ws2_32.lib Advapi32.lib userenv.lib bcrypt.lib  libcmt.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:msvcrtd.lib /DLL /OUT:onyx_opengles.dll

del onyx_opengles.obj
del onyx_opengles.lib
del onyx_opengles.exp