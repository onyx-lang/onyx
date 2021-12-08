@echo off

call "tools\dev.bat"

cl /MT /std:c17 /TC /I include /I lib/common/include /IC:\tools\glfw-3.3.5.bin.WIN64\include /D_USRDLL /D_WINDLL modules\glfw3\onyx_glfw3.c /link C:\tools\glfw-3.3.5.bin.WIN64\lib-vc2019\glfw3dll.lib opengl32.lib ws2_32.lib Advapi32.lib userenv.lib bcrypt.lib  libcmt.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:msvcrtd.lib /DLL /OUT:onyx_glfw3.dll

del onyx_glfw3.obj
del onyx_glfw3.lib
del onyx_glfw3.exp