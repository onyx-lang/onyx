@echo off

cl /MT /std:c17 /TC /I include /I lib/common/include /I "C:\Program Files (x86)\OpenAL 1.1 SDK\include" /D_USRDLL /D_WINDLL modules\openal\onyx_openal.c /link "C:\Program Files (x86)\OpenAL 1.1 SDK\libs\Win64\OpenAL32.lib" /DLL /OUT:onyx_openal.dll