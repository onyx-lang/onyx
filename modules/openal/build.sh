#/bin/sh

onyx run modules/openal/build --generate-foreign-info
gcc -shared -fPIC modules/openal/onyx_openal.c -lopenal -I include -I lib/common/include -o ./modules/openal/onyx_openal.so