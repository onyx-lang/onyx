#!/bin/sh

gcc -shared -fPIC modules/opengles/onyx_opengles.c -I include -I lib/linux_x86_64/include -o ./modules/opengles/onyx_opengles.so -lGL