#!/bin/sh

gcc -shared -fPIC modules/opengles/onyx_opengles.c -I include -I lib/common/include -o ./modules/opengles/onyx_opengles.so -lGL