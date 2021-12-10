#!/bin/sh

gcc -shared -fpic -I include -I lib/common/include modules/onyx_runtime/onyx_runtime.c -o onyx_runtime.so -lpthread