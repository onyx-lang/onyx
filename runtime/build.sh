#!/bin/sh

echo "Compiling onyx_runtime.so"
$ONYX_CC -shared -fpic -w -O2 \
    -o onyx_runtime.so \
    -I ../shared/include -I ../compiler/include \
    ./onyx_runtime.c \
    -lpthread
