#!/bin/sh

. ../settings.sh

$CC -shared -fpic -w \
    -o ../bin/onyx_runtime.so \
    -I ../shared/include -I ../compiler/include \
    ./onyx_runtime.c \
    -lpthread

echo "Installing onyx_runtime.so"
sudo mv "../bin/onyx_runtime.so" "$CORE_DIR/lib/onyx_runtime.so"
