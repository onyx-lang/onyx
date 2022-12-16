#!/bin/sh

. ../settings.sh

# FLAGS="-g3 -O2 -DOVM_DEBUG=1"
# FLAGS="-g3 -DOVM_VERBOSE=1"
FLAGS="-Ofast"
LIBS="-pthread"
TARGET="../shared/lib/linux_$(uname -m)/lib/libovmwasm.so"
C_FILES="src/wasm.c src/vm/*.c src/wasm/*.c src/debug/*.c"
INCLUDES="-I../shared/include -Iinclude"

echo "Compiling libovmwasm.so"
$CC $FLAGS $INCLUDES -shared -fPIC -o $TARGET $C_FILES $LIBS $WARNINGS

