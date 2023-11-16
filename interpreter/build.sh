#!/bin/sh

# FLAGS="-g3 -O2 -DOVM_DEBUG=1 -fno-stack-protector"
# FLAGS="-g3 -DOVM_VERBOSE=1"
FLAGS="-Ofast -fno-stack-protector"
LIBS="-pthread"
TARGET="../shared/lib/linux_$(uname -m)/lib/libovmwasm.a"
C_FILES="src/ovmwasm.c src/vm/*.c src/wasm/*.c src/debug/*.c"
INCLUDES="-I../shared/include -Iinclude"

mkdir -p "build_tmp"

echo "Compiling ovmwasm to $TARGET"
for c_file in $C_FILES; do
    $ONYX_CC $FLAGS $INCLUDES -fPIC -o $(mktemp -p build_tmp -t XXXXXXX.o) -c $c_file $LIBS
done

ar cr "$TARGET" build_tmp/*.o

rm -r "build_tmp"
