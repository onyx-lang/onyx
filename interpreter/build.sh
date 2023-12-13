#!/bin/sh

# FLAGS="-g3 -O2 -DOVM_DEBUG=1 -fno-stack-protector"
# FLAGS="-g3 -DOVM_VERBOSE=1"
FLAGS="-O3 -fno-stack-protector"
LIBS="-pthread"
TARGET="../shared/lib/$ONYX_ARCH/lib/libovmwasm.a"
C_FILES="src/ovmwasm.c src/vm/*.c src/wasm/*.c src/debug/*.c"
INCLUDES="-I../shared/include -Iinclude"

if [ ! -z ${ONYX_TARGET+x} ]; then
    FLAGS="$FLAGS --target=$ONYX_TARGET"
fi

mkdir -p "build_tmp"

echo "Compiling ovmwasm to $TARGET"
for c_file in $C_FILES; do
    $ONYX_CC $FLAGS $INCLUDES -fPIC -o build_tmp/$(basename $c_file).o -c $c_file $LIBS
done

ar crs "$TARGET" build_tmp/*.o*

rm -r "build_tmp"
