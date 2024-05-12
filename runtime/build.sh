#!/bin/sh

case "$(uname)" in
    Linux)  suffix='so' ;;
    *BSD)   suffix='so' ;;
    Darwin) suffix='dylib' ;;
    *)      suffix='dll' ;;
esac

FLAGS=""
if [ "$(uname)" = "Darwin" ]; then
    FLAGS="$FLAGS -framework Security"
fi
if [ ! -z ${ONYX_TARGET+x} ]; then
    FLAGS="$FLAGS --target=$ONYX_TARGET"
fi


echo "Compiling onyx_runtime.$suffix"
$ONYX_CC -shared -fpic -w -O2 \
    -o onyx_runtime.$suffix \
    $FLAGS \
    -Wno-incompatible-pointer-types \
    -I ../shared/include -I ../compiler/include \
    ./onyx_runtime.c \
    -lpthread
