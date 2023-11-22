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

echo "Compiling onyx_runtime.$suffix"
$ONYX_CC -shared -fpic -w -O2 \
    -o onyx_runtime.$suffix \
    -I ../shared/include -I ../compiler/include \
    ./onyx_runtime.c \
    $FLAGS \
    -lpthread
