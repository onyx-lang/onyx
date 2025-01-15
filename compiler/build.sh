#!/bin/sh

C_FILES="library_main astnodes builtins checker clone doc entities errors lex parser types utils wasm_emit extensions "
LIBS="-lpthread -ldl -lm"
INCLUDES="-I./include -I../shared/include -I../shared/include/dyncall"

WARNINGS='-Wimplicit -Wmisleading-indentation -Wparentheses -Wsequence-point -Wreturn-type -Wshift-negative-value -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wsign-compare -Wstrict-overflow -Wtrigraphs -Waddress'

if [ "$1" = "debug" ]; then
    FLAGS="$WARNINGS -g3"
else
    FLAGS="$WARNINGS -O3"
fi

FLAGS="$FLAGS -DENABLE_DEBUG_INFO -fvisibility=hidden"

if [ ! -z ${ONYX_TARGET+x} ]; then
    FLAGS="$FLAGS --target=$ONYX_TARGET"
fi

if [ ! -z ${ONYX_RUNTIME_LIBRARY+x} ]; then
    FLAGS="$FLAGS -DONYX_RUNTIME_LIBRARY=$ONYX_RUNTIME_LIBRARY"
    C_FILES="${C_FILES}wasm_runtime "

    case "${ONYX_RUNTIME_LIBRARY}" in
        ovmwasm)
            LIBS="../shared/lib/$ONYX_ARCH/lib/libovmwasm.a $LIBS"
            FLAGS="$FLAGS -DUSE_OVM_DEBUGGER"
            ;;

        wasmer)
            wasmer_lib="$(wasmer config --libdir || echo $WASMER_LIB_PATH)"
            LIBS="$wasmer_lib/libwasmer.a $LIBS"
            ;;

        *)
            echo "Unknown WebAssembly runtime '$ONYX_RUNTIME_LIBRARY'. Aborting.";
            exit 1
            ;;
    esac
fi

case "$ONYX_ARCH" in
    *darwin*)
        LIBS="$LIBS -lffi -framework CoreFoundation -framework SystemConfiguration"
        ;;
esac

if [ "$ONYX_USE_DYNCALL" = "1" ] && [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
    LIBS="$LIBS ../shared/lib/$ONYX_ARCH/lib/libdyncall_s.a ../shared/lib/$ONYX_ARCH/lib/libdyncallback_s.a"
    FLAGS="$FLAGS -DUSE_DYNCALL"
fi

case "$(uname)" in
    Linux)  suffix='so' ;;
    *BSD)   suffix='so' ;;
    Darwin) suffix='dylib' ;;
    *)      suffix='dll' ;;
esac

echo "Compiling libonyx.$suffix"
for c_file in $(echo "$C_FILES" | sed 's/ /\n/g;s/\([a-zA-Z_0-9]*\)\n/src\/\1.c\n/g;s/\n/ /g'); do
    $ONYX_CC $FLAGS $INCLUDES -fPIC -o $(basename $c_file).o -c $c_file
done

echo "Compiling onyx executable"
$ONYX_CC $INCLUDES $FLAGS cli/main.c *.o -o onyx $LIBS

FLAGS=""
if [ ! -z ${ONYX_TARGET+x} ]; then
    FLAGS="$FLAGS --target=$ONYX_TARGET"
fi
$ONYX_CC -shared $FLAGS -o "libonyx.$suffix" *.o $LIBS

rm *.o
