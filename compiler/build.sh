#!/bin/sh

C_FILES="onyx astnodes builtins checker clone doc entities errors lex parser symres types utils wasm_emit "
LIBS="-lpthread -ldl -lm"
INCLUDES="-I./include -I../shared/include -I../shared/include/dyncall"

WARNINGS='-Wimplicit -Wmisleading-indentation -Wparentheses -Wsequence-point -Wreturn-type -Wshift-negative-value -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wmaybe-uninitialized -Wsign-compare -Wstrict-overflow -Wduplicated-branches -Wduplicated-cond -Wtrigraphs -Waddress -Wlogical-op'

if [ "$1" = "debug" ]; then
    FLAGS="$WARNINGS -g3"
else
    FLAGS="$WARNINGS -O3"
fi

FLAGS="$FLAGS -DENABLE_DEBUG_INFO"

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
            LIBS="$(wasmer config --libdir)/libwasmer.a $LIBS"
            ;;

        # Used by MacOS ARM64 cross compilation
        wasmer-custom)
            LIBS="$WASMER_LIB_PATH/libwasmer.a $LIBS"
            ;;

        *)
            echo "Unknown WebAssembly runtime '$ONYX_RUNTIME_LIBRARY'. Aborting.";
            exit 1
            ;;
    esac
fi

case "$ONYX_ARCH" in
    *darwin*)
        LIBS="$LIBS -lffi -lSystem -framework CoreFoundation -framework SystemConfiguration"
        ;;
esac

if [ "$ONYX_USE_DYNCALL" = "1" ] && [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
    LIBS="$LIBS ../shared/lib/$ONYX_ARCH/lib/libdyncall_s.a ../shared/lib/$ONYX_ARCH/lib/libdyncallback_s.a"
    FLAGS="$FLAGS -DUSE_DYNCALL"
fi

echo "Compiling onyx"
$ONYX_CC -o "onyx" \
    $FLAGS $INCLUDES \
    $(echo "$C_FILES" | sed 's/ /\n/g;s/\([a-zA-Z_0-9]*\)\n/src\/\1.c\n/g;s/\n/ /g') \
    $LIBS
