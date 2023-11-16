#!/bin/sh

C_FILES="onyx astnodes builtins checker clone doc entities errors lex parser symres types utils wasm_emit "
LIBS="-L$ONYX_LIBRARY_DIR -lpthread -ldl -lm"
INCLUDES="-I./include -I../shared/include -I../shared/include/dyncall"

WARNINGS='-Wimplicit -Wmisleading-indentation -Wparentheses -Wsequence-point -Wreturn-type -Wshift-negative-value -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wmaybe-uninitialized -Wsign-compare -Wstrict-overflow -Wduplicated-branches -Wduplicated-cond -Wtrigraphs -Waddress -Wlogical-op'

if [ "$1" = "debug" ]; then
    FLAGS="$WARNINGS -g3"
else
    FLAGS="$WARNINGS -O3"
fi

if [ ! -z ${ONYX_RUNTIME_LIBRARY+x} ]; then
    FLAGS="$FLAGS -DENABLE_RUN_WITH_WASMER"
    C_FILES="${C_FILES}wasm_runtime "
    LIBS="-l$ONYX_RUNTIME_LIBRARY $LIBS"
fi

if [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
    FLAGS="$FLAGS -DUSE_OVM_DEBUGGER"
fi

FLAGS="$FLAGS -DENABLE_DEBUG_INFO"

if [ "$ONYX_USE_DYNCALL" = "1" ] && [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
    LIBS="$LIBS ../shared/lib/linux_$ONYX_ARCH/lib/libdyncall_s.a ../shared/lib/linux_$ONYX_ARCH/lib/libdyncallback_s.a"
    FLAGS="$FLAGS -DUSE_DYNCALL"
fi

echo "Compiling onyx"
$ONYX_CC -o "onyx" \
    $FLAGS $INCLUDES \
    $(echo "$C_FILES" | sed 's/ /\n/g;s/\([a-zA-Z_0-9]*\)\n/src\/\1.c\n/g;s/\n/ /g') \
    $LIBS
