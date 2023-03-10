#!/bin/sh

. ../settings.sh

# Temporary flag
ENABLE_DEBUG_INFO=1

C_FILES="onyx astnodes builtins checker clone doc entities errors lex parser symres types utils wasm_emit wasm_runtime "
LIBS="-L$CORE_DIR/lib -l$RUNTIME_LIBRARY -Wl,-rpath=$CORE_DIR/lib:./ -lpthread -ldl -lm -ldyncall_s"
INCLUDES="-I./include -I../shared/include -I../shared/include/dyncall"

WARNINGS='-Wimplicit -Wmisleading-indentation -Wparentheses -Wsequence-point -Wreturn-type -Wshift-negative-value -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wmaybe-uninitialized -Wsign-compare -Wstrict-overflow -Wduplicated-branches -Wduplicated-cond -Wtrigraphs -Waddress -Wlogical-op'

if [ "$1" = "debug" ]; then
    FLAGS="$WARNINGS -g3"
else
    FLAGS="$WARNINGS -O3"
fi

if [ "$RUNTIME_LIBRARY" = "ovmwasm" ]; then
    FLAGS="$FLAGS -DUSE_OVM_DEBUGGER"
fi

if [ "$ENABLE_DEBUG_INFO" = "1" ]; then
    FLAGS="$FLAGS -DENABLE_DEBUG_INFO"
fi

FLAGS="$FLAGS -DENABLE_RUN_WITH_WASMER"

sudo mkdir -p "$BIN_DIR"

echo "Compiling onyx..."
$CC -o "../bin/onyx" \
    $FLAGS \
    "-DCORE_INSTALLATION=\"$CORE_DIR\"" \
    $INCLUDES \
    $(echo "$C_FILES" | sed 's/ /\n/g;s/\([a-zA-Z_0-9]*\)\n/src\/\1.c\n/g;s/\n/ /g') \
    $LIBS

echo "Installing onyx executable"
sudo cp "../bin/onyx" "$BIN_DIR/onyx"

C_FILES="onyxrun wasm_runtime "

echo "Compiling onyx-run..."
$CC -o "../bin/onyx-run" \
    $FLAGS \
    $INCLUDES \
    $(echo "$C_FILES" | sed 's/ /\n/g;s/\([a-zA-Z_0-9]*\)\n/src\/\1.c\n/g;s/\n/ /g') \
    $LIBS

echo "Installing onyx-run executable"
sudo cp "../bin/onyx-run" "$BIN_DIR/onyx-run"
