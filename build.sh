#!/bin/sh

# Where the core libraries for Onyx will go. 
CORE_DIR='/usr/share/onyx'

# Where the onyx executable will be placed.
BIN_DIR='/usr/bin'

echo "Installing core libs"
sudo mkdir -p "$CORE_DIR"
sudo cp -r ./core/ "$CORE_DIR"

[ "$1" = "libs_only" ] && exit 0

C_FILES="onyx astnodes builtins checker clone doc entities errors lex parser symres types utils wasm_emit"
CC='gcc'
LIBS=
INCLUDES=

WARNINGS='-Wimplicit -Wmisleading-indentation -Wparentheses -Wsequence-point -Wreturn-type -Wshift-negative-value -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wmaybe-uninitialized -Wsign-compare -Wstrict-overflow -Wduplicated-branches -Wduplicated-cond -Wtrigraphs -Waddress -Wlogical-op'

if [ "$1" = "debug" ]; then
    FLAGS="$WARNINGS -g3 -I./include"
    TARGET="./bin/onyx-debug"
else
    FLAGS="$WARNINGS -O3 -I./include"
    TARGET='./bin/onyx'
fi

if [ ! -z "$ONYX_ENABLE_RUN_WITH_WASMER" ]; then
    C_FILES="$C_FILES wasm_runtime"
    FLAGS="$FLAGS -DENABLE_RUN_WITH_WASMER"
    LIBS="$(wasmer config --libs) -Wl,-rpath=$(wasmer config --libdir)"
    INCLUDES="$(wasmer config --cflags)"
fi

BUILD_DIR='./build'
mkdir -p "$BUILD_DIR"

for file in $C_FILES ; do
    echo "Compiling $file.c"
    $CC -o $BUILD_DIR/$file.o \
        $FLAGS \
        "-DCORE_INSTALLATION=\"$CORE_DIR\"" \
        -c src/$file.c \
        $INCLUDES $LIBS
done

echo "Linking $TARGET"
$CC -o $TARGET $FLAGS $(for file in $C_FILES ; do printf "$BUILD_DIR/%s.o " $file ; done) $LIBS

echo "Removing object files"
for file in $C_FILES ; do rm -f "$BUILD_DIR/$file".o 2>/dev/null ; done

echo "Installing onyx executable"
sudo cp ./bin/onyx "$BIN_DIR/onyx"

# Otherwise the prompt ends on the same line
printf "\n"
