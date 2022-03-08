#!/bin/sh

# Where the core libraries for Onyx will go. 
CORE_DIR='/usr/share/onyx'

# Where the onyx executable will be placed.
BIN_DIR='/usr/bin'

# Where the Wasmer library files can be found.
# They are bundled with the project, but if a different version is available, these can be changed.
WASMER_INCLUDE_DIR="$(pwd)/lib/common/include"
WASMER_LIBRARY_DIR="$(pwd)/lib/linux_x86_64/lib"

# Where the intermediate build files go.
BUILD_DIR='./build'

echo "Installing core libs"
sudo mkdir -p "$CORE_DIR"
sudo cp -r ./core/ "$CORE_DIR"

[ "$1" = "libs_only" ] && exit 0

if [ ! -f "$CORE_DIR/lib/libwasmer.so" ]; then
    sudo mkdir -p "$CORE_DIR/lib"

    echo "Copying libwasmer to $CORE_DIR/lib (first install)"
    # sudo cp "$WASMER_LIBRARY_DIR/libiwasm.so" "$CORE_DIR/lib/libiwasm.so"
    sudo cp "$WASMER_LIBRARY_DIR/libwasmer.so" "$CORE_DIR/lib/libwasmer.so"
    sudo cp "$WASMER_LIBRARY_DIR/libwasmer.a" "$CORE_DIR/lib/libwasmer.a"
fi

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

C_FILES="$C_FILES wasm_runtime"
FLAGS="$FLAGS -DENABLE_RUN_WITH_WASMER"
LIBS="-L$CORE_DIR/lib -lwasmer -Wl,-rpath=$CORE_DIR/lib:./ -lpthread -ldl"
INCLUDES="-I$WASMER_INCLUDE_DIR"

mkdir -p "$BUILD_DIR"

compile() {
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
}

compile
echo "Installing onyx executable"
sudo cp ./bin/onyx "$BIN_DIR/onyx"

C_FILES="onyxrun wasm_runtime"
TARGET="./bin/onyxrun"

compile
echo "Installing onyxrun executable"
sudo cp ./bin/onyxrun "$BIN_DIR/onyxrun"

./modules/onyx_runtime/build.sh
sudo mv "./onyx_runtime.so" "$CORE_DIR/lib/onyx_runtime.so"

# Otherwise the prompt ends on the same line
printf "\n"
