#!/bin/sh

# The base path for the installation.
# This is typcially /usr or /usr/local.
INSTALL_DIR="/usr"

# Where the core libraries for Onyx will go. 
CORE_DIR="$INSTALL_DIR/share/onyx"

# Where the onyx executable will be placed.
BIN_DIR="$INSTALL_DIR/bin"

# The compiler to use. Only GCC and TCC have been tested.
CC='gcc'

# The architecture of your system. If your not sure, leave this alone.
ARCH="$(uname -m)"

# Comment this line if you do not want the Wamser libraries installed,
# and do not with to have the Onyx runtime.
ENABLE_BUNDLING_WASMER=1

# Where the Wasmer library files can be found.
# They are bundled with the project, but if a different version is available, these can be changed.
WASMER_INCLUDE_DIR="$(pwd)/lib/common/include"
WASMER_LIBRARY_DIR="$(pwd)/lib/linux_$ARCH/lib"

# Where the intermediate build files go.
BUILD_DIR='./build'




echo "Installing core libs"
sudo mkdir -p "$CORE_DIR"
sudo cp -r ./core/ "$CORE_DIR"

# This is a development feature to allow for quickly reinstalling core libraries
# without have to recompile the entire compiler
[ "$1" = "core" ] && exit 0


C_FILES="onyx astnodes builtins checker clone doc entities errors lex parser symres types utils wasm_emit"
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


if [ ! -z "$ENABLE_BUNDLING_WASMER" ]; then
    C_FILES="$C_FILES wasm_runtime"
    FLAGS="$FLAGS -DENABLE_RUN_WITH_WASMER"
    LIBS="$LIBS -L$CORE_DIR/lib -lwasmer -Wl,-rpath=$CORE_DIR/lib:./ -lpthread -ldl"
    INCLUDES="$INCLUDES -I$WASMER_INCLUDE_DIR"

    if [ ! -f "$CORE_DIR/lib/libwasmer.so" ]; then
        sudo mkdir -p "$CORE_DIR/lib"

        echo "Copying libwasmer to $CORE_DIR/lib (first install)"
        # sudo cp "$WASMER_LIBRARY_DIR/libiwasm.so" "$CORE_DIR/lib/libiwasm.so"
        sudo cp "$WASMER_LIBRARY_DIR/libwasmer.so" "$CORE_DIR/lib/libwasmer.so"
        sudo cp "$WASMER_LIBRARY_DIR/libwasmer.a" "$CORE_DIR/lib/libwasmer.a"
    fi
fi

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
sudo mkdir -p "$BIN_DIR"
sudo cp ./bin/onyx "$BIN_DIR/onyx"

if [ ! -z "$ENABLE_BUNDLING_WASMER" ]; then
    C_FILES="onyxrun wasm_runtime"
    TARGET="./bin/onyxrun"

    compile
    echo "Installing onyxrun executable"
    sudo cp ./bin/onyxrun "$BIN_DIR/onyxrun"

    $CC -shared -fpic -I include -I lib/common/include src/onyx_runtime.c -o onyx_runtime.so -lpthread
    sudo mv "./onyx_runtime.so" "$CORE_DIR/lib/onyx_runtime.so"
fi

# Otherwise the prompt ends on the same line
printf "\n"
