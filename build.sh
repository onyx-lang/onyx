#!/bin/sh

echo "Installing core libs"
CORE_DIR='/usr/share/onyx'
sudo mkdir -p "$CORE_DIR"
sudo cp -r ./core/ "$CORE_DIR"

[ "$1" = "libs_only" ] && exit 0

C_FILES="onyx astnodes builtins checker clone doc entities errors lex parser symres types utils wasm"
CC='gcc'

WARNINGS='-Wimplicit -Wmisleading-indentation -Wparentheses -Wsequence-point -Wreturn-type -Wshift-negative-value -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wmaybe-uninitialized -Wsign-compare -Wstrict-overflow -Wduplicated-branches -Wduplicated-cond -Wtrigraphs -Waddress -Wlogical-op'

if [ "$1" = "debug" ]; then
    FLAGS="$WARNINGS -g3 -I./include"
    TARGET="./bin/onyx-debug"
else
    FLAGS="$WARNINGS -O3 -I./include"
    TARGET='./bin/onyx'
fi

BUILD_DIR='./build'
mkdir -p "$BUILD_DIR"

for file in $C_FILES ; do
    echo "Compiling $file.c"
    $CC -o $BUILD_DIR/$file.o $FLAGS -c src/$file.c
done

echo "Linking $TARGET"
$CC -o $TARGET $FLAGS $(for file in $C_FILES ; do printf "$BUILD_DIR/%s.o " $file ; done)

echo "Removing object files"
for file in $C_FILES ; do rm -f "$BUILD_DIR/$file".o 2>/dev/null ; done

echo "Installing onyx executable"
BIN_DIR='/usr/bin'
sudo cp ./bin/onyx "$BIN_DIR/onyx"

# Otherwise the prompt ends on the same line
printf "\n"
