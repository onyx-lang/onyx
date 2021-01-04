#!/bin/sh

C_FILES="onyx onyxbuiltins onyxchecker onyxclone onyxdoc onyxentities onyxerrors onyxlex onyxparser onyxsempass onyxsymres onyxtypes onyxutils onyxwasm"
TARGET='./bin/onyx'
CC='gcc'

if [ "$1" = "debug" ]; then
    FLAGS='-g3 -I./include'
else
    FLAGS='-O3 -I./include'
fi

BUILD_DIR='./build'
mkdir -p "$BUILD_DIR"

for file in $C_FILES ; do
    echo "Compiling $file.c"
    $CC -o build/$file.o $FLAGS -c src/$file.c
done

ALL_FILES="$(for file in $C_FILES ; do printf "$BUILD_DIR/%s.o " $file ; done)"
echo "Linking $TARGET"
$CC -o $TARGET $FLAGS $ALL_FILES

echo "Installing onyx executable"
BIN_DIR='/usr/bin'
sudo cp ./bin/onyx "$BIN_DIR/onyx"

echo "Installing core libs"
CORE_DIR='/usr/share/onyx'
sudo mkdir -p "$CORE_DIR"
sudo cp -r ./core/ "$CORE_DIR"


# Otherwise the prompt ends on the same line
printf "\n"